package cql2text

import (
	"errors"
	"fmt"
	"strings"
	"time"

	"github.com/planetlabs/go-ogc/filter"
)

func convertScalarExpression(expr *ScalarExpression) (filter.ScalarExpression, error) {
	switch {
	case expr.Function != nil:
		return convertFunctionExpression(expr.Function)
	case expr.CaseIFunc != nil:
		value, err := convertScalarExpression(expr.CaseIFunc.Value)
		if err != nil {
			return nil, fmt.Errorf("converting CASEI argument: %w", err)
		}
		return &filter.Function{Op: "casei", Args: []filter.Expression{value}}, nil
	case expr.AccentIFunc != nil:
		value, err := convertScalarExpression(expr.AccentIFunc.Value)
		if err != nil {
			return nil, fmt.Errorf("converting ACCENTI argument: %w", err)
		}
		return &filter.Function{Op: "accenti", Args: []filter.Expression{value}}, nil
	case expr.Boolean != nil:
		return &filter.Boolean{Value: expr.Boolean.Value}, nil
	case expr.Property != nil:
		return &filter.Property{Name: expr.Property.Name}, nil
	case expr.Literal != nil:
		return convertLiteralValue(expr.Literal)
	default:
		return nil, errors.New("internal grammar error: unsupported scalar expression")
	}
}

func convertFunctionExpression(expr *FunctionExpression) (*filter.Function, error) {
	args := make([]filter.Expression, len(expr.Args))
	for i, arg := range expr.Args {
		converted, err := convertScalarExpression(arg)
		if err != nil {
			return nil, fmt.Errorf("converting argument %d for function %q: %w", i, expr.Name, err)
		}
		args[i] = converted
	}

	funcName := expr.Name
	knownLowercaseFuncs := map[string]bool{
		"CASEI":   true,
		"ACCENTI": true,
	}
	if knownLowercaseFuncs[strings.ToUpper(funcName)] {
		funcName = strings.ToLower(funcName)
	}

	return &filter.Function{Op: funcName, Args: args}, nil
}

func convertLiteralValue(value *LiteralValue) (filter.ScalarExpression, error) {
	if value.String != nil {
		return &filter.String{Value: *value.String}, nil
	}
	if value.Number != nil {
		return &filter.Number{Value: *value.Number}, nil
	}

	return nil, errors.New("internal grammar error: unsupported literal value")
}

func convertSpatialOperand(operand *SpatialOperand) (filter.Expression, error) {
	if operand.Property != nil {
		return &filter.Property{Name: operand.Property.Name}, nil
	}
	if operand.Function != nil {
		return convertFunctionExpression(operand.Function)
	}

	return nil, errors.New("internal grammar error: unsupported spatial operand type")
}

func convertTemporalOperand(operand *TemporalOperand) (filter.TemporalExpression, error) {
	switch {
	case operand.Property != nil:
		return &filter.Property{Name: operand.Property.Name}, nil
	case operand.Timestamp != nil:
		instant, err := parseInstant(operand.Timestamp.Value, "temporal operand")
		if err != nil {
			return nil, err
		}
		if instant == nil {
			return nil, errors.New("internal error: unexpected nil instant for temporal operand")
		}
		return instant, nil
	case operand.Interval != nil:
		return convertInterval(operand.Interval)
	default:
		return nil, errors.New("internal grammar error: unsupported temporal operand type")
	}
}

func convertInterval(interval *Interval) (*filter.Interval, error) {
	start, err := parseInstant(interval.Start.Value, "interval start")
	if err != nil {
		return nil, err
	}

	end, err := parseInstant(interval.End.Value, "interval end")
	if err != nil {
		return nil, err
	}

	if start == nil && end == nil {
		return nil, errors.New("invalid interval literal: both start and end are unbounded ('..')")
	}

	return &filter.Interval{Start: start, End: end}, nil
}

func parseInstant(unquotedValue, context string) (filter.InstantExpression, error) {
	if unquotedValue == ".." {
		return nil, nil
	}

	formats := []string{
		time.RFC3339Nano,
		time.RFC3339,
		time.DateOnly,
		"2006-01-02T15:04:05",
	}

	var parsedTime time.Time
	var parseErr error

	for _, format := range formats {
		if t, err := time.Parse(format, unquotedValue); err == nil {
			parsedTime = t
			if format == time.DateOnly {
				return &filter.Date{Value: parsedTime}, nil
			}
			return &filter.Timestamp{Value: parsedTime}, nil
		} else {
			parseErr = err
		}
	}

	return nil, fmt.Errorf("invalid date or timestamp format for %s: %q (last error: %w)", context, unquotedValue, parseErr)
}

func convertArrayOperand(operand *ArrayOperand) (filter.ArrayExpression, error) {
	switch {
	case operand.Property != nil:
		return &filter.Property{Name: operand.Property.Name}, nil
	case operand.Array != nil:
		items := make([]filter.ArrayItemExpression, len(operand.Array))
		for i, item := range operand.Array {
			converted, err := convertScalarExpression(item)
			if err != nil {
				return nil, fmt.Errorf("converting array literal item %d: %w", i, err)
			}
			arrayItem, ok := converted.(filter.ArrayItemExpression)
			if !ok {
				return nil, fmt.Errorf("array literal item %d (type %T) is not a valid array item expression", i, converted)
			}
			items[i] = arrayItem
		}
		return filter.Array(items), nil
	default:
		return nil, errors.New("internal grammar error: unsupported array operand type")
	}
}

func ensureNumericExpression(expr filter.ScalarExpression, context string) (filter.NumericExpression, error) {
	numeric, ok := expr.(filter.NumericExpression)
	if !ok {
		return nil, fmt.Errorf("%s must be numeric expression, got %T", context, expr)
	}
	return numeric, nil
}
