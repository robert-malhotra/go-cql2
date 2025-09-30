package cql2text

import (
	"errors"
	"fmt"
	"strings"

	"github.com/planetlabs/go-ogc/filter"
)

func convertPredicate(pred *Predicate) (filter.BooleanExpression, error) {
	switch {
	case pred.Comparison != nil:
		return convertComparisonPredicate(pred.Comparison)
	case pred.Like != nil:
		return convertLikePredicate(pred.Like)
	case pred.Between != nil:
		return convertBetweenPredicate(pred.Between)
	case pred.In != nil:
		return convertInPredicate(pred.In)
	case pred.IsNull != nil:
		return convertIsNullPredicate(pred.IsNull)
	case pred.Spatial != nil:
		return convertSpatialPredicate(pred.Spatial)
	case pred.Temporal != nil:
		return convertTemporalPredicate(pred.Temporal)
	case pred.Array != nil:
		return convertArrayPredicate(pred.Array)
	default:
		return nil, errors.New("internal grammar error: unknown predicate type")
	}
}

func convertComparisonPredicate(pred *ComparisonPredicate) (*filter.Comparison, error) {
	left, err := convertScalarExpression(pred.Left)
	if err != nil {
		return nil, fmt.Errorf("converting left comparison operand: %w", err)
	}

	right, err := convertScalarExpression(pred.Right)
	if err != nil {
		return nil, fmt.Errorf("converting right comparison operand: %w", err)
	}

	opMap := map[string]string{
		"=":  filter.Equals,
		"<>": filter.NotEquals,
		"<":  filter.LessThan,
		"<=": filter.LessThanOrEquals,
		">":  filter.GreaterThan,
		">=": filter.GreaterThanOrEquals,
	}
	op, ok := opMap[pred.Operator]
	if !ok {
		return nil, fmt.Errorf("internal grammar error: unknown comparison operator: %s", pred.Operator)
	}

	return &filter.Comparison{
		Name:  op,
		Left:  left,
		Right: right,
	}, nil
}

func convertLikePredicate(pred *LikePredicate) (filter.BooleanExpression, error) {
	left, err := convertScalarExpression(pred.Left)
	if err != nil {
		return nil, fmt.Errorf("converting LIKE left operand: %w", err)
	}

	pattern, err := convertScalarExpression(pred.Pattern)
	if err != nil {
		return nil, fmt.Errorf("converting LIKE pattern operand: %w", err)
	}

	return &filter.Function{
		Op:   "like",
		Args: []filter.Expression{left, pattern},
	}, nil
}

func convertBetweenPredicate(pred *BetweenPredicate) (*filter.Between, error) {
	valueExpr, err := convertScalarExpression(pred.Value)
	if err != nil {
		return nil, fmt.Errorf("converting BETWEEN value: %w", err)
	}
	value, err := ensureNumericExpression(valueExpr, "BETWEEN value")
	if err != nil {
		return nil, err
	}

	lowExpr, err := convertScalarExpression(pred.Low)
	if err != nil {
		return nil, fmt.Errorf("converting BETWEEN low bound: %w", err)
	}
	low, err := ensureNumericExpression(lowExpr, "BETWEEN low bound")
	if err != nil {
		return nil, err
	}

	highExpr, err := convertScalarExpression(pred.High)
	if err != nil {
		return nil, fmt.Errorf("converting BETWEEN high bound: %w", err)
	}
	high, err := ensureNumericExpression(highExpr, "BETWEEN high bound")
	if err != nil {
		return nil, err
	}

	return &filter.Between{
		Value: value,
		Low:   low,
		High:  high,
	}, nil
}

func convertInPredicate(pred *InPredicate) (*filter.In, error) {
	item, err := convertScalarExpression(pred.Value)
	if err != nil {
		return nil, fmt.Errorf("converting IN value: %w", err)
	}

	list := make([]filter.ScalarExpression, len(pred.List))
	for i, candidate := range pred.List {
		converted, err := convertScalarExpression(candidate)
		if err != nil {
			return nil, fmt.Errorf("converting IN list item %d: %w", i, err)
		}
		list[i] = converted
	}

	return &filter.In{
		Item: item,
		List: filter.ScalarList(list),
	}, nil
}

func convertIsNullPredicate(pred *IsNullPredicate) (*filter.IsNull, error) {
	value, err := convertScalarExpression(pred.Value)
	if err != nil {
		return nil, fmt.Errorf("converting IS NULL value: %w", err)
	}

	return &filter.IsNull{Value: value}, nil
}

func convertSpatialPredicate(pred *SpatialPredicate) (filter.BooleanExpression, error) {
	leftExpr, err := convertSpatialOperand(pred.Left)
	if err != nil {
		return nil, fmt.Errorf("converting left spatial operand for %s: %w", pred.Op, err)
	}
	rightExpr, err := convertSpatialOperand(pred.Right)
	if err != nil {
		return nil, fmt.Errorf("converting right spatial operand for %s: %w", pred.Op, err)
	}

	opName := strings.ToLower(pred.Op)

	return &filter.Function{
		Op:   opName,
		Args: []filter.Expression{leftExpr, rightExpr},
	}, nil
}

func convertTemporalPredicate(pred *TemporalPredicate) (*filter.TemporalComparison, error) {
	left, err := convertTemporalOperand(pred.Left)
	if err != nil {
		return nil, fmt.Errorf("converting left temporal operand for %s: %w", pred.Op, err)
	}
	right, err := convertTemporalOperand(pred.Right)
	if err != nil {
		return nil, fmt.Errorf("converting right temporal operand for %s: %w", pred.Op, err)
	}

	opMap := map[string]string{
		"T_AFTER":        filter.TimeAfter,
		"T_BEFORE":       filter.TimeBefore,
		"T_CONTAINS":     filter.TimeContains,
		"T_DISJOINT":     filter.TimeDisjoint,
		"T_DURING":       filter.TimeDuring,
		"T_EQUALS":       filter.TimeEquals,
		"T_FINISHEDBY":   filter.TimeFinishedBy,
		"T_FINISHES":     filter.TimeFinishes,
		"T_INTERSECTS":   filter.TimeIntersects,
		"T_MEETS":        filter.TimeMeets,
		"T_METBY":        filter.TimeMetBy,
		"T_OVERLAPPEDBY": filter.TimeOverlappedBy,
		"T_OVERLAPS":     filter.TimeOverlaps,
		"T_STARTEDBY":    filter.TimeStartedBy,
		"T_STARTS":       filter.TimeStarts,
	}
	op, ok := opMap[strings.ToUpper(pred.Op)]
	if !ok {
		return nil, fmt.Errorf("internal grammar error: unknown temporal operator: %s", pred.Op)
	}

	return &filter.TemporalComparison{
		Name:  op,
		Left:  left,
		Right: right,
	}, nil
}

func convertArrayPredicate(pred *ArrayPredicate) (*filter.ArrayComparison, error) {
	left, err := convertArrayOperand(pred.Left)
	if err != nil {
		return nil, fmt.Errorf("converting left array operand for %s: %w", pred.Op, err)
	}
	right, err := convertArrayOperand(pred.Right)
	if err != nil {
		return nil, fmt.Errorf("converting right array operand for %s: %w", pred.Op, err)
	}

	opMap := map[string]string{
		"A_CONTAINEDBY": filter.ArrayContainedBy,
		"A_CONTAINS":    filter.ArrayContains,
		"A_EQUALS":      filter.ArrayEquals,
		"A_OVERLAPS":    filter.ArrayOverlaps,
	}
	op, ok := opMap[strings.ToUpper(pred.Op)]
	if !ok {
		return nil, fmt.Errorf("internal grammar error: unknown array operator: %s", pred.Op)
	}

	return &filter.ArrayComparison{
		Name:  op,
		Left:  left,
		Right: right,
	}, nil
}
