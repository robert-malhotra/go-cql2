package cql2text

import (
	"errors"

	"github.com/planetlabs/go-ogc/filter"
)

func convertExpression(expr *Expression) (filter.BooleanExpression, error) {
	return convertBooleanExpression(expr.BoolExpr)
}

func convertBooleanExpression(expr *BooleanExpression) (filter.BooleanExpression, error) {
	left, err := convertBooleanTerm(expr.Left)
	if err != nil {
		return nil, err
	}

	if len(expr.Right) == 0 {
		return left, nil
	}

	args := make([]filter.BooleanExpression, len(expr.Right)+1)
	args[0] = left

	for i, op := range expr.Right {
		right, err := convertBooleanTerm(op.Term)
		if err != nil {
			return nil, err
		}
		args[i+1] = right
	}

	return &filter.Or{Args: args}, nil
}

func convertBooleanTerm(term *BooleanTerm) (filter.BooleanExpression, error) {
	left, err := convertBooleanFactor(term.Left)
	if err != nil {
		return nil, err
	}

	if len(term.Right) == 0 {
		return left, nil
	}

	args := make([]filter.BooleanExpression, len(term.Right)+1)
	args[0] = left

	for i, op := range term.Right {
		right, err := convertBooleanFactor(op.Factor)
		if err != nil {
			return nil, err
		}
		args[i+1] = right
	}

	return &filter.And{Args: args}, nil
}

func convertBooleanFactor(factor *BooleanFactor) (filter.BooleanExpression, error) {
	expr, err := convertBooleanGroup(factor.Group)
	if err != nil {
		return nil, err
	}

	if factor.Not {
		return &filter.Not{Arg: expr}, nil
	}

	return expr, nil
}

func convertBooleanGroup(group *BooleanGroup) (filter.BooleanExpression, error) {
	if group.SubExpr != nil {
		return convertBooleanExpression(group.SubExpr)
	}

	if group.Pred != nil {
		return convertPredicate(group.Pred)
	}

	return nil, errors.New("internal grammar error: empty boolean group")
}
