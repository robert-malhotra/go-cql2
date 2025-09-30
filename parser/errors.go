package cql2text

import (
	"errors"
	"fmt"

	"github.com/alecthomas/participle/v2"
)

var errEmptyExpression = errors.New("cql parse error: empty expression")

func emptyExpressionError() error {
	return errEmptyExpression
}

func wrapParseError(err error) error {
	var perr participle.Error
	if errors.As(err, &perr) {
		return fmt.Errorf("cql parse error at %s: %w", perr.Position(), err)
	}
	return fmt.Errorf("cql parse error: %w", err)
}

func wrapConversionError(err error) error {
	return fmt.Errorf("cql conversion error: %w", err)
}
