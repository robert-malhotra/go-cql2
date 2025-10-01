package cql2text

import (
	"strings"

	"github.com/alecthomas/participle/v2"
	"github.com/planetlabs/go-ogc/filter"
)

var parser = newParser()

// Parse parses a CQL2 text expression and returns a filter.Filter AST.
func Parse(cqlText string) (*filter.Filter, error) {
	if strings.TrimSpace(cqlText) == "" {
		return nil, emptyExpressionError()
	}

	expr, err := parser.ParseString("", cqlText)
	if err != nil {
		return nil, wrapParseError(err)
	}

	filterExpr, err := convertExpression(expr)
	if err != nil {
		return nil, wrapConversionError(err)
	}

	return &filter.Filter{Expression: filterExpr}, nil
}

func newParser() *participle.Parser[Expression] {
	return participle.MustBuild[Expression](
		participle.Lexer(cqlDef),
		participle.Elide("whitespace"),
		participle.Unquote("String"),
		participle.CaseInsensitive("Ident",
			"LIKE", "IN", "BETWEEN", "AND", "OR", "NOT", "IS", "NULL", "TRUE", "FALSE",
			"CASEI", "ACCENTI",
			"S_CONTAINS", "S_CROSSES", "S_DISJOINT", "S_EQUALS", "S_INTERSECTS", "S_OVERLAPS", "S_TOUCHES", "S_WITHIN",
			"T_AFTER", "T_BEFORE", "T_CONTAINS", "T_DISJOINT", "T_DURING", "T_EQUALS", "T_FINISHEDBY", "T_FINISHES", "T_INTERSECTS", "T_MEETS", "T_METBY", "T_OVERLAPPEDBY", "T_OVERLAPS", "T_STARTEDBY", "T_STARTS",
			"A_CONTAINEDBY", "A_CONTAINS", "A_EQUALS", "A_OVERLAPS"),
		participle.UseLookahead(20),
	)
}
