package cql2text

// Expression is the root node of the CQL2 grammar
type Expression struct {
	BoolExpr *BooleanExpression `@@`
}

// BooleanExpression represents logical expressions (OR)
type BooleanExpression struct {
	Left  *BooleanTerm        `@@`
	Right []*BooleanOperation `@@*`
}

// BooleanOperation represents OR operations
type BooleanOperation struct {
	Or   bool         `@"OR"`
	Term *BooleanTerm `@@`
}

// BooleanTerm represents terms in boolean expressions (AND)
type BooleanTerm struct {
	Left  *BooleanFactor       `@@`
	Right []*BooleanTermFactor `@@*`
}

// BooleanTermFactor represents AND operations
type BooleanTermFactor struct {
	And    bool           `@"AND"`
	Factor *BooleanFactor `@@`
}

// BooleanFactor represents NOT or nested expressions
type BooleanFactor struct {
	Not   bool          `@"NOT"?`
	Group *BooleanGroup `@@`
}

// BooleanGroup represents a predicate or parenthesized expression
type BooleanGroup struct {
	SubExpr *BooleanExpression `"(" @@ ")"`
	Pred    *Predicate         `| @@`
}
