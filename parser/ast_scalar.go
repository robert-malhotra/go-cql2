package text

// ScalarExpression represents any value expression
type ScalarExpression struct {
	Function    *FunctionExpression    `@@`
	CaseIFunc   *CaseInsensitiveFunc   `| @@`
	AccentIFunc *AccentInsensitiveFunc `| @@`
	Boolean     *BooleanLiteral        `| @@`
	Property    *PropertyName          `| @@`
	Literal     *LiteralValue          `| @@`
}

// AccentInsensitiveFunc represents ACCENTI(...)
type AccentInsensitiveFunc struct {
	Name  string            `@"ACCENTI"`
	Value *ScalarExpression `"(" @@ ")"`
}

// CaseInsensitiveFunc represents CASEI(...)
type CaseInsensitiveFunc struct {
	Name  string            `@"CASEI"`
	Value *ScalarExpression `"(" @@ ")"`
}

// BooleanLiteral is a dedicated type for boolean literals
type BooleanLiteral struct {
	Value bool `@("TRUE" | "FALSE")`
}

// FunctionExpression represents a function call
type FunctionExpression struct {
	Name string              `@Ident`
	Args []*ScalarExpression `"(" ( @@ ( "," @@ )* )? ")"`
}

// LiteralValue represents literals like numbers and strings (not booleans)
type LiteralValue struct {
	String *string  `@String`
	Number *float64 `| @( Float | Int )`
}

// StringLiteral represents a string enclosed in quotes
type StringLiteral struct {
	Value string `@String`
}

// PropertyName represents a property reference
type PropertyName struct {
	Name string `@Ident`
}
