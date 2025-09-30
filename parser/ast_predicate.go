package cql2text

// Predicate represents any comparison or operation that returns boolean
type Predicate struct {
	Comparison *ComparisonPredicate `@@`
	Like       *LikePredicate       `| @@`
	Between    *BetweenPredicate    `| @@`
	In         *InPredicate         `| @@`
	IsNull     *IsNullPredicate     `| @@`
	Spatial    *SpatialPredicate    `| @@`
	Temporal   *TemporalPredicate   `| @@`
	Array      *ArrayPredicate      `| @@`
}

// ComparisonPredicate represents binary comparison operations
type ComparisonPredicate struct {
	Left     *ScalarExpression `@@`
	Operator string            `@( "=" | "<>" | "<" | "<=" | ">" | ">=" )`
	Right    *ScalarExpression `@@`
}

// LikePredicate represents LIKE operations
type LikePredicate struct {
	Left    *ScalarExpression `@@`
	Like    string            `@"LIKE"`
	Pattern *ScalarExpression `@@`
}

// BetweenPredicate represents BETWEEN operations
type BetweenPredicate struct {
	Value   *ScalarExpression `@@`
	Between string            `@"BETWEEN"`
	Low     *ScalarExpression `@@`
	And     string            `@"AND"`
	High    *ScalarExpression `@@`
}

// InPredicate represents IN operations
type InPredicate struct {
	Value *ScalarExpression   `@@`
	In    string              `@"IN"`
	List  []*ScalarExpression `"(" @@ ( "," @@ )* ")"`
}

// IsNullPredicate represents IS NULL operations
type IsNullPredicate struct {
	Value *ScalarExpression `@@`
	Is    string            `@"IS"`
	Null  string            `@"NULL"`
}

// SpatialPredicate represents spatial operations (S_*)
type SpatialPredicate struct {
	Op    string          `@( "S_CONTAINS" | "S_CROSSES" | "S_DISJOINT" | "S_EQUALS" | "S_INTERSECTS" | "S_OVERLAPS" | "S_TOUCHES" | "S_WITHIN" )`
	Left  *SpatialOperand `"(" @@ ","`
	Right *SpatialOperand `@@ ")"`
}

// TemporalPredicate represents temporal operations (T_*)
type TemporalPredicate struct {
	Op    string           `@( "T_AFTER" | "T_BEFORE" | "T_CONTAINS" | "T_DISJOINT" | "T_DURING" | "T_EQUALS" | "T_FINISHEDBY" | "T_FINISHES" | "T_INTERSECTS" | "T_MEETS" | "T_METBY" | "T_OVERLAPPEDBY" | "T_OVERLAPS" | "T_STARTEDBY" | "T_STARTS" )`
	Left  *TemporalOperand `"(" @@ ","`
	Right *TemporalOperand `@@ ")"`
}

// ArrayPredicate represents array operations (A_*)
type ArrayPredicate struct {
	Op    string        `@( "A_CONTAINEDBY" | "A_CONTAINS" | "A_EQUALS" | "A_OVERLAPS" )`
	Left  *ArrayOperand `"(" @@ ","`
	Right *ArrayOperand `@@ ")"`
}

// SpatialOperand represents geometry expressions (property or function)
type SpatialOperand struct {
	Function *FunctionExpression `@@`
	Property *PropertyName       `| @@`
}

// TemporalOperand represents temporal expressions (property, timestamp literal, or interval)
type TemporalOperand struct {
	Property  *PropertyName  `@@`
	Timestamp *StringLiteral `| @@`
	Interval  *Interval      `| @@`
}

// Interval represents a time interval literal like ['2020-01-01', '..']
type Interval struct {
	Start *StringLiteral `"[" @@`
	End   *StringLiteral `"," @@ "]"`
}

// ArrayOperand represents array expressions (property or literal array)
type ArrayOperand struct {
	Property *PropertyName       `@@`
	Array    []*ScalarExpression `| "[" ( @@ ( "," @@ )* )? "]"`
}
