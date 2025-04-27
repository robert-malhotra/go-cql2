package text

import (
	"errors"
	"fmt"
	"strings"
	"time"

	"github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
	"github.com/planetlabs/go-ogc/filter"
)

// Package text provides a parser for CQL2 text format
// that converts to the Go-OGC filter AST.

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
	Or   bool         `@"OR"` // Case-insensitive handled by parser option
	Term *BooleanTerm `@@`
}

// BooleanTerm represents terms in boolean expressions (AND)
type BooleanTerm struct {
	Left  *BooleanFactor       `@@`
	Right []*BooleanTermFactor `@@*`
}

// BooleanTermFactor represents AND operations
type BooleanTermFactor struct {
	And    bool           `@"AND"` // Case-insensitive handled by parser option
	Factor *BooleanFactor `@@`
}

// BooleanFactor represents NOT or nested expressions
type BooleanFactor struct {
	Not   bool          `@"NOT"?` // Case-insensitive handled by parser option
	Group *BooleanGroup `@@`
}

// BooleanGroup represents a predicate or parenthesized expression
type BooleanGroup struct {
	SubExpr *BooleanExpression `"(" @@ ")"`
	Pred    *Predicate         `| @@`
}

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

// ScalarExpression represents any value expression
type ScalarExpression struct {
	Function    *FunctionExpression    `@@`
	CaseIFunc   *CaseInsensitiveFunc   `| @@`
	AccentIFunc *AccentInsensitiveFunc `| @@` // Added ACCENTI
	Boolean     *BooleanLiteral        `| @@`
	Property    *PropertyName          `| @@`
	Literal     *LiteralValue          `| @@`
}

// AccentInsensitiveFunc represents ACCENTI(...)
type AccentInsensitiveFunc struct {
	Name  string            `@"ACCENTI"` // Case-insensitive handled by parser option
	Value *ScalarExpression `"(" @@ ")"`
}

// CaseInsensitiveFunc represents CASEI(...)
type CaseInsensitiveFunc struct {
	Name  string            `@"CASEI"` // Case-insensitive handled by parser option
	Value *ScalarExpression `"(" @@ ")"`
}

// BooleanLiteral is a dedicated type for boolean literals
type BooleanLiteral struct {
	Value bool `@("TRUE" | "FALSE")` // Case-insensitive handled by parser option
}

// FunctionExpression represents a function call
type FunctionExpression struct {
	Name string              `@Ident`
	Args []*ScalarExpression `"(" ( @@ ( "," @@ )* )? ")"`
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
	Like    string            `@"LIKE"` // Case-insensitive handled by parser option
	Pattern *ScalarExpression `@@`      // Can be string, property, or function (like CASEI)
}

// BetweenPredicate represents BETWEEN operations
type BetweenPredicate struct {
	Value   *ScalarExpression `@@`
	Between string            `@"BETWEEN"` // Case-insensitive handled by parser option
	Low     *ScalarExpression `@@`
	And     string            `@"AND"` // Case-insensitive handled by parser option
	High    *ScalarExpression `@@`
}

// InPredicate represents IN operations
type InPredicate struct {
	Value *ScalarExpression   `@@`
	In    string              `@"IN"` // Case-insensitive handled by parser option
	List  []*ScalarExpression `"(" @@ ( "," @@ )* ")"`
}

// IsNullPredicate represents IS NULL operations
type IsNullPredicate struct {
	Value *ScalarExpression `@@`
	Is    string            `@"IS"`   // Case-insensitive handled by parser option
	Null  string            `@"NULL"` // Case-insensitive handled by parser option
}

// SpatialPredicate represents spatial operations (S_*)
type SpatialPredicate struct {
	// Case-insensitive handled by parser option
	Op    string          `@( "S_CONTAINS" | "S_CROSSES" | "S_DISJOINT" | "S_EQUALS" | "S_INTERSECTS" | "S_OVERLAPS" | "S_TOUCHES" | "S_WITHIN" )`
	Left  *SpatialOperand `"(" @@ ","`
	Right *SpatialOperand `@@ ")"`
}

// TemporalPredicate represents temporal operations (T_*)
type TemporalPredicate struct {
	// Case-insensitive handled by parser option
	Op    string           `@( "T_AFTER" | "T_BEFORE" | "T_CONTAINS" | "T_DISJOINT" | "T_DURING" | "T_EQUALS" | "T_FINISHEDBY" | "T_FINISHES" | "T_INTERSECTS" | "T_MEETS" | "T_METBY" | "T_OVERLAPPEDBY" | "T_OVERLAPS" | "T_STARTEDBY" | "T_STARTS" )`
	Left  *TemporalOperand `"(" @@ ","`
	Right *TemporalOperand `@@ ")"`
}

// ArrayPredicate represents array operations (A_*)
type ArrayPredicate struct {
	// Case-insensitive handled by parser option
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
	Timestamp *StringLiteral `| @@` // Date or Timestamp string
	Interval  *Interval      `| @@`
}

// Interval represents a time interval literal like ['2020-01-01', '..']
type Interval struct {
	Start *StringLiteral `"[" @@`     // Includes ".."
	End   *StringLiteral `"," @@ "]"` // Includes ".."
}

// ArrayOperand represents array expressions (property or literal array)
type ArrayOperand struct {
	Property *PropertyName       `@@`
	Array    []*ScalarExpression `| "[" ( @@ ( "," @@ )* )? "]"` // Allow empty array
}

// LiteralValue represents literals like numbers and strings (not booleans)
type LiteralValue struct {
	String *string  `@String` // Use basic type, unquoted by parser
	Number *float64 `| @( Float | Int )`
}

// StringLiteral represents a string enclosed in quotes (used where unquoting isn't desired directly, e.g., temporal)
// Kept separate for clarity in the grammar, although parser unquotes it.
type StringLiteral struct {
	Value string `@String` // Value is unquoted by the parser
}

// PropertyName represents a property reference
type PropertyName struct {
	Name string `@Ident`
}

// Define a lexer for CQL2
var cqlDef = lexer.MustStateful(lexer.Rules{
	"Root": {
		{Name: "whitespace", Pattern: `\s+`, Action: nil},
		// Order matters: Float before Int
		{Name: "Float", Pattern: `[-+]?\d+\.\d+([eE][-+]?\d+)?|[-+]?\d*\.\d+([eE][-+]?\d+)?|[-+]?\d+[eE][-+]?\d+`, Action: nil},
		{Name: "Int", Pattern: `[-+]?\d+`, Action: nil},
		// Handles single and double quotes, and basic escape sequences like \' or \"
		{Name: "String", Pattern: `'[^']*'|"[^"]*"`, Action: nil},  // Simpler pattern if lexer handles escapes
		{Name: "Punct", Pattern: `[(),\[\]]`, Action: nil},         // Removed . and {} as they aren't in the grammar? Check CQL spec if needed.
		{Name: "Operator", Pattern: `<>|<=|>=|[=<>]`, Action: nil}, // Combined simple operators
		// Identifiers: Start with letter or _, followed by letters, numbers, or _
		// Ensure keywords like AND, OR, LIKE etc. are handled correctly via CaseInsensitive parser option
		{Name: "Ident", Pattern: `[a-zA-Z_][a-zA-Z0-9_]*`, Action: nil},
	},
})

// Parser instance for CQL2
var parser = participle.MustBuild[Expression](
	participle.Lexer(cqlDef),
	participle.Elide("whitespace"),
	// Handle unquoting of string literals automatically
	participle.Unquote("String"),
	// Make keywords and specific function names case insensitive
	participle.CaseInsensitive("Ident", // Apply to Ident only if needed, keywords below are specific
		// Keywords
		"LIKE", "IN", "BETWEEN", "AND", "OR", "NOT", "IS", "NULL", "TRUE", "FALSE",
		// Functions treated as keywords in some contexts
		"CASEI", "ACCENTI",
		// Spatial Predicates
		"S_CONTAINS", "S_CROSSES", "S_DISJOINT", "S_EQUALS", "S_INTERSECTS", "S_OVERLAPS", "S_TOUCHES", "S_WITHIN",
		// Temporal Predicates
		"T_AFTER", "T_BEFORE", "T_CONTAINS", "T_DISJOINT", "T_DURING", "T_EQUALS", "T_FINISHEDBY", "T_FINISHES", "T_INTERSECTS", "T_MEETS", "T_METBY", "T_OVERLAPPEDBY", "T_OVERLAPS", "T_STARTEDBY", "T_STARTS",
		// Array Predicates
		"A_CONTAINEDBY", "A_CONTAINS", "A_EQUALS", "A_OVERLAPS"),
	participle.UseLookahead(20), // Keep lookahead as tweaking can be complex and requires deep analysis
)

// Parse parses a CQL2 text expression and returns a filter.Filter AST.
func Parse(cqlText string) (*filter.Filter, error) {
	// Check for empty input explicitly for a clearer error message
	if strings.TrimSpace(cqlText) == "" {
		return nil, errors.New("cql parse error: empty expression")
	}

	expr, err := parser.ParseString("", cqlText)
	if err != nil {
		// Try to provide more context if it's a participle error
		var perr participle.Error
		if errors.As(err, &perr) {
			return nil, fmt.Errorf("cql parse error at %s: %w", perr.Position(), err)
		}
		return nil, fmt.Errorf("cql parse error: %w", err)
	}

	// If parsing succeeds, expr and expr.BoolExpr should be non-nil based on grammar `@@`
	filterExpr, err := convertExpression(expr)
	if err != nil {
		// Wrap conversion errors for context
		return nil, fmt.Errorf("cql conversion error: %w", err)
	}

	return &filter.Filter{
		Expression: filterExpr,
	}, nil
}

// convertExpression converts the parsed Expression to filter.BooleanExpression
func convertExpression(expr *Expression) (filter.BooleanExpression, error) {
	// Assumes expr and expr.BoolExpr are non-nil due to `@@` in grammar after successful parse
	return convertBooleanExpression(expr.BoolExpr)
}

// convertBooleanExpression converts BooleanExpression (OR chain) to filter.BooleanExpression
func convertBooleanExpression(expr *BooleanExpression) (filter.BooleanExpression, error) {
	// Assumes expr.Left is non-nil
	left, err := convertBooleanTerm(expr.Left)
	if err != nil {
		return nil, err
	}

	if len(expr.Right) == 0 {
		return left, nil // Single term, no OR
	}

	// Handle OR operations
	args := make([]filter.BooleanExpression, len(expr.Right)+1)
	args[0] = left

	for i, op := range expr.Right {
		// Assumes op.Term is non-nil
		right, err := convertBooleanTerm(op.Term)
		if err != nil {
			return nil, err // Error converting right side of OR
		}
		args[i+1] = right
	}

	return &filter.Or{Args: args}, nil
}

// convertBooleanTerm converts BooleanTerm (AND chain) to filter.BooleanExpression
func convertBooleanTerm(term *BooleanTerm) (filter.BooleanExpression, error) {
	// Assumes term.Left is non-nil
	left, err := convertBooleanFactor(term.Left)
	if err != nil {
		return nil, err
	}

	if len(term.Right) == 0 {
		return left, nil // Single factor, no AND
	}

	// Handle AND operations
	args := make([]filter.BooleanExpression, len(term.Right)+1)
	args[0] = left

	for i, op := range term.Right {
		// Assumes op.Factor is non-nil
		right, err := convertBooleanFactor(op.Factor)
		if err != nil {
			return nil, err // Error converting right side of AND
		}
		args[i+1] = right
	}

	return &filter.And{Args: args}, nil
}

// convertBooleanFactor converts BooleanFactor (NOT or Group) to filter.BooleanExpression
func convertBooleanFactor(factor *BooleanFactor) (filter.BooleanExpression, error) {
	// Assumes factor.Group is non-nil
	expr, err := convertBooleanGroup(factor.Group)
	if err != nil {
		return nil, err
	}

	if factor.Not {
		return &filter.Not{Arg: expr}, nil
	}

	return expr, nil
}

// convertBooleanGroup converts BooleanGroup (Parenthesized Expr or Predicate) to filter.BooleanExpression
func convertBooleanGroup(group *BooleanGroup) (filter.BooleanExpression, error) {
	if group.SubExpr != nil {
		// Assumes group.SubExpr is non-nil if this branch is taken
		return convertBooleanExpression(group.SubExpr)
	}

	if group.Pred != nil {
		// Assumes group.Pred is non-nil if this branch is taken
		return convertPredicate(group.Pred)
	}

	// Should be unreachable if grammar is correct and parse succeeded
	return nil, errors.New("internal grammar error: empty boolean group")
}

// convertPredicate converts Predicate node to the appropriate filter.BooleanExpression
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
		// Spatial predicates are represented as functions in the filter AST for flexibility
		return convertSpatialPredicate(pred.Spatial)
	case pred.Temporal != nil:
		return convertTemporalPredicate(pred.Temporal)
	case pred.Array != nil:
		return convertArrayPredicate(pred.Array)
	default:
		// Should be unreachable
		return nil, errors.New("internal grammar error: unknown predicate type")
	}
}

// convertComparisonPredicate converts ComparisonPredicate to filter.Comparison
func convertComparisonPredicate(pred *ComparisonPredicate) (*filter.Comparison, error) {
	// Operands guaranteed non-nil by `@@`
	left, err := convertScalarExpression(pred.Left)
	if err != nil {
		return nil, fmt.Errorf("converting left comparison operand: %w", err)
	}

	right, err := convertScalarExpression(pred.Right)
	if err != nil {
		return nil, fmt.Errorf("converting right comparison operand: %w", err)
	}

	// Map operator names directly (case handled by lexer/parser)
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
		// Should be unreachable due to grammar constraints
		return nil, fmt.Errorf("internal grammar error: unknown comparison operator: %s", pred.Operator)
	}

	return &filter.Comparison{
		Name:  op,
		Left:  left,
		Right: right,
	}, nil
}

// convertLikePredicate converts LikePredicate to filter.Function (op: "like").
// This matches the structure expected by tests, even for simple string patterns.
func convertLikePredicate(pred *LikePredicate) (filter.BooleanExpression, error) {
	// Operands guaranteed non-nil by `@@`
	left, err := convertScalarExpression(pred.Left)
	if err != nil {
		return nil, fmt.Errorf("converting LIKE left operand: %w", err)
	}

	pattern, err := convertScalarExpression(pred.Pattern)
	if err != nil {
		return nil, fmt.Errorf("converting LIKE pattern operand: %w", err)
	}

	// Consistently represent LIKE as a function call in the AST
	return &filter.Function{
		Op:   "like", // Use lowercase 'like' for the function representation
		Args: []filter.Expression{left, pattern},
	}, nil
}

// convertBetweenPredicate converts BetweenPredicate to filter.Between
func convertBetweenPredicate(pred *BetweenPredicate) (*filter.Between, error) {
	// Operands guaranteed non-nil by `@@`
	value, err := convertScalarExpression(pred.Value)
	if err != nil {
		return nil, fmt.Errorf("converting BETWEEN value: %w", err)
	}
	low, err := convertScalarExpression(pred.Low)
	if err != nil {
		return nil, fmt.Errorf("converting BETWEEN lower bound: %w", err)
	}
	high, err := convertScalarExpression(pred.High)
	if err != nil {
		return nil, fmt.Errorf("converting BETWEEN upper bound: %w", err)
	}

	// Type assertions required as conversion returns the general ScalarExpression interface
	numValue, ok := value.(filter.NumericExpression)
	if !ok {
		return nil, fmt.Errorf("BETWEEN requires numeric expression (property or number) for value, got %T", value)
	}
	numLow, ok := low.(filter.NumericExpression)
	if !ok {
		return nil, fmt.Errorf("BETWEEN requires numeric expression (property or number) for low bound, got %T", low)
	}
	numHigh, ok := high.(filter.NumericExpression)
	if !ok {
		return nil, fmt.Errorf("BETWEEN requires numeric expression (property or number) for high bound, got %T", high)
	}

	return &filter.Between{
		Value: numValue,
		Low:   numLow,
		High:  numHigh,
	}, nil
}

// convertInPredicate converts InPredicate to filter.In
func convertInPredicate(pred *InPredicate) (*filter.In, error) {
	// Value guaranteed non-nil by `@@`
	item, err := convertScalarExpression(pred.Value)
	if err != nil {
		return nil, fmt.Errorf("converting IN item: %w", err)
	}

	// List might be empty but elements guaranteed non-nil by `@@`
	list := make(filter.ScalarList, len(pred.List)) // Use filter.ScalarList type directly
	for i, listItem := range pred.List {
		converted, err := convertScalarExpression(listItem)
		if err != nil {
			return nil, fmt.Errorf("converting IN list item %d: %w", i, err)
		}
		list[i] = converted // Already filter.ScalarExpression
	}

	return &filter.In{
		Item: item,
		List: list,
	}, nil
}

// convertIsNullPredicate converts IsNullPredicate to filter.IsNull
func convertIsNullPredicate(pred *IsNullPredicate) (*filter.IsNull, error) {
	// Value guaranteed non-nil by `@@`
	// IS NULL can apply to various types, so use the general Expression interface
	value, err := convertScalarExpression(pred.Value) // Convert to specific scalar first
	if err != nil {
		// Attempt to convert as spatial if scalar fails? CQL spec check needed.
		// For now, assume it must be a scalar context based on grammar structure.
		// If IS NULL applies to raw geometry literals (not props/funcs), grammar needs update.
		return nil, fmt.Errorf("converting IS NULL value: %w", err)
	}

	// Value in filter.IsNull is filter.Expression, which filter.ScalarExpression implements.
	return &filter.IsNull{Value: value}, nil
}

// convertSpatialPredicate converts SpatialPredicate to a filter.Function.
// This provides flexibility as the filter AST doesn't have dedicated spatial comparison nodes.
func convertSpatialPredicate(pred *SpatialPredicate) (filter.BooleanExpression, error) {
	// Operands guaranteed non-nil by `@@` rules
	leftExpr, err := convertSpatialOperand(pred.Left)
	if err != nil {
		return nil, fmt.Errorf("converting left spatial operand for %s: %w", pred.Op, err)
	}
	rightExpr, err := convertSpatialOperand(pred.Right)
	if err != nil {
		return nil, fmt.Errorf("converting right spatial operand for %s: %w", pred.Op, err)
	}

	// Use lowercase for the spatial operation name in the generic function (e.g., "s_intersects")
	// This matches the JSON output expected by the tests.
	opName := strings.ToLower(pred.Op)

	// Create a generic Function
	funcNode := &filter.Function{
		Op:   opName,
		Args: []filter.Expression{leftExpr, rightExpr},
	}
	// Spatial predicates return boolean, so the Function node itself acts as BooleanExpression
	return funcNode, nil
}

// convertTemporalPredicate converts TemporalPredicate to filter.TemporalComparison
func convertTemporalPredicate(pred *TemporalPredicate) (*filter.TemporalComparison, error) {
	// Operands guaranteed non-nil by `@@` rules
	left, err := convertTemporalOperand(pred.Left)
	if err != nil {
		return nil, fmt.Errorf("converting left temporal operand for %s: %w", pred.Op, err)
	}
	right, err := convertTemporalOperand(pred.Right)
	if err != nil {
		return nil, fmt.Errorf("converting right temporal operand for %s: %w", pred.Op, err)
	}

	// Map operator names to filter constants (case handled by parser option)
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
	// Use ToUpper for map lookup robustness, although parser should handle case
	op, ok := opMap[strings.ToUpper(pred.Op)]
	if !ok {
		// Should be unreachable
		return nil, fmt.Errorf("internal grammar error: unknown temporal operator: %s", pred.Op)
	}

	return &filter.TemporalComparison{
		Name:  op,
		Left:  left,
		Right: right,
	}, nil
}

// convertArrayPredicate converts ArrayPredicate to filter.ArrayComparison
func convertArrayPredicate(pred *ArrayPredicate) (*filter.ArrayComparison, error) {
	// Operands guaranteed non-nil by `@@` rules
	left, err := convertArrayOperand(pred.Left)
	if err != nil {
		return nil, fmt.Errorf("converting left array operand for %s: %w", pred.Op, err)
	}
	right, err := convertArrayOperand(pred.Right)
	if err != nil {
		return nil, fmt.Errorf("converting right array operand for %s: %w", pred.Op, err)
	}

	// Map operator names to filter constants (case handled by parser option)
	opMap := map[string]string{
		"A_CONTAINEDBY": filter.ArrayContainedBy,
		"A_CONTAINS":    filter.ArrayContains,
		"A_EQUALS":      filter.ArrayEquals,
		"A_OVERLAPS":    filter.ArrayOverlaps,
	}
	// Use ToUpper for map lookup robustness
	op, ok := opMap[strings.ToUpper(pred.Op)]
	if !ok {
		// Should be unreachable
		return nil, fmt.Errorf("internal grammar error: unknown array operator: %s", pred.Op)
	}

	return &filter.ArrayComparison{
		Name:  op,
		Left:  left,
		Right: right,
	}, nil
}

// convertScalarExpression converts ScalarExpression node to the appropriate filter.ScalarExpression
func convertScalarExpression(expr *ScalarExpression) (filter.ScalarExpression, error) {
	switch {
	case expr.Function != nil:
		return convertFunctionExpression(expr.Function)
	case expr.CaseIFunc != nil:
		// Assumes expr.CaseIFunc.Value is non-nil
		value, err := convertScalarExpression(expr.CaseIFunc.Value)
		if err != nil {
			return nil, fmt.Errorf("converting CASEI argument: %w", err)
		}
		charValue, ok := value.(filter.CharacterExpression)
		if !ok {
			// Technically CASEI could wrap a function returning string, property, or string literal
			// The filter.Function representation handles this flexibility.
			// We might need a type check here if filter.CaseInsensitive required CharacterExpression specifically.
			// For now, represent as generic function.
		}
		_ = charValue // Avoid unused variable error if check added later
		// Represent CASEI as a function call in the filter AST
		return &filter.Function{
			Op:   "casei", // Standardize to lowercase
			Args: []filter.Expression{value},
		}, nil
	case expr.AccentIFunc != nil:
		// Assumes expr.AccentIFunc.Value is non-nil
		value, err := convertScalarExpression(expr.AccentIFunc.Value)
		if err != nil {
			return nil, fmt.Errorf("converting ACCENTI argument: %w", err)
		}
		// Similar type considerations as CASEI
		// Represent ACCENTI as a function call in the filter AST
		return &filter.Function{
			Op:   "accenti", // Standardize to lowercase
			Args: []filter.Expression{value},
		}, nil
	case expr.Boolean != nil:
		return &filter.Boolean{Value: expr.Boolean.Value}, nil
	case expr.Property != nil:
		return &filter.Property{Name: expr.Property.Name}, nil
	case expr.Literal != nil:
		return convertLiteralValue(expr.Literal)
	default:
		// Should be unreachable
		return nil, errors.New("internal grammar error: unsupported scalar expression")
	}
}

// convertFunctionExpression converts FunctionExpression to filter.Function
func convertFunctionExpression(expr *FunctionExpression) (*filter.Function, error) {
	// Args might be empty but elements guaranteed non-nil by `@@`
	args := make([]filter.Expression, len(expr.Args))
	for i, arg := range expr.Args {
		// Arguments within a function call are themselves scalar expressions
		converted, err := convertScalarExpression(arg)
		if err != nil {
			return nil, fmt.Errorf("converting argument %d for function %q: %w", i, expr.Name, err)
		}
		args[i] = converted
	}

	// Preserve case for most functions (like "Buffer", spatial functions, user functions)
	// Only force lowercase for specific known function names standardized in lowercase.
	funcName := expr.Name
	knownLowercaseFuncs := map[string]bool{
		// Map keys are UPPERCASE for case-insensitive lookup
		"CASEI":   true,
		"ACCENTI": true,
		// Add other standard lowercase functions if needed (e.g., spatial funcs if spec standardizes lowercase)
	}
	if knownLowercaseFuncs[strings.ToUpper(funcName)] {
		funcName = strings.ToLower(funcName)
	}

	return &filter.Function{
		Op:   funcName,
		Args: args,
	}, nil
}

// convertLiteralValue converts LiteralValue (string or number) to filter.ScalarExpression
func convertLiteralValue(value *LiteralValue) (filter.ScalarExpression, error) {
	if value.String != nil {
		// String value is already unquoted by the parser
		return &filter.String{Value: *value.String}, nil
	}
	if value.Number != nil {
		return &filter.Number{Value: *value.Number}, nil
	}

	// Should be unreachable
	return nil, errors.New("internal grammar error: unsupported literal value")
}

// convertSpatialOperand converts SpatialOperand (property or function) to filter.Expression
func convertSpatialOperand(operand *SpatialOperand) (filter.Expression, error) {
	// Logic to determine operand type
	if operand.Property != nil {
		// Properties can represent spatial types
		return &filter.Property{Name: operand.Property.Name}, nil
	}
	if operand.Function != nil {
		// Functions can return spatial types (e.g., Buffer, PointConstructor)
		// Convert the function expression itself
		return convertFunctionExpression(operand.Function)
	}

	// Should be unreachable
	return nil, errors.New("internal grammar error: unsupported spatial operand type")
}

// convertTemporalOperand converts TemporalOperand (property, timestamp literal, interval literal) to filter.TemporalExpression
func convertTemporalOperand(operand *TemporalOperand) (filter.TemporalExpression, error) {
	switch {
	case operand.Property != nil:
		// Property can represent a temporal type
		return &filter.Property{Name: operand.Property.Name}, nil
	case operand.Timestamp != nil:
		// Timestamp literal (string was already unquoted by parser)
		// This needs to be parsed into a filter.Date or filter.Timestamp
		instant, err := parseInstant(operand.Timestamp.Value, "temporal operand")
		if err != nil {
			return nil, err
		}
		// Check if it's a Date or Timestamp, both implement TemporalExpression
		if instant == nil {
			// This case shouldn't happen here as ".." is only valid inside intervals
			return nil, errors.New("internal error: unexpected nil instant for temporal operand")
		}
		return instant, nil // instant is filter.InstantExpression which is filter.TemporalExpression
	case operand.Interval != nil:
		return convertInterval(operand.Interval)
	default:
		// Should be unreachable
		return nil, errors.New("internal grammar error: unsupported temporal operand type")
	}
}

// convertInterval converts Interval grammar node to filter.Interval
func convertInterval(interval *Interval) (*filter.Interval, error) {
	// Start and End StringLiterals are guaranteed non-nil by grammar `@@`
	// Their .Value fields are already unquoted by the parser.
	var start, end filter.InstantExpression // Use interface type
	var err error

	start, err = parseInstant(interval.Start.Value, "interval start")
	if err != nil {
		return nil, err // Error parsing start bound
	}

	end, err = parseInstant(interval.End.Value, "interval end")
	if err != nil {
		return nil, err // Error parsing end bound
	}

	if start == nil && end == nil {
		// CQL2 spec requires at least one bound for an interval literal
		return nil, errors.New("invalid interval literal: both start and end are unbounded ('..')")
	}

	return &filter.Interval{Start: start, End: end}, nil
}

// parseInstant parses an unquoted string value into a filter.InstantExpression (Date or Timestamp).
// Handles open interval bounds ("..") by returning nil.
func parseInstant(unquotedValue, context string) (filter.InstantExpression, error) {
	if unquotedValue == ".." {
		return nil, nil // Open bound represented by nil InstantExpression
	}

	// Attempt to parse known RFC3339 and ISO 8601 / SQL date formats.
	// Order matters: Try most specific (RFC3339Nano) first.
	formats := []string{
		time.RFC3339Nano,
		time.RFC3339,          // Handles 'Z' and timezone offsets
		time.DateOnly,         // ISO 8601 Date YYYY-MM-DD
		"2006-01-02T15:04:05", // Timestamp without timezone (assume UTC or context-specific default)
	}

	var parsedTime time.Time
	var parseErr error
	parsed := false

	for _, format := range formats {
		if t, err := time.Parse(format, unquotedValue); err == nil {
			parsedTime = t
			parsed = true
			// Check if it was DateOnly format to return filter.Date
			if format == time.DateOnly {
				return &filter.Date{Value: parsedTime}, nil
			}
			// Otherwise, it's a Timestamp
			return &filter.Timestamp{Value: parsedTime}, nil
		} else {
			// Keep track of the last error for reporting if none match
			parseErr = err
		}
	}

	if !parsed {
		return nil, fmt.Errorf("invalid date or timestamp format for %s: %q (last error: %w)", context, unquotedValue, parseErr)
	}

	// Should not be reached if loop logic is correct
	return nil, fmt.Errorf("internal parsing error for %s: %q", context, unquotedValue)
}

// convertArrayOperand converts ArrayOperand (property or literal array) to filter.ArrayExpression
func convertArrayOperand(operand *ArrayOperand) (filter.ArrayExpression, error) {
	switch {
	case operand.Property != nil:
		// Property can represent an array type
		return &filter.Property{Name: operand.Property.Name}, nil
	case operand.Array != nil:
		// Literal array: "[" @@ ( "," @@ )* "]"
		// Array might be empty but elements guaranteed non-nil by `@@`
		items := make([]filter.ArrayItemExpression, len(operand.Array))
		for i, item := range operand.Array {
			// Items in an array literal are scalar expressions
			converted, err := convertScalarExpression(item)
			if err != nil {
				return nil, fmt.Errorf("converting array literal item %d: %w", i, err)
			}
			// Check if the converted scalar expression is a valid array item type
			// (string, number, boolean, property, maybe spatial/temporal literals? Check spec)
			// The filter.ArrayItemExpression interface marks valid types in the filter package.
			arrayItem, ok := converted.(filter.ArrayItemExpression)
			if !ok {
				// Provide more context about the invalid type
				return nil, fmt.Errorf("array literal item %d (type %T) is not a valid array item expression (e.g., string, number, boolean, property)", i, converted)
			}
			items[i] = arrayItem
		}
		// Use filter.Array constructor which returns the ArrayExpression interface type
		return filter.Array(items), nil // filter.Array implements filter.ArrayExpression

	default:
		// Should be unreachable
		return nil, errors.New("internal grammar error: unsupported array operand type")
	}
}
