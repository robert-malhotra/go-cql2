package cql2

// Operator is a typed string identifying a CQL2 operator.
type Operator string

const (
	// Logical
	OpAnd Operator = "and"
	OpOr  Operator = "or"
	OpNot Operator = "not"

	// Comparison
	OpEq  Operator = "="
	OpNeq Operator = "<>"
	OpLt  Operator = "<"
	OpLte Operator = "<="
	OpGt  Operator = ">"
	OpGte Operator = ">="

	// Advanced comparison
	OpLike    Operator = "like"
	OpBetween Operator = "between"
	OpIn      Operator = "in"
	OpIsNull  Operator = "isNull"

	// Arithmetic
	OpAdd  Operator = "+"
	OpSub  Operator = "-"
	OpMul  Operator = "*"
	OpDiv  Operator = "/"
	OpMod  Operator = "%"
	OpPow  Operator = "^"
	OpIDiv Operator = "div"

	// Spatial
	OpSIntersects Operator = "s_intersects"
	OpSEquals     Operator = "s_equals"
	OpSDisjoint   Operator = "s_disjoint"
	OpSTouches    Operator = "s_touches"
	OpSWithin     Operator = "s_within"
	OpSOverlaps   Operator = "s_overlaps"
	OpSCrosses    Operator = "s_crosses"
	OpSContains   Operator = "s_contains"

	// Temporal
	OpTAfter        Operator = "t_after"
	OpTBefore       Operator = "t_before"
	OpTContains     Operator = "t_contains"
	OpTDisjoint     Operator = "t_disjoint"
	OpTDuring       Operator = "t_during"
	OpTEquals       Operator = "t_equals"
	OpTFinishedBy   Operator = "t_finishedBy"
	OpTFinishes     Operator = "t_finishes"
	OpTIntersects   Operator = "t_intersects"
	OpTMeets        Operator = "t_meets"
	OpTMetBy        Operator = "t_metBy"
	OpTOverlappedBy Operator = "t_overlappedBy"
	OpTOverlaps     Operator = "t_overlaps"
	OpTStartedBy    Operator = "t_startedBy"
	OpTStarts       Operator = "t_starts"

	// Array
	OpAContains    Operator = "a_contains"
	OpAContainedBy Operator = "a_containedBy"
	OpAEquals      Operator = "a_equals"
	OpAOverlaps    Operator = "a_overlaps"

	// Case / accent insensitivity (modeled as ops per spec)
	OpCaseI   Operator = "casei"
	OpAccentI Operator = "accenti"
)

// opMeta is the single source of truth for per-operator metadata. Every
// switch that previously enumerated operators by hand (conformance gating,
// arity validation, result typing, human names, canonical lookup) now reads
// from opTable instead, so the operator set is maintained in exactly one place.
type opMeta struct {
	conf      Conformance // conformance class RequiredFor the op; 0 = none
	result    Kind        // result kind of an Op with this operator
	human     string      // human label; "" => upper-case of the op string
	arity     int         // exact arg count; -1 = n-ary (>=2); 0 = no arity rule
	propGate  bool        // participates in Property-Property shape gating
	arith     bool        // arithmetic op (both-operands-property gating)
	passthru  bool        // result kind is that of the first argument (casei/accenti)
	funcStyle bool        // rendered/parsed in function-call form (spatial/temporal/array/casei/accenti)
}

// opTable holds metadata for every Operator. Keep it exhaustive: TestOpTable
// asserts there is a row for each Op* constant.
var opTable = map[Operator]opMeta{
	OpAnd: {conf: ConfBasic, result: KindResultBoolean, human: "AND", arity: -1},
	OpOr:  {conf: ConfBasic, result: KindResultBoolean, human: "OR", arity: -1},
	OpNot: {conf: ConfBasic, result: KindResultBoolean, human: "NOT", arity: 1},

	OpEq:  {conf: ConfBasic, result: KindResultBoolean, human: "=", arity: 2, propGate: true},
	OpNeq: {conf: ConfBasic, result: KindResultBoolean, human: "<>", arity: 2, propGate: true},
	OpLt:  {conf: ConfBasic, result: KindResultBoolean, human: "<", arity: 2, propGate: true},
	OpLte: {conf: ConfBasic, result: KindResultBoolean, human: "<=", arity: 2, propGate: true},
	OpGt:  {conf: ConfBasic, result: KindResultBoolean, human: ">", arity: 2, propGate: true},
	OpGte: {conf: ConfBasic, result: KindResultBoolean, human: ">=", arity: 2, propGate: true},

	OpLike:    {conf: ConfAdvancedComparison, result: KindResultBoolean, human: "LIKE", arity: 2, propGate: true},
	OpBetween: {conf: ConfAdvancedComparison, result: KindResultBoolean, human: "BETWEEN", arity: 3, propGate: true},
	OpIn:      {conf: ConfAdvancedComparison, result: KindResultBoolean, human: "IN", arity: 2, propGate: true},
	OpIsNull:  {conf: ConfAdvancedComparison, result: KindResultBoolean, human: "IS NULL", arity: 1},

	OpAdd:  {conf: ConfArithmetic, result: KindResultNumber, human: "+", arity: -1, arith: true},
	OpSub:  {conf: ConfArithmetic, result: KindResultNumber, human: "-", arity: -1, arith: true},
	OpMul:  {conf: ConfArithmetic, result: KindResultNumber, human: "*", arith: true, arity: -1},
	OpDiv:  {conf: ConfArithmetic, result: KindResultNumber, human: "/", arith: true, arity: -1},
	OpMod:  {conf: ConfArithmetic, result: KindResultNumber, human: "%", arith: true, arity: -1},
	OpPow:  {conf: ConfArithmetic, result: KindResultNumber, human: "^", arith: true, arity: -1},
	OpIDiv: {conf: ConfArithmetic, result: KindResultNumber, human: "div", arith: true, arity: -1},

	OpSIntersects: {conf: ConfBasicSpatial, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpSEquals:     {conf: ConfSpatial, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpSDisjoint:   {conf: ConfSpatial, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpSTouches:    {conf: ConfSpatial, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpSWithin:     {conf: ConfSpatial, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpSOverlaps:   {conf: ConfSpatial, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpSCrosses:    {conf: ConfSpatial, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpSContains:   {conf: ConfSpatial, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},

	OpTAfter:        {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTBefore:       {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTContains:     {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTDisjoint:     {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTDuring:       {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTEquals:       {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTFinishedBy:   {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTFinishes:     {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTIntersects:   {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTMeets:        {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTMetBy:        {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTOverlappedBy: {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTOverlaps:     {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTStartedBy:    {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpTStarts:       {conf: ConfTemporal, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},

	OpAContains:    {conf: ConfArray, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpAContainedBy: {conf: ConfArray, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpAEquals:      {conf: ConfArray, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},
	OpAOverlaps:    {conf: ConfArray, result: KindResultBoolean, arity: 2, propGate: true, funcStyle: true},

	OpCaseI:   {conf: ConfCaseInsensitive, human: "CASEI", arity: 1, passthru: true, funcStyle: true},
	OpAccentI: {conf: ConfAccentInsensitive, human: "ACCENTI", arity: 1, passthru: true, funcStyle: true},
}

// Operators returns every operator known to the package, in no particular
// order. It is the canonical source for codecs that need to enumerate the
// operator set (e.g. building a case-insensitive name lookup).
func Operators() []Operator {
	ops := make([]Operator, 0, len(opTable))
	for op := range opTable {
		ops = append(ops, op)
	}
	return ops
}

// FunctionStyleOperators returns the operators that use CQL2 function-call
// syntax (the spatial, temporal, and array predicates plus CASEI/ACCENTI),
// as opposed to the infix logical/comparison/arithmetic operators. The text
// codec uses this to recognize and emit those operators without re-listing
// them.
func FunctionStyleOperators() []Operator {
	var ops []Operator
	for op, m := range opTable {
		if m.funcStyle {
			ops = append(ops, op)
		}
	}
	return ops
}
