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
	OpTFinishedBy   Operator = "t_finishedby"
	OpTFinishes     Operator = "t_finishes"
	OpTIntersects   Operator = "t_intersects"
	OpTMeets        Operator = "t_meets"
	OpTMetBy        Operator = "t_metby"
	OpTOverlappedBy Operator = "t_overlappedby"
	OpTOverlaps     Operator = "t_overlaps"
	OpTStartedBy    Operator = "t_startedby"
	OpTStarts       Operator = "t_starts"

	// Array
	OpAContains    Operator = "a_contains"
	OpAContainedBy Operator = "a_containedby"
	OpAEquals      Operator = "a_equals"
	OpAOverlaps    Operator = "a_overlaps"

	// Case / accent insensitivity (modeled as ops per spec)
	OpCaseI   Operator = "casei"
	OpAccentI Operator = "accenti"
)
