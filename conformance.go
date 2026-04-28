package cql2

// Conformance is a bitset of OGC API - Filter conformance classes.
type Conformance uint32

const (
	ConfBasic Conformance = 1 << iota
	ConfAdvancedComparison
	ConfCaseInsensitive
	ConfAccentInsensitive
	ConfBasicSpatial
	ConfSpatial
	ConfTemporal
	ConfArray
	ConfPropertyProperty
	ConfFunctions
	ConfArithmetic

	// ConfAll enables every conformance class, including future bits.
	ConfAll = ^Conformance(0)
)

// Has reports whether c includes every flag in want.
func (c Conformance) Has(want Conformance) bool {
	return c&want == want
}

// Sentinel feature names recognized by RequiredFor for non-Operator features.
const (
	FeatureFunction         = "function"
	FeaturePropertyProperty = "property-property"
)

// RequiredFor reports the conformance class needed to use feature.
// feature is an Operator value or a sentinel string for non-Op features
// such as FeatureFunction or FeaturePropertyProperty.
//
// Returns 0 (no requirement) for unrecognized features.
func RequiredFor(feature any) Conformance {
	switch f := feature.(type) {
	case Operator:
		return requiredForOp(f)
	case string:
		switch f {
		case FeatureFunction:
			return ConfFunctions
		case FeaturePropertyProperty:
			return ConfPropertyProperty
		}
	}
	return 0
}

func requiredForOp(op Operator) Conformance {
	switch op {
	case OpAnd, OpOr, OpNot,
		OpEq, OpNeq, OpLt, OpLte, OpGt, OpGte:
		return ConfBasic
	case OpLike, OpBetween, OpIn, OpIsNull:
		return ConfAdvancedComparison
	case OpCaseI:
		return ConfCaseInsensitive
	case OpAccentI:
		return ConfAccentInsensitive
	case OpSIntersects:
		return ConfBasicSpatial
	case OpSEquals, OpSDisjoint, OpSTouches, OpSWithin,
		OpSOverlaps, OpSCrosses, OpSContains:
		return ConfSpatial
	case OpTAfter, OpTBefore, OpTContains, OpTDisjoint,
		OpTDuring, OpTEquals, OpTFinishedBy, OpTFinishes,
		OpTIntersects, OpTMeets, OpTMetBy, OpTOverlappedBy,
		OpTOverlaps, OpTStartedBy, OpTStarts:
		return ConfTemporal
	case OpAContains, OpAContainedBy, OpAEquals, OpAOverlaps:
		return ConfArray
	case OpAdd, OpSub, OpMul, OpDiv, OpMod, OpPow, OpIDiv:
		return ConfArithmetic
	}
	return 0
}

// HumanFeatureName returns a human-readable label for an Operator or
// sentinel string, suitable for the Feature field of *ConformanceError.
func HumanFeatureName(feature any) string {
	switch f := feature.(type) {
	case Operator:
		return humanOpName(f)
	case string:
		switch f {
		case FeatureFunction:
			return "function call"
		case FeaturePropertyProperty:
			return "property-to-property comparison"
		}
		return f
	}
	return ""
}

func humanOpName(op Operator) string {
	switch op {
	case OpAnd:
		return "AND"
	case OpOr:
		return "OR"
	case OpNot:
		return "NOT"
	case OpEq:
		return "="
	case OpNeq:
		return "<>"
	case OpLt:
		return "<"
	case OpLte:
		return "<="
	case OpGt:
		return ">"
	case OpGte:
		return ">="
	case OpLike:
		return "LIKE"
	case OpBetween:
		return "BETWEEN"
	case OpIn:
		return "IN"
	case OpIsNull:
		return "IS NULL"
	case OpCaseI:
		return "CASEI"
	case OpAccentI:
		return "ACCENTI"
	case OpAdd:
		return "+"
	case OpSub:
		return "-"
	case OpMul:
		return "*"
	case OpDiv:
		return "/"
	case OpMod:
		return "%"
	case OpPow:
		return "^"
	case OpIDiv:
		return "div"
	}
	// Spatial / temporal / array operators: emit upper-case form.
	s := string(op)
	out := make([]byte, len(s))
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c >= 'a' && c <= 'z' {
			c -= 'a' - 'A'
		}
		out[i] = c
	}
	return string(out)
}
