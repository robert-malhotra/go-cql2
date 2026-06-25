package cql2

import (
	"strings"

	"github.com/exergy-dev/go-topology-suite/geom"
)

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
	// ConfBasicSpatialPlus is the OGC "basic-spatial-functions-plus" class,
	// which extends ConfBasicSpatial to permit S_INTERSECTS with any
	// geometry literal (LineString, Polygon, MultiPoint, etc.) rather
	// than only Point or BBox.
	ConfBasicSpatialPlus

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

// RequiresBasicSpatialPlus reports whether an S_INTERSECTS predicate
// uses geometry literals beyond Point and BBox — i.e. LineString, Polygon,
// MultiPoint, MultiLineString, MultiPolygon, or GeometryCollection. Such
// predicates require the OGC basic-spatial-functions-plus class on top
// of the basic-spatial-functions class.
//
// Returns false for ops other than S_INTERSECTS, and for predicates whose
// only literals are Point or BBox.
func RequiresBasicSpatialPlus(op Operator, args []Node) bool {
	if op != OpSIntersects {
		return false
	}
	for _, a := range args {
		switch v := a.(type) {
		case *BBoxLit:
			// allowed in basic
		case *GeomLit:
			if !isPointGeometry(v.Geom) {
				return true
			}
		}
	}
	return false
}

func isPointGeometry(g geom.Geometry) bool {
	return g != nil && g.Type() == geom.PointType
}

// RequiresPropertyPropertyShape reports whether a binary predicate op with
// the given argument shapes deviates from the Basic-CQL2 "property-on-left,
// literal-on-right" form, and thus requires the Property-Property
// Comparisons class (/req/property-property/withdraw-permissions).
//
// The check fires for comparison, LIKE, BETWEEN, IN, spatial, temporal, and
// array predicates. It returns true when:
//
//   - the right-hand side (or any argument after the first, for BETWEEN/IN)
//     is a *PropertyRef — covers `prop = other_prop`, `s_intersects(g, h)`,
//     `t_after(t, other_t)`, etc.
//   - the left-hand side is a CQL2 literal node — covers `'foo' = name`,
//     `s_intersects(POINT(1 2), geom)`, etc. Literal includes string,
//     number, boolean, timestamp, date, interval, geometry, bbox, array.
//
// Arithmetic expressions on either side are *not* flagged here; they're
// gated by ConfArithmetic. Function calls are gated by ConfFunctions.
func RequiresPropertyPropertyShape(op Operator, args []Node) bool {
	if !isPropertyPropertyGated(op) || len(args) == 0 {
		return false
	}
	if isLiteralNode(args[0]) {
		return true
	}
	for _, a := range args[1:] {
		if _, ok := a.(*PropertyRef); ok {
			return true
		}
	}
	return false
}

func isPropertyPropertyGated(op Operator) bool {
	return opTable[op].propGate
}

// isLiteralNode reports whether n is a literal-kind AST node — i.e. a
// value that, when used as the left-hand side of a Basic-CQL2 predicate,
// makes the predicate non-Basic per /per/basic-cql2/cql2-filter.
func isLiteralNode(n Node) bool {
	switch n.(type) {
	case *StringLit, *NumLit, *BoolLit, *NullLit,
		*TimestampLit, *DateLit, *IntervalLit,
		*GeomLit, *BBoxLit, *ArrayLit:
		return true
	}
	return false
}

func requiredForOp(op Operator) Conformance {
	return opTable[op].conf
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
	if h := opTable[op].human; h != "" {
		return h
	}
	// Spatial / temporal / array operators have no bespoke label: emit the
	// upper-case form of the operator string (e.g. S_INTERSECTS).
	return strings.ToUpper(string(op))
}
