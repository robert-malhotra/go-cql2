// Package cqlbuilder provides generics-based, compile-time constrained helpers
// to build go-ogc filter ASTs. We keep friendly names (In, Eq, Between, Lt,...)
// and avoid runtime errors by constraining T to supported natives
package cqlbuilder

import (
	"time"

	"github.com/planetlabs/go-ogc/filter"
)

// ---------- Type-set constraints ----------
//
// Numeric allows common signed ints and floats.
// (Intentionally exclude uint* to avoid precision surprises converting to float64.)
type Numeric interface {
	~int | ~int32 | ~int64 | ~float32 | ~float64
}

// ScalarNative is the union of supported native scalar literal types.
type ScalarNative interface {
	~string | ~bool | Numeric
}

func toScalarNode[T ScalarNative](v T) filter.ScalarExpression {
	return scalarFromNative(any(v))
}

func toNumberNode[T Numeric](v T) *filter.Number {
	return numericFromNative(any(v))
}

func toArrayItem[T ScalarNative](v T) filter.ArrayItemExpression {
	expr := toScalarNode(v)
	item, ok := expr.(filter.ArrayItemExpression)
	if !ok {
		panic("unsupported array item expression")
	}
	return item
}

func scalarFromNative(v any) filter.ScalarExpression {
	switch x := v.(type) {
	case string:
		return &filter.String{Value: x}
	case bool:
		return &filter.Boolean{Value: x}
	case int, int32, int64, float32, float64:
		return numericFromNative(x)
	default:
		panic("unreachable: ScalarNative exhaustiveness")
	}
}

func numericFromNative(v any) *filter.Number {
	switch x := v.(type) {
	case int:
		return &filter.Number{Value: float64(x)}
	case int32:
		return &filter.Number{Value: float64(x)}
	case int64:
		return &filter.Number{Value: float64(x)}
	case float32:
		return &filter.Number{Value: float64(x)}
	case float64:
		return &filter.Number{Value: x}
	default:
		panic("unreachable: Numeric exhaustiveness")
	}
}

func copyBooleanArgs[T filter.BooleanExpression](args []T) []filter.BooleanExpression {
	out := make([]filter.BooleanExpression, len(args))
	for i, a := range args {
		out[i] = a
	}
	return out
}

func newComparison(name string, left, right filter.ScalarExpression) *filter.Comparison {
	return &filter.Comparison{Name: name, Left: left, Right: right}
}

func newSpatialComparison(name string, left, right filter.SpatialExpression) *filter.SpatialComparison {
	return &filter.SpatialComparison{Name: name, Left: left, Right: right}
}

func newTemporalComparison(name string, left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: name, Left: left, Right: right}
}

func newArrayComparison(name string, left, right filter.ArrayExpression) *filter.ArrayComparison {
	return &filter.ArrayComparison{Name: name, Left: left, Right: right}
}

// Operation enums for comparison builders.

type SpatialOp string

const (
	SpatialContains   SpatialOp = SpatialOp(filter.GeometryContains)
	SpatialCrosses    SpatialOp = SpatialOp(filter.GeometryCrosses)
	SpatialDisjoint   SpatialOp = SpatialOp(filter.GeometryDisjoint)
	SpatialEquals     SpatialOp = SpatialOp(filter.GeometryEquals)
	SpatialIntersects SpatialOp = SpatialOp(filter.GeometryIntersects)
	SpatialOverlaps   SpatialOp = SpatialOp(filter.GeometryOverlaps)
	SpatialTouches    SpatialOp = SpatialOp(filter.GeometryTouches)
	SpatialWithin     SpatialOp = SpatialOp(filter.GeometryWithin)
)

type TemporalOp string

const (
	TemporalAfter        TemporalOp = TemporalOp(filter.TimeAfter)
	TemporalBefore       TemporalOp = TemporalOp(filter.TimeBefore)
	TemporalContains     TemporalOp = TemporalOp(filter.TimeContains)
	TemporalDisjoint     TemporalOp = TemporalOp(filter.TimeDisjoint)
	TemporalDuring       TemporalOp = TemporalOp(filter.TimeDuring)
	TemporalEquals       TemporalOp = TemporalOp(filter.TimeEquals)
	TemporalFinishedBy   TemporalOp = TemporalOp(filter.TimeFinishedBy)
	TemporalFinishes     TemporalOp = TemporalOp(filter.TimeFinishes)
	TemporalIntersects   TemporalOp = TemporalOp(filter.TimeIntersects)
	TemporalMeets        TemporalOp = TemporalOp(filter.TimeMeets)
	TemporalMetBy        TemporalOp = TemporalOp(filter.TimeMetBy)
	TemporalOverlappedBy TemporalOp = TemporalOp(filter.TimeOverlappedBy)
	TemporalOverlaps     TemporalOp = TemporalOp(filter.TimeOverlaps)
	TemporalStartedBy    TemporalOp = TemporalOp(filter.TimeStartedBy)
	TemporalStarts       TemporalOp = TemporalOp(filter.TimeStarts)
)

type ArrayOp string

const (
	ArrayContainedBy ArrayOp = ArrayOp(filter.ArrayContainedBy)
	ArrayContains    ArrayOp = ArrayOp(filter.ArrayContains)
	ArrayEquals      ArrayOp = ArrayOp(filter.ArrayEquals)
	ArrayOverlaps    ArrayOp = ArrayOp(filter.ArrayOverlaps)
)

//
// ---------- One unified literal ----------
//

// Lit converts a native Go value (string, bool, numeric) into the corresponding
// filter node (*filter.String / *filter.Boolean / *filter.Number) and returns it
// as filter.Expression. Callers can assert to a narrower interface as needed,
// e.g. Lit(true).(filter.BooleanExpression), Lit("x").(filter.PatternExpression),
// or Lit(1).(filter.ArrayItemExpression).
func Lit[T ScalarNative](v T) filter.Expression {
	return toScalarNode(v) // concrete node implements Expression
}

//
// ---------- Core leaf / utility constructors ----------
//

func Property(name string) *filter.Property { return &filter.Property{Name: name} }
func Date(v time.Time) *filter.Date         { return &filter.Date{Value: v} }
func Timestamp(v time.Time) *filter.Timestamp {
	return &filter.Timestamp{Value: v}
}
func BBox(extent ...float64) *filter.BoundingBox { return &filter.BoundingBox{Extent: extent} }
func Geometry(value any) *filter.Geometry        { return &filter.Geometry{Value: value} }

// Array builds a literal array from *homogeneous* native scalar values.
//
//	(For mixed-type arrays, build with filter.Array{
//	  Lit("a").(filter.ArrayItemExpression), Lit(true).(filter.ArrayItemExpression), ...
//	})
func Array[T ScalarNative](items ...T) filter.Array {
	out := make(filter.Array, len(items))
	for i, v := range items {
		out[i] = toArrayItem(v)
	}
	return out
}

//
// ---------- Logical (generic over filter.BooleanExpression) ----------
//

func And[T filter.BooleanExpression](args ...T) *filter.And {
	return &filter.And{Args: copyBooleanArgs(args)}
}

func Or[T filter.BooleanExpression](args ...T) *filter.Or {
	return &filter.Or{Args: copyBooleanArgs(args)}
}

func Not[T filter.BooleanExpression](arg T) *filter.Not {
	return &filter.Not{Arg: arg}
}

//
// ---------- Comparisons (generic over left expr + native RHS) ----------
//

func Eq[L filter.ScalarExpression, T ScalarNative](left L, right T) *filter.Comparison {
	return newComparison(filter.Equals, left, toScalarNode(right))
}
func Ne[L filter.ScalarExpression, T ScalarNative](left L, right T) *filter.Comparison {
	return newComparison(filter.NotEquals, left, toScalarNode(right))
}
func Lt[L filter.NumericExpression, T Numeric](left L, right T) *filter.Comparison {
	return newComparison(filter.LessThan, left, toNumberNode(right))
}
func Le[L filter.NumericExpression, T Numeric](left L, right T) *filter.Comparison {
	return newComparison(filter.LessThanOrEquals, left, toNumberNode(right))
}
func Gt[L filter.NumericExpression, T Numeric](left L, right T) *filter.Comparison {
	return newComparison(filter.GreaterThan, left, toNumberNode(right))
}
func Ge[L filter.NumericExpression, T Numeric](left L, right T) *filter.Comparison {
	return newComparison(filter.GreaterThanOrEquals, left, toNumberNode(right))
}

//
// ---------- LIKE & wrappers (generic over expression kinds) ----------
//

// NOTE: filter.PatternExpression is implemented by *filter.String, *filter.CaseInsensitive,
// *filter.AccentInsensitive. Do not pass *filter.Function unless go-ogc adds support.

func Like[V filter.CharacterExpression, P filter.PatternExpression](value V, pattern P) *filter.Like {
	return &filter.Like{Value: value, Pattern: pattern}
}

func CaseInsensitiveExpr[V filter.CharacterExpression](v V) *filter.CaseInsensitive {
	return &filter.CaseInsensitive{Value: v}
}

func CaseInsensitive[T string](v T) *filter.CaseInsensitive {
	return &filter.CaseInsensitive{Value: toScalarNode(v).(filter.CharacterExpression)}
}

func AccentInsensitiveExpr[V filter.CharacterExpression](v V) *filter.AccentInsensitive {
	return &filter.AccentInsensitive{Value: v}
}

func AccentInsensitive[T string](v T) *filter.AccentInsensitive {
	return &filter.AccentInsensitive{Value: toScalarNode(v).(filter.CharacterExpression)}
}

//
// ---------- BETWEEN / IN / IS NULL (generic) ----------
//

func Between[V filter.NumericExpression, T Numeric](value V, low, high T) *filter.Between {
	return &filter.Between{Value: value, Low: toNumberNode(low), High: toNumberNode(high)}
}

func In[I filter.ScalarExpression, T ScalarNative](item I, list ...T) *filter.In {
	sl := make(filter.ScalarList, len(list))
	for i, v := range list {
		sl[i] = toScalarNode(v)
	}
	return &filter.In{Item: item, List: sl}
}

func IsNullExpr[E filter.Expression](expr E) *filter.IsNull { return &filter.IsNull{Value: expr} }
func IsNull[T ScalarNative](v T) *filter.IsNull             { return &filter.IsNull{Value: toScalarNode(v)} }

//

// ---------- Spatial (generic over filter.SpatialExpression) ----------
//

func Spatial[L filter.SpatialExpression, R filter.SpatialExpression](op SpatialOp, left L, right R) *filter.SpatialComparison {
	return newSpatialComparison(string(op), left, right)
}

//
// ---------- Temporal (generic over filter.TemporalExpression) ----------
//

func Interval[S filter.InstantExpression, E filter.InstantExpression](start S, end E) *filter.Interval {
	return &filter.Interval{Start: start, End: end}
}

func Temporal[L filter.TemporalExpression, R filter.TemporalExpression](op TemporalOp, left L, right R) *filter.TemporalComparison {
	return newTemporalComparison(string(op), left, right)
}

//
// ---------- Array comparisons (generic over filter.ArrayExpression) ----------
//

func ArrayCompare[L filter.ArrayExpression, R filter.ArrayExpression](op ArrayOp, left L, right R) *filter.ArrayComparison {
	return newArrayComparison(string(op), left, right)
}
