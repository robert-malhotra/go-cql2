// Package cqlbuilder provides helper functions to easily create
// filter.Expression structs for CQL2 JSON ASTs.
package cqlbuilder

import (
	"fmt"
	"time"

	"github.com/planetlabs/go-ogc/filter"
)

// --- Type Conversion Helpers (Unexported) ---

// toExpression converts a native Go type or an existing filter expression into a filter.Expression.
func toExpression(v any) (filter.Expression, error) {
	switch val := v.(type) {
	case filter.Expression:
		return val, nil
	case string:
		return &filter.String{Value: val}, nil
	case bool:
		return &filter.Boolean{Value: val}, nil
	case float64:
		return &filter.Number{Value: val}, nil
	case int:
		return &filter.Number{Value: float64(val)}, nil
	case int32:
		return &filter.Number{Value: float64(val)}, nil
	case int64:
		return &filter.Number{Value: float64(val)}, nil
	default:
		return nil, fmt.Errorf("unsupported native type %T for expression; wrap complex types with cql helpers (e.g., cql.BBox(...))", v)
	}
}

// toBoolean converts a native bool or an existing filter expression into a filter.BooleanExpression.
func toBoolean(v any) (filter.BooleanExpression, error) {
	switch val := v.(type) {
	case filter.BooleanExpression:
		return val, nil
	case bool:
		return &filter.Boolean{Value: val}, nil
	default:
		return nil, fmt.Errorf("unsupported type %T for boolean expression", v)
	}
}

// toScalar converts a native Go type or an existing filter expression into a filter.ScalarExpression.
func toScalar(v any) (filter.ScalarExpression, error) {
	switch val := v.(type) {
	case filter.ScalarExpression:
		return val, nil
	case string:
		return &filter.String{Value: val}, nil
	case bool:
		return &filter.Boolean{Value: val}, nil
	case float64:
		return &filter.Number{Value: val}, nil
	case int:
		return &filter.Number{Value: float64(val)}, nil
	case int32:
		return &filter.Number{Value: float64(val)}, nil
	case int64:
		return &filter.Number{Value: float64(val)}, nil
	default:
		return nil, fmt.Errorf("unsupported type %T for scalar expression", v)
	}
}

// toCharacter converts a native Go type or an existing filter expression into a filter.CharacterExpression.
func toCharacter(v any) (filter.CharacterExpression, error) {
	switch val := v.(type) {
	case filter.CharacterExpression:
		return val, nil
	case string:
		return &filter.String{Value: val}, nil
	default:
		return nil, fmt.Errorf("unsupported type %T for character expression", v)
	}
}

// toNumeric converts a native Go type or an existing filter expression into a filter.NumericExpression.
func toNumeric(v any) (filter.NumericExpression, error) {
	switch val := v.(type) {
	case filter.NumericExpression:
		return val, nil
	case float64:
		return &filter.Number{Value: val}, nil
	case int:
		return &filter.Number{Value: float64(val)}, nil
	case int32:
		return &filter.Number{Value: float64(val)}, nil
	case int64:
		return &filter.Number{Value: float64(val)}, nil
	default:
		return nil, fmt.Errorf("unsupported type %T for numeric expression", v)
	}
}

func toPattern(v any) (filter.PatternExpression, error) {
	switch val := v.(type) {
	case filter.PatternExpression:
		return val, nil
	case string:
		return &filter.String{Value: val}, nil
	default:
		return nil, fmt.Errorf("unsupported type %T for pattern expression; use string, cql.CaseInsensitive(...), or cql.AccentInsensitive(...)", v)
	}
}

// toArrayItem converts a native Go type or an existing filter expression into a filter.ArrayItemExpression.
func toArrayItem(v any) (filter.ArrayItemExpression, error) {
	switch val := v.(type) {
	case filter.ArrayItemExpression:
		return val, nil
	case string:
		return &filter.String{Value: val}, nil
	case bool:
		return &filter.Boolean{Value: val}, nil
	case float64:
		return &filter.Number{Value: val}, nil
	case int:
		return &filter.Number{Value: float64(val)}, nil
	default:
		return nil, fmt.Errorf("unsupported type %T for array item expression", v)
	}
}

// --- Literals and Properties (Complex Types Only) ---

func Property(name string) *filter.Property {
	return &filter.Property{Name: name}
}
func Date(value time.Time) *filter.Date {
	return &filter.Date{Value: value}
}
func Timestamp(value time.Time) *filter.Timestamp {
	return &filter.Timestamp{Value: value}
}
func BBox(extent ...float64) *filter.BoundingBox {
	return &filter.BoundingBox{Extent: extent}
}
func Geometry(value any) *filter.Geometry {
	return &filter.Geometry{Value: value}
}

// Array creates an array literal from native types or other expressions.
func Array(items ...any) (filter.Array, error) {
	expressions := make([]filter.ArrayItemExpression, len(items))
	for i, item := range items {
		expr, err := toArrayItem(item)
		if err != nil {
			return nil, err
		}
		expressions[i] = expr
	}
	return filter.Array(expressions), nil
}

// --- Logical Operators ---

func And(args ...any) (*filter.And, error) {
	expressions := make([]filter.BooleanExpression, len(args))
	for i, arg := range args {
		expr, err := toBoolean(arg)
		if err != nil {
			return nil, err
		}
		expressions[i] = expr
	}
	return &filter.And{Args: expressions}, nil
}
func Or(args ...any) (*filter.Or, error) {
	expressions := make([]filter.BooleanExpression, len(args))
	for i, arg := range args {
		expr, err := toBoolean(arg)
		if err != nil {
			return nil, err
		}
		expressions[i] = expr
	}
	return &filter.Or{Args: expressions}, nil
}
func Not(arg any) (*filter.Not, error) {
	b, err := toBoolean(arg)
	if err != nil {
		return nil, err
	}
	return &filter.Not{Arg: b}, nil
}

// --- Comparison Operators ---

func Eq(left, right any) (*filter.Comparison, error) {
	l, err := toScalar(left)
	if err != nil {
		return nil, err
	}
	r, err := toScalar(right)
	if err != nil {
		return nil, err
	}
	return &filter.Comparison{Name: filter.Equals, Left: l, Right: r}, nil
}
func Ne(left, right any) (*filter.Comparison, error) {
	l, err := toScalar(left)
	if err != nil {
		return nil, err
	}
	r, err := toScalar(right)
	if err != nil {
		return nil, err
	}
	return &filter.Comparison{Name: filter.NotEquals, Left: l, Right: r}, nil
}
func Lt(left, right any) (*filter.Comparison, error) {
	l, err := toScalar(left)
	if err != nil {
		return nil, err
	}
	r, err := toScalar(right)
	if err != nil {
		return nil, err
	}
	return &filter.Comparison{Name: filter.LessThan, Left: l, Right: r}, nil
}
func Le(left, right any) (*filter.Comparison, error) {
	l, err := toScalar(left)
	if err != nil {
		return nil, err
	}
	r, err := toScalar(right)
	if err != nil {
		return nil, err
	}
	return &filter.Comparison{Name: filter.LessThanOrEquals, Left: l, Right: r}, nil
}
func Gt(left, right any) (*filter.Comparison, error) {
	l, err := toScalar(left)
	if err != nil {
		return nil, err
	}
	r, err := toScalar(right)
	if err != nil {
		return nil, err
	}
	return &filter.Comparison{Name: filter.GreaterThan, Left: l, Right: r}, nil
}
func Ge(left, right any) (*filter.Comparison, error) {
	l, err := toScalar(left)
	if err != nil {
		return nil, err
	}
	r, err := toScalar(right)
	if err != nil {
		return nil, err
	}
	return &filter.Comparison{Name: filter.GreaterThanOrEquals, Left: l, Right: r}, nil
}

func Like(value, pattern any) (*filter.Like, error) {
	v, err := toCharacter(value)
	if err != nil {
		return nil, err
	}
	p, err := toPattern(pattern)
	if err != nil {
		return nil, err
	}
	return &filter.Like{Value: v, Pattern: p}, nil
}

// CaseInsensitive wraps a character expression to be case-insensitive.
func CaseInsensitive(value any) (*filter.CaseInsensitive, error) {
	v, err := toCharacter(value)
	if err != nil {
		return nil, err
	}
	return &filter.CaseInsensitive{Value: v}, nil
}

// AccentInsensitive wraps a character expression to be accent-insensitive.
func AccentInsensitive(value any) (*filter.AccentInsensitive, error) {
	v, err := toCharacter(value)
	if err != nil {
		return nil, err
	}
	return &filter.AccentInsensitive{Value: v}, nil
}

func Between(value, low, high any) (*filter.Between, error) {
	v, err := toNumeric(value)
	if err != nil {
		return nil, err
	}
	l, err := toNumeric(low)
	if err != nil {
		return nil, err
	}
	h, err := toNumeric(high)
	if err != nil {
		return nil, err
	}
	return &filter.Between{Value: v, Low: l, High: h}, nil
}

func In(item any, list ...any) (*filter.In, error) {
	i, err := toScalar(item)
	if err != nil {
		return nil, err
	}
	scalarList := make([]filter.ScalarExpression, len(list))
	for j, v := range list {
		s, err := toScalar(v)
		if err != nil {
			return nil, err
		}
		scalarList[j] = s
	}
	return &filter.In{Item: i, List: filter.ScalarList(scalarList)}, nil
}

func IsNull(value any) (*filter.IsNull, error) {
	v, err := toExpression(value)
	if err != nil {
		s, sErr := toScalar(value)
		if sErr != nil {
			return nil, err
		}
		v = s
	}
	return &filter.IsNull{Value: v}, nil
}

// --- Spatial Operators ---

func SContains(left, right filter.SpatialExpression) *filter.SpatialComparison {
	return &filter.SpatialComparison{Name: filter.GeometryContains, Left: left, Right: right}
}
func SCrosses(left, right filter.SpatialExpression) *filter.SpatialComparison {
	return &filter.SpatialComparison{Name: filter.GeometryCrosses, Left: left, Right: right}
}
func SDisjoint(left, right filter.SpatialExpression) *filter.SpatialComparison {
	return &filter.SpatialComparison{Name: filter.GeometryDisjoint, Left: left, Right: right}
}
func SEquals(left, right filter.SpatialExpression) *filter.SpatialComparison {
	return &filter.SpatialComparison{Name: filter.GeometryEquals, Left: left, Right: right}
}
func SIntersects(left, right filter.SpatialExpression) *filter.SpatialComparison {
	return &filter.SpatialComparison{Name: filter.GeometryIntersects, Left: left, Right: right}
}
func SOverlaps(left, right filter.SpatialExpression) *filter.SpatialComparison {
	return &filter.SpatialComparison{Name: filter.GeometryOverlaps, Left: left, Right: right}
}
func STouches(left, right filter.SpatialExpression) *filter.SpatialComparison {
	return &filter.SpatialComparison{Name: filter.GeometryTouches, Left: left, Right: right}
}
func SWithin(left, right filter.SpatialExpression) *filter.SpatialComparison {
	return &filter.SpatialComparison{Name: filter.GeometryWithin, Left: left, Right: right}
}

// --- Temporal Operators ---

func Interval(start, end filter.InstantExpression) *filter.Interval {
	return &filter.Interval{Start: start, End: end}
}
func TAfter(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeAfter, Left: left, Right: right}
}
func TBefore(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeBefore, Left: left, Right: right}
}
func TContains(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeContains, Left: left, Right: right}
}
func TDisjoint(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeDisjoint, Left: left, Right: right}
}
func TDuring(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeDuring, Left: left, Right: right}
}
func TEquals(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeEquals, Left: left, Right: right}
}
func TFinishedBy(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeFinishedBy, Left: left, Right: right}
}
func TFinishes(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeFinishes, Left: left, Right: right}
}
func TIntersects(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeIntersects, Left: left, Right: right}
}
func TMeets(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeMeets, Left: left, Right: right}
}
func TMetBy(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeMetBy, Left: left, Right: right}
}
func TOverlappedBy(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeOverlappedBy, Left: left, Right: right}
}
func TOverlaps(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeOverlaps, Left: left, Right: right}
}
func TStartedBy(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeStartedBy, Left: left, Right: right}
}
func TStarts(left, right filter.TemporalExpression) *filter.TemporalComparison {
	return &filter.TemporalComparison{Name: filter.TimeStarts, Left: left, Right: right}
}

// --- Array Operators ---

func AContainedBy(left, right filter.ArrayExpression) *filter.ArrayComparison {
	return &filter.ArrayComparison{Name: filter.ArrayContainedBy, Left: left, Right: right}
}
func AContains(left, right filter.ArrayExpression) *filter.ArrayComparison {
	return &filter.ArrayComparison{Name: filter.ArrayContains, Left: left, Right: right}
}
func AEquals(left, right filter.ArrayExpression) *filter.ArrayComparison {
	return &filter.ArrayComparison{Name: filter.ArrayEquals, Left: left, Right: right}
}
func AOverlaps(left, right filter.ArrayExpression) *filter.ArrayComparison {
	return &filter.ArrayComparison{Name: filter.ArrayOverlaps, Left: left, Right: right}
}

// --- Functions ---

func Func(name string, args ...any) (*filter.Function, error) {
	expressions := make([]filter.Expression, len(args))
	for i, arg := range args {
		expr, err := toExpression(arg)
		if err != nil {
			return nil, err
		}
		expressions[i] = expr
	}
	return &filter.Function{
		Op:   name,
		Args: expressions,
	}, nil
}
