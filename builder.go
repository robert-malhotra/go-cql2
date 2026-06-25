package cql2

import (
	"encoding/json"
	"reflect"
	"slices"
	"strconv"
	"time"

	"github.com/exergy-dev/go-topology-suite/geom"
)

// Expr is the type returned by every builder helper. It wraps a Node so
// callers can pass Expr values around and unwrap the underlying AST when
// needed.
type Expr struct{ N Node }

// Node returns the wrapped AST node.
func (e Expr) Node() Node { return e.N }

// lift converts a Go value to an AST Node per the design §9.1 table.
// It panics with *UnliftableError on unsupported types.
func lift(v any) Node {
	switch x := v.(type) {
	case nil:
		return &NullLit{}
	case bool:
		return &BoolLit{Value: x}
	case int, int8, int16, int32, int64:
		return &NumLit{Value: json.Number(strconv.FormatInt(reflect.ValueOf(x).Int(), 10))}
	case uint, uint8, uint16, uint32, uint64:
		return &NumLit{Value: json.Number(strconv.FormatUint(reflect.ValueOf(x).Uint(), 10))}
	case float32:
		return &NumLit{Value: json.Number(strconv.FormatFloat(float64(x), 'g', -1, 64))}
	case float64:
		return &NumLit{Value: json.Number(strconv.FormatFloat(x, 'g', -1, 64))}
	case json.Number:
		return &NumLit{Value: x}
	case string:
		return &StringLit{Value: x}
	case time.Time:
		return &TimestampLit{Value: x}
	case geom.Geometry:
		return &GeomLit{Geom: x}
	case []any:
		return &ArrayLit{Elements: mapSlice(x, lift)}
	case []Expr:
		return &ArrayLit{Elements: mapSlice(x, exprNode)}
	case []Node:
		// Take a fresh slice so callers cannot mutate the AST after the fact.
		return &ArrayLit{Elements: slices.Clone(x)}
	case Expr:
		return x.N
	case Node:
		return x
	default:
		panic(&UnliftableError{Value: v})
	}
}

// TryLit lifts v to an Expr and returns any *UnliftableError as a regular
// error rather than panicking.
func TryLit(v any) (e Expr, err error) {
	defer func() {
		if r := recover(); r != nil {
			if ue, ok := r.(*UnliftableError); ok {
				err = ue
				return
			}
			panic(r)
		}
	}()
	return Expr{N: lift(v)}, nil
}

// Lit lifts v to an Expr (panics with *UnliftableError on unsupported types).
func Lit(v any) Expr { return Expr{N: lift(v)} }

// Bool returns a boolean literal expression.
func Bool(b bool) Expr { return Expr{N: &BoolLit{Value: b}} }

// Int returns an integer numeric literal expression.
func Int(i int64) Expr {
	return Expr{N: &NumLit{Value: json.Number(strconv.FormatInt(i, 10))}}
}

// Float returns a floating-point numeric literal expression.
func Float(f float64) Expr {
	return Expr{N: &NumLit{Value: json.Number(strconv.FormatFloat(f, 'g', -1, 64))}}
}

// Str returns a string literal expression.
func Str(s string) Expr { return Expr{N: &StringLit{Value: s}} }

// Time returns a timestamp literal expression.
func Time(t time.Time) Expr { return Expr{N: &TimestampLit{Value: t}} }

// Date returns a calendar-date literal expression at UTC midnight.
func Date(y int, m time.Month, d int) Expr {
	return Expr{N: &DateLit{Value: time.Date(y, m, d, 0, 0, 0, 0, time.UTC)}}
}

// Interval returns an interval literal expression. start and end may be a
// time.Time, a *DateLit / *TimestampLit, an Expr wrapping one of those, a
// *Unbounded, or the string ".." (which becomes an Unbounded endpoint).
// To create date endpoints (rather than timestamp), pass Date(y,m,d).
func Interval(start, end any) Expr {
	return Expr{N: &IntervalLit{
		Start: intervalEndpoint(start),
		End:   intervalEndpoint(end),
	}}
}

func intervalEndpoint(v any) Node {
	switch x := v.(type) {
	case string:
		if x == ".." {
			return &Unbounded{}
		}
		panic(&UnliftableError{Value: v, Msg: "interval string endpoint must be \"..\""})
	case time.Time:
		return &TimestampLit{Value: x}
	case Expr:
		return intervalEndpoint(x.N)
	case Node:
		if isValidIntervalEndpoint(x) {
			return x
		}
		panic(&UnliftableError{Value: v, Msg: "not a valid interval endpoint"})
	default:
		panic(&UnliftableError{Value: v, Msg: "not a valid interval endpoint"})
	}
}

// Geom returns a geometry literal expression.
func Geom(g geom.Geometry) Expr { return Expr{N: &GeomLit{Geom: g}} }

// exprNode unwraps an Expr to its underlying Node. Used with mapSlice.
func exprNode(e Expr) Node { return e.N }

// Array returns an array literal expression composed of the lifted elems.
func Array(elems ...any) Expr {
	return Expr{N: &ArrayLit{Elements: mapSlice(elems, lift)}}
}

// Property returns a property reference expression. Use this in value
// positions where a bare Go string would otherwise lift to a string literal.
func Property(name string) Expr { return Expr{N: &PropertyRef{Name: name}} }

// naryBool builds an n-ary logical op. With 0 args it panics; with 1 arg it
// returns that arg unchanged; with 2+ it builds an Op{op, ...}.
func naryBool(op Operator, name string, args []Expr) Expr {
	switch len(args) {
	case 0:
		panic("cql2: " + name + " requires at least one argument")
	case 1:
		return args[0]
	}
	return Expr{N: &Op{Op: op, Args: mapSlice(args, exprNode)}}
}

// And builds an n-ary conjunction. With 0 args it panics; with 1 arg it
// returns that arg unchanged; with 2+ it builds an Op{OpAnd, ...}.
func And(args ...Expr) Expr { return naryBool(OpAnd, "And", args) }

// Or builds an n-ary disjunction. With 0 args it panics; with 1 arg it
// returns that arg unchanged; with 2+ it builds an Op{OpOr, ...}.
func Or(args ...Expr) Expr { return naryBool(OpOr, "Or", args) }

// Not negates an expression.
func Not(e Expr) Expr {
	return Expr{N: &Op{Op: OpNot, Args: []Node{e.N}}}
}

func cmp(op Operator, prop string, value any) Expr {
	return Expr{N: &Op{Op: op, Args: []Node{&PropertyRef{Name: prop}, lift(value)}}}
}

// Eq builds prop = value.
func Eq(prop string, value any) Expr { return cmp(OpEq, prop, value) }

// Neq builds prop <> value.
func Neq(prop string, value any) Expr { return cmp(OpNeq, prop, value) }

// Lt builds prop < value.
func Lt(prop string, value any) Expr { return cmp(OpLt, prop, value) }

// Lte builds prop <= value.
func Lte(prop string, value any) Expr { return cmp(OpLte, prop, value) }

// Gt builds prop > value.
func Gt(prop string, value any) Expr { return cmp(OpGt, prop, value) }

// Gte builds prop >= value.
func Gte(prop string, value any) Expr { return cmp(OpGte, prop, value) }

// Like builds a SQL LIKE pattern match.
func Like(prop, pattern string) Expr {
	return Expr{N: &Op{Op: OpLike, Args: []Node{&PropertyRef{Name: prop}, &StringLit{Value: pattern}}}}
}

// ILike builds a case-insensitive LIKE pattern match modeled as
// LIKE(CASEI(prop), CASEI(pattern)).
func ILike(prop, pattern string) Expr {
	return Expr{N: &Op{Op: OpLike, Args: []Node{
		&Op{Op: OpCaseI, Args: []Node{&PropertyRef{Name: prop}}},
		&Op{Op: OpCaseI, Args: []Node{&StringLit{Value: pattern}}},
	}}}
}

// Between builds prop BETWEEN lo AND hi.
func Between(prop string, lo, hi any) Expr {
	return Expr{N: &Op{Op: OpBetween, Args: []Node{
		&PropertyRef{Name: prop},
		lift(lo),
		lift(hi),
	}}}
}

// In builds prop IN (values...).
func In(prop string, values ...any) Expr {
	return Expr{N: &Op{Op: OpIn, Args: []Node{
		&PropertyRef{Name: prop},
		&ArrayLit{Elements: mapSlice(values, lift)},
	}}}
}

// IsNull builds prop IS NULL.
func IsNull(prop string) Expr {
	return Expr{N: &Op{Op: OpIsNull, Args: []Node{&PropertyRef{Name: prop}}}}
}

// IsNotNull builds prop IS NOT NULL as Not(IsNull(prop)).
func IsNotNull(prop string) Expr { return Not(IsNull(prop)) }

func nary(op Operator, args []any) Expr {
	return Expr{N: &Op{Op: op, Args: mapSlice(args, lift)}}
}

// Add builds an n-ary addition.
func Add(args ...any) Expr { return nary(OpAdd, args) }

// Sub builds an n-ary subtraction.
func Sub(args ...any) Expr { return nary(OpSub, args) }

// Mul builds an n-ary multiplication.
func Mul(args ...any) Expr { return nary(OpMul, args) }

// Div builds an n-ary division.
func Div(args ...any) Expr { return nary(OpDiv, args) }

// Mod builds an n-ary modulus.
func Mod(args ...any) Expr { return nary(OpMod, args) }

// Pow builds an n-ary exponentiation.
func Pow(args ...any) Expr { return nary(OpPow, args) }

// IDiv builds an n-ary integer division.
func IDiv(args ...any) Expr { return nary(OpIDiv, args) }

// SIntersects builds a spatial-intersects predicate.
func SIntersects(prop string, value any) Expr { return cmp(OpSIntersects, prop, value) }

// SEquals builds a spatial-equals predicate.
func SEquals(prop string, value any) Expr { return cmp(OpSEquals, prop, value) }

// SDisjoint builds a spatial-disjoint predicate.
func SDisjoint(prop string, value any) Expr { return cmp(OpSDisjoint, prop, value) }

// STouches builds a spatial-touches predicate.
func STouches(prop string, value any) Expr { return cmp(OpSTouches, prop, value) }

// SWithin builds a spatial-within predicate.
func SWithin(prop string, value any) Expr { return cmp(OpSWithin, prop, value) }

// SOverlaps builds a spatial-overlaps predicate.
func SOverlaps(prop string, value any) Expr { return cmp(OpSOverlaps, prop, value) }

// SCrosses builds a spatial-crosses predicate.
func SCrosses(prop string, value any) Expr { return cmp(OpSCrosses, prop, value) }

// SContains builds a spatial-contains predicate.
func SContains(prop string, value any) Expr { return cmp(OpSContains, prop, value) }

// TAfter builds a temporal-after predicate.
func TAfter(prop string, value any) Expr { return cmp(OpTAfter, prop, value) }

// TBefore builds a temporal-before predicate.
func TBefore(prop string, value any) Expr { return cmp(OpTBefore, prop, value) }

// TContains builds a temporal-contains predicate.
func TContains(prop string, value any) Expr { return cmp(OpTContains, prop, value) }

// TDisjoint builds a temporal-disjoint predicate.
func TDisjoint(prop string, value any) Expr { return cmp(OpTDisjoint, prop, value) }

// TDuring builds a temporal-during predicate.
func TDuring(prop string, value any) Expr { return cmp(OpTDuring, prop, value) }

// TEquals builds a temporal-equals predicate.
func TEquals(prop string, value any) Expr { return cmp(OpTEquals, prop, value) }

// TFinishedBy builds a temporal-finishedby predicate.
func TFinishedBy(prop string, value any) Expr { return cmp(OpTFinishedBy, prop, value) }

// TFinishes builds a temporal-finishes predicate.
func TFinishes(prop string, value any) Expr { return cmp(OpTFinishes, prop, value) }

// TIntersects builds a temporal-intersects predicate.
func TIntersects(prop string, value any) Expr { return cmp(OpTIntersects, prop, value) }

// TMeets builds a temporal-meets predicate.
func TMeets(prop string, value any) Expr { return cmp(OpTMeets, prop, value) }

// TMetBy builds a temporal-metby predicate.
func TMetBy(prop string, value any) Expr { return cmp(OpTMetBy, prop, value) }

// TOverlappedBy builds a temporal-overlappedby predicate.
func TOverlappedBy(prop string, value any) Expr { return cmp(OpTOverlappedBy, prop, value) }

// TOverlaps builds a temporal-overlaps predicate.
func TOverlaps(prop string, value any) Expr { return cmp(OpTOverlaps, prop, value) }

// TStartedBy builds a temporal-startedby predicate.
func TStartedBy(prop string, value any) Expr { return cmp(OpTStartedBy, prop, value) }

// TStarts builds a temporal-starts predicate.
func TStarts(prop string, value any) Expr { return cmp(OpTStarts, prop, value) }

// AContains builds an array-contains predicate.
func AContains(prop string, value any) Expr { return cmp(OpAContains, prop, value) }

// AContainedBy builds an array-containedby predicate.
func AContainedBy(prop string, value any) Expr { return cmp(OpAContainedBy, prop, value) }

// AEquals builds an array-equals predicate.
func AEquals(prop string, value any) Expr { return cmp(OpAEquals, prop, value) }

// AOverlaps builds an array-overlaps predicate.
func AOverlaps(prop string, value any) Expr { return cmp(OpAOverlaps, prop, value) }

// Call builds a named function invocation. Each arg is lifted.
func Call(name string, args ...any) Expr {
	return Expr{N: &FunctionCall{Name: name, Args: mapSlice(args, lift)}}
}
