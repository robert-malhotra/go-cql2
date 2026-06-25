package cql2

import (
	"encoding/json"
	"errors"
	"reflect"
	"testing"
	"time"

	"github.com/exergy-dev/go-topology-suite/geom"
)

func TestBuilder_WorkedExample(t *testing.T) {
	got := Or(
		And(Gt("floors", 5), Eq("material", "brick")),
		Eq("swimming_pool", true),
	).N

	want := &Op{Op: OpOr, Args: []Node{
		&Op{Op: OpAnd, Args: []Node{
			&Op{Op: OpGt, Args: []Node{
				&PropertyRef{Name: "floors"},
				&NumLit{Value: json.Number("5")},
			}},
			&Op{Op: OpEq, Args: []Node{
				&PropertyRef{Name: "material"},
				&StringLit{Value: "brick"},
			}},
		}},
		&Op{Op: OpEq, Args: []Node{
			&PropertyRef{Name: "swimming_pool"},
			&BoolLit{Value: true},
		}},
	}}

	if !reflect.DeepEqual(got, want) {
		t.Fatalf("worked example mismatch\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_LiftTable(t *testing.T) {
	pt := geom.NewPoint(nil, geom.XY{X: 1, Y: 2})
	ts := time.Date(2024, 1, 2, 3, 4, 5, 0, time.UTC)

	tests := []struct {
		name string
		in   any
		want Node
	}{
		{"nil -> Null", nil, &NullLit{}},
		{"true", true, &BoolLit{Value: true}},
		{"false", false, &BoolLit{Value: false}},
		{"int", int(5), &NumLit{Value: json.Number("5")}},
		{"int8", int8(-3), &NumLit{Value: json.Number("-3")}},
		{"int16", int16(7), &NumLit{Value: json.Number("7")}},
		{"int32", int32(-9), &NumLit{Value: json.Number("-9")}},
		{"int64", int64(123456789012), &NumLit{Value: json.Number("123456789012")}},
		{"uint", uint(7), &NumLit{Value: json.Number("7")}},
		{"uint8", uint8(8), &NumLit{Value: json.Number("8")}},
		{"uint16", uint16(9), &NumLit{Value: json.Number("9")}},
		{"uint32", uint32(10), &NumLit{Value: json.Number("10")}},
		{"uint64", uint64(11), &NumLit{Value: json.Number("11")}},
		{"float32", float32(1.5), &NumLit{Value: json.Number("1.5")}},
		{"float64", float64(3.25), &NumLit{Value: json.Number("3.25")}},
		{"json.Number preserved", json.Number("1.230"), &NumLit{Value: json.Number("1.230")}},
		{"string", "hello", &StringLit{Value: "hello"}},
		{"time.Time", ts, &TimestampLit{Value: ts}},
		{"Geometry", geom.Geometry(pt), &GeomLit{Geom: pt}},
		{"[]any", []any{1, "x"}, &ArrayLit{Elements: []Node{
			&NumLit{Value: json.Number("1")},
			&StringLit{Value: "x"},
		}}},
		{"[]Expr", []Expr{Int(1), Str("x")}, &ArrayLit{Elements: []Node{
			&NumLit{Value: json.Number("1")},
			&StringLit{Value: "x"},
		}}},
		{"[]Node", []Node{&BoolLit{Value: true}}, &ArrayLit{Elements: []Node{
			&BoolLit{Value: true},
		}}},
		{"Expr unwraps", Expr{N: &BoolLit{Value: true}}, &BoolLit{Value: true}},
		{"Node passthrough", Node(&NullLit{}), &NullLit{}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := lift(tc.in)
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("lift(%v): got %#v, want %#v", tc.in, got, tc.want)
			}
			if got.Kind() != tc.want.Kind() {
				t.Fatalf("Kind mismatch: got %v want %v", got.Kind(), tc.want.Kind())
			}
		})
	}
}

func TestBuilder_LiftIntFormat(t *testing.T) {
	// int(5) -> "5", not "5.0"
	got := lift(int(5)).(*NumLit).Value
	if got != json.Number("5") {
		t.Fatalf("int(5) -> %q, want \"5\"", got)
	}
	// float64(5.0) -> "5" (FormatFloat 'g' with -1 strips trailing zero)
	got = lift(5.0).(*NumLit).Value
	if got != json.Number("5") {
		t.Fatalf("float64(5.0) -> %q, want \"5\" (FormatFloat 'g' -1 drops trailing zero)", got)
	}
	// json.Number preserves original lexeme
	got = lift(json.Number("5.0")).(*NumLit).Value
	if got != json.Number("5.0") {
		t.Fatalf("json.Number(\"5.0\") -> %q, want \"5.0\"", got)
	}
}

func TestBuilder_LiftUnsupported(t *testing.T) {
	type weird struct{ X int }
	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected panic on unliftable type")
		}
		ue, ok := r.(*UnliftableError)
		if !ok {
			t.Fatalf("got panic %v (%T), want *UnliftableError", r, r)
		}
		if ue.Error() == "" {
			t.Fatal("UnliftableError has empty message")
		}
	}()
	_ = lift(weird{X: 1})
}

func TestBuilder_TryLit(t *testing.T) {
	type weird struct{ X int }
	e, err := TryLit(weird{X: 2})
	if err == nil {
		t.Fatal("expected error from TryLit on unliftable type")
	}
	var ue *UnliftableError
	if !errors.As(err, &ue) {
		t.Fatalf("TryLit returned %T, want *UnliftableError", err)
	}
	if e.N != nil {
		t.Fatalf("expected zero Expr on error, got %#v", e)
	}

	e, err = TryLit(42)
	if err != nil {
		t.Fatalf("TryLit(42) errored: %v", err)
	}
	if !reflect.DeepEqual(e.N, &NumLit{Value: json.Number("42")}) {
		t.Fatalf("TryLit(42) -> %#v", e.N)
	}

	// Same input via Lit panics.
	defer func() {
		if r := recover(); r == nil {
			t.Fatal("expected Lit to panic on unliftable type")
		}
	}()
	_ = Lit(weird{X: 3})
}

func TestBuilder_In(t *testing.T) {
	got := In("status", "a", "b", "c").N
	want := &Op{Op: OpIn, Args: []Node{
		&PropertyRef{Name: "status"},
		&ArrayLit{Elements: []Node{
			&StringLit{Value: "a"},
			&StringLit{Value: "b"},
			&StringLit{Value: "c"},
		}},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("In mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_Between(t *testing.T) {
	got := Between("year", 1900, 2000).N
	want := &Op{Op: OpBetween, Args: []Node{
		&PropertyRef{Name: "year"},
		&NumLit{Value: json.Number("1900")},
		&NumLit{Value: json.Number("2000")},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Between mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_IsNotNull(t *testing.T) {
	got := IsNotNull("x").N
	want := &Op{Op: OpNot, Args: []Node{
		&Op{Op: OpIsNull, Args: []Node{&PropertyRef{Name: "x"}}},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("IsNotNull mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_Not(t *testing.T) {
	got := Not(Eq("a", 1)).N
	want := &Op{Op: OpNot, Args: []Node{
		&Op{Op: OpEq, Args: []Node{
			&PropertyRef{Name: "a"},
			&NumLit{Value: json.Number("1")},
		}},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Not mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_PropertyInValuePosition(t *testing.T) {
	got := Gt("a", Property("b")).N
	want := &Op{Op: OpGt, Args: []Node{
		&PropertyRef{Name: "a"},
		&PropertyRef{Name: "b"},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Property in value position mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_Call(t *testing.T) {
	got := Call("ABS", Property("delta")).N
	fc, ok := got.(*FunctionCall)
	if !ok {
		t.Fatalf("Call did not return *FunctionCall, got %T", got)
	}
	if fc.Name != "ABS" {
		t.Fatalf("name = %q", fc.Name)
	}
	if len(fc.Args) != 1 {
		t.Fatalf("len(Args) = %d", len(fc.Args))
	}
	if !reflect.DeepEqual(fc.Args[0], &PropertyRef{Name: "delta"}) {
		t.Fatalf("arg[0] = %#v", fc.Args[0])
	}
}

func TestBuilder_AddMixed(t *testing.T) {
	got := Add(Property("base"), 100).N
	want := &Op{Op: OpAdd, Args: []Node{
		&PropertyRef{Name: "base"},
		&NumLit{Value: json.Number("100")},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Add(Property,int) mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_AndArity(t *testing.T) {
	// 0 args panics.
	func() {
		defer func() {
			if r := recover(); r == nil {
				t.Fatal("And() with 0 args did not panic")
			}
		}()
		_ = And()
	}()
	// 1 arg returns the arg unchanged.
	x := Eq("a", 1)
	if got := And(x); !reflect.DeepEqual(got, x) {
		t.Fatalf("And(x) = %#v, want %#v", got, x)
	}
	// 2+ args build n-ary Op.
	y := Eq("b", 2)
	got := And(x, y).N
	want := &Op{Op: OpAnd, Args: []Node{x.N, y.N}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("And(x,y) mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_OrArity(t *testing.T) {
	func() {
		defer func() {
			if r := recover(); r == nil {
				t.Fatal("Or() with 0 args did not panic")
			}
		}()
		_ = Or()
	}()
	x := Eq("a", 1)
	if got := Or(x); !reflect.DeepEqual(got, x) {
		t.Fatalf("Or(x) = %#v, want %#v", got, x)
	}
}

func TestBuilder_ILike(t *testing.T) {
	got := ILike("name", "%Smith%").N
	want := &Op{Op: OpLike, Args: []Node{
		&Op{Op: OpCaseI, Args: []Node{&PropertyRef{Name: "name"}}},
		&Op{Op: OpCaseI, Args: []Node{&StringLit{Value: "%Smith%"}}},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("ILike mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_Like(t *testing.T) {
	got := Like("name", "Smith%").N
	want := &Op{Op: OpLike, Args: []Node{
		&PropertyRef{Name: "name"},
		&StringLit{Value: "Smith%"},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Like mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_IsNull(t *testing.T) {
	got := IsNull("x").N
	want := &Op{Op: OpIsNull, Args: []Node{&PropertyRef{Name: "x"}}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("IsNull mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_Date(t *testing.T) {
	got := Date(2024, time.March, 15).N
	want := &DateLit{Value: time.Date(2024, time.March, 15, 0, 0, 0, 0, time.UTC)}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Date mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_Interval(t *testing.T) {
	t1 := time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
	got := Interval(t1, "..").N
	il, ok := got.(*IntervalLit)
	if !ok {
		t.Fatalf("Interval did not return *IntervalLit, got %T", got)
	}
	if !reflect.DeepEqual(il.Start, &TimestampLit{Value: t1}) {
		t.Fatalf("Start = %#v", il.Start)
	}
	if !reflect.DeepEqual(il.End, &Unbounded{}) {
		t.Fatalf("End = %#v", il.End)
	}

	// Date endpoints
	got2 := Interval(Date(2024, 1, 1).N, Date(2024, 12, 31).N).N
	il2 := got2.(*IntervalLit)
	if _, ok := il2.Start.(*DateLit); !ok {
		t.Fatalf("Start = %T, want *DateLit", il2.Start)
	}
	if _, ok := il2.End.(*DateLit); !ok {
		t.Fatalf("End = %T, want *DateLit", il2.End)
	}
}

func TestBuilder_Array(t *testing.T) {
	got := Array(1, "x", true).N
	want := &ArrayLit{Elements: []Node{
		&NumLit{Value: json.Number("1")},
		&StringLit{Value: "x"},
		&BoolLit{Value: true},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Array mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_SpatialPredicates(t *testing.T) {
	pt := geom.NewPoint(nil, geom.XY{X: 1, Y: 2})
	got := SIntersects("geom", pt).N
	want := &Op{Op: OpSIntersects, Args: []Node{
		&PropertyRef{Name: "geom"},
		&GeomLit{Geom: pt},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("SIntersects mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_TemporalPredicate(t *testing.T) {
	ts := time.Date(2024, 6, 1, 12, 0, 0, 0, time.UTC)
	got := TAfter("when", ts).N
	want := &Op{Op: OpTAfter, Args: []Node{
		&PropertyRef{Name: "when"},
		&TimestampLit{Value: ts},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("TAfter mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_ArrayPredicate(t *testing.T) {
	got := AContains("tags", []any{"a", "b"}).N
	want := &Op{Op: OpAContains, Args: []Node{
		&PropertyRef{Name: "tags"},
		&ArrayLit{Elements: []Node{
			&StringLit{Value: "a"},
			&StringLit{Value: "b"},
		}},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("AContains mismatch:\n got: %#v\nwant: %#v", got, want)
	}
}

func TestBuilder_ExprNodeAccessor(t *testing.T) {
	e := Bool(true)
	if e.Node() != e.N {
		t.Fatalf("Expr.Node() did not return e.N")
	}
}

func TestBuilder_LitConstructors(t *testing.T) {
	if !reflect.DeepEqual(Bool(true).N, &BoolLit{Value: true}) {
		t.Fatal("Bool")
	}
	if !reflect.DeepEqual(Int(42).N, &NumLit{Value: json.Number("42")}) {
		t.Fatal("Int")
	}
	if !reflect.DeepEqual(Float(1.5).N, &NumLit{Value: json.Number("1.5")}) {
		t.Fatal("Float")
	}
	if !reflect.DeepEqual(Str("hi").N, &StringLit{Value: "hi"}) {
		t.Fatal("Str")
	}
	ts := time.Now()
	if !reflect.DeepEqual(Time(ts).N, &TimestampLit{Value: ts}) {
		t.Fatal("Time")
	}
	pt := geom.NewEmptyPoint(nil, geom.LayoutXY)
	if !reflect.DeepEqual(Geom(pt).N, &GeomLit{Geom: pt}) {
		t.Fatal("Geom")
	}
}
