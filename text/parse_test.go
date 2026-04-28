package text

import (
	"encoding/json"
	"errors"
	"strings"
	"testing"
	"time"

	cql2 "github.com/example/go-cql2"
)

func mustParse(t *testing.T, src string) cql2.Node {
	t.Helper()
	n, err := Parse(src)
	if err != nil {
		t.Fatalf("Parse(%q) returned error: %v", src, err)
	}
	if n == nil {
		t.Fatalf("Parse(%q) returned nil node", src)
	}
	return n
}

func TestParse_Section1Example(t *testing.T) {
	src := `(floors > 5 AND material = 'brick') OR swimming_pool = true`
	n := mustParse(t, src)
	want := &cql2.Op{
		Op: cql2.OpOr,
		Args: []cql2.Node{
			&cql2.Op{Op: cql2.OpAnd, Args: []cql2.Node{
				&cql2.Op{Op: cql2.OpGt, Args: []cql2.Node{
					&cql2.PropertyRef{Name: "floors"}, &cql2.NumLit{Value: json.Number("5")},
				}},
				&cql2.Op{Op: cql2.OpEq, Args: []cql2.Node{
					&cql2.PropertyRef{Name: "material"}, &cql2.StringLit{Value: "brick"},
				}},
			}},
			&cql2.Op{Op: cql2.OpEq, Args: []cql2.Node{
				&cql2.PropertyRef{Name: "swimming_pool"}, &cql2.BoolLit{Value: true},
			}},
		},
	}
	if !cql2.Equal(n, want) {
		t.Fatalf("AST mismatch\n got: %#v\nwant: %#v", n, want)
	}
	// Round-trip.
	encoded, err := Encode(n)
	if err != nil {
		t.Fatalf("Encode error: %v", err)
	}
	n2 := mustParse(t, encoded)
	if !cql2.Equal(n, n2) {
		t.Fatalf("round-trip mismatch.\nencoded: %s\ngot AST: %#v", encoded, n2)
	}
}

func TestParse_AppendixAExample(t *testing.T) {
	src := `landsat:scene_id LIKE 'LC8%' AND eo:cloud_cover < 0.1 AND ` +
		`S_INTERSECTS(geometry, POLYGON((-77.5 38.7, -77.4 38.7, -77.4 38.8, -77.5 38.8, -77.5 38.7))) AND ` +
		`T_AFTER(datetime, TIMESTAMP('2021-04-08T04:39:23Z'))`
	n := mustParse(t, src)
	encoded, err := Encode(n)
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	n2 := mustParse(t, encoded)
	if !cql2.Equal(n, n2) {
		t.Fatalf("round-trip mismatch.\nencoded: %s", encoded)
	}
	// Spot-check a polygon coord survived.
	if !strings.Contains(encoded, "-77.5") {
		t.Errorf("expected polygon coordinate in encoded form, got: %s", encoded)
	}
	if !strings.Contains(encoded, "TIMESTAMP('2021-04-08T04:39:23Z')") {
		t.Errorf("expected timestamp in encoded form, got: %s", encoded)
	}
}

func TestParse_DesugarBetween(t *testing.T) {
	n := mustParse(t, `a BETWEEN 1 AND 10`)
	want := &cql2.Op{Op: cql2.OpBetween, Args: []cql2.Node{
		&cql2.PropertyRef{Name: "a"},
		&cql2.NumLit{Value: json.Number("1")},
		&cql2.NumLit{Value: json.Number("10")},
	}}
	if !cql2.Equal(n, want) {
		t.Fatalf("got %#v\nwant %#v", n, want)
	}
	roundTrip(t, n)
}

func TestParse_DesugarNotBetween(t *testing.T) {
	n := mustParse(t, `a NOT BETWEEN 1 AND 10`)
	op, ok := n.(*cql2.Op)
	if !ok || op.Op != cql2.OpNot {
		t.Fatalf("expected outer NOT, got %#v", n)
	}
	inner, ok := op.Args[0].(*cql2.Op)
	if !ok || inner.Op != cql2.OpBetween {
		t.Fatalf("expected inner BETWEEN, got %#v", op.Args[0])
	}
	roundTrip(t, n)
}

func TestParse_DesugarIn(t *testing.T) {
	n := mustParse(t, `a IN (1, 2, 3)`)
	want := &cql2.Op{Op: cql2.OpIn, Args: []cql2.Node{
		&cql2.PropertyRef{Name: "a"},
		&cql2.ArrayLit{Elements: []cql2.Node{
			&cql2.NumLit{Value: json.Number("1")},
			&cql2.NumLit{Value: json.Number("2")},
			&cql2.NumLit{Value: json.Number("3")},
		}},
	}}
	if !cql2.Equal(n, want) {
		t.Fatalf("got %#v\nwant %#v", n, want)
	}
	roundTrip(t, n)
}

func TestParse_DesugarNotIn(t *testing.T) {
	n := mustParse(t, `a NOT IN ('x', 'y')`)
	op, ok := n.(*cql2.Op)
	if !ok || op.Op != cql2.OpNot {
		t.Fatalf("expected outer NOT, got %#v", n)
	}
	roundTrip(t, n)
}

func TestParse_DesugarIsNull(t *testing.T) {
	n := mustParse(t, `a IS NULL`)
	want := &cql2.Op{Op: cql2.OpIsNull, Args: []cql2.Node{&cql2.PropertyRef{Name: "a"}}}
	if !cql2.Equal(n, want) {
		t.Fatalf("got %#v want %#v", n, want)
	}
	roundTrip(t, n)
}

func TestParse_DesugarIsNotNull(t *testing.T) {
	n := mustParse(t, `a IS NOT NULL`)
	op, ok := n.(*cql2.Op)
	if !ok || op.Op != cql2.OpNot {
		t.Fatalf("expected outer NOT, got %#v", n)
	}
	inner, ok := op.Args[0].(*cql2.Op)
	if !ok || inner.Op != cql2.OpIsNull {
		t.Fatalf("expected inner IS NULL, got %#v", op.Args[0])
	}
	roundTrip(t, n)
}

func TestParse_DesugarLike(t *testing.T) {
	n := mustParse(t, `name LIKE 'foo%'`)
	want := &cql2.Op{Op: cql2.OpLike, Args: []cql2.Node{
		&cql2.PropertyRef{Name: "name"},
		&cql2.StringLit{Value: "foo%"},
	}}
	if !cql2.Equal(n, want) {
		t.Fatalf("got %#v\nwant %#v", n, want)
	}
	roundTrip(t, n)
}

func TestParse_DesugarNotLike(t *testing.T) {
	n := mustParse(t, `name NOT LIKE 'foo%'`)
	op, ok := n.(*cql2.Op)
	if !ok || op.Op != cql2.OpNot {
		t.Fatalf("expected outer NOT, got %#v", n)
	}
	roundTrip(t, n)
}

func TestParse_CaseI(t *testing.T) {
	n := mustParse(t, `CASEI(a) = CASEI(b)`)
	want := &cql2.Op{Op: cql2.OpEq, Args: []cql2.Node{
		&cql2.Op{Op: cql2.OpCaseI, Args: []cql2.Node{&cql2.PropertyRef{Name: "a"}}},
		&cql2.Op{Op: cql2.OpCaseI, Args: []cql2.Node{&cql2.PropertyRef{Name: "b"}}},
	}}
	if !cql2.Equal(n, want) {
		t.Fatalf("got %#v\nwant %#v", n, want)
	}
	roundTrip(t, n)
}

func TestParse_AccentI(t *testing.T) {
	n := mustParse(t, `ACCENTI(a) = ACCENTI(b)`)
	want := &cql2.Op{Op: cql2.OpEq, Args: []cql2.Node{
		&cql2.Op{Op: cql2.OpAccentI, Args: []cql2.Node{&cql2.PropertyRef{Name: "a"}}},
		&cql2.Op{Op: cql2.OpAccentI, Args: []cql2.Node{&cql2.PropertyRef{Name: "b"}}},
	}}
	if !cql2.Equal(n, want) {
		t.Fatalf("got %#v\nwant %#v", n, want)
	}
	roundTrip(t, n)
}

func TestParse_ArithmeticPrecedence(t *testing.T) {
	n := mustParse(t, `a + b * c`)
	want := &cql2.Op{Op: cql2.OpAdd, Args: []cql2.Node{
		&cql2.PropertyRef{Name: "a"},
		&cql2.Op{Op: cql2.OpMul, Args: []cql2.Node{
			&cql2.PropertyRef{Name: "b"}, &cql2.PropertyRef{Name: "c"},
		}},
	}}
	if !cql2.Equal(n, want) {
		t.Fatalf("got %#v\nwant %#v", n, want)
	}
	encoded, err := Encode(n)
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	if strings.Contains(encoded, "(") {
		t.Errorf("expected no parens for natural-precedence form, got %q", encoded)
	}

	n2 := mustParse(t, `(a + b) * c`)
	encoded2, err := Encode(n2)
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	if !strings.Contains(encoded2, "(") {
		t.Errorf("expected parens for grouped form, got %q", encoded2)
	}
	// Round-trip preserves semantics.
	n3 := mustParse(t, encoded2)
	if !cql2.Equal(n2, n3) {
		t.Fatalf("round-trip mismatch for grouped form: %s", encoded2)
	}
}

func TestParse_PropertyNameForms(t *testing.T) {
	cases := []struct {
		src  string
		want string
	}{
		{`abc`, "abc"},
		{`a.b.c`, "a.b.c"},
		{`eo:cloud_cover`, "eo:cloud_cover"},
		{`"property with space"`, "property with space"},
		{`"with""quote"`, `with"quote`},
	}
	for _, tc := range cases {
		t.Run(tc.src, func(t *testing.T) {
			n := mustParse(t, tc.src)
			pr, ok := n.(*cql2.PropertyRef)
			if !ok {
				t.Fatalf("expected PropertyRef, got %T", n)
			}
			if pr.Name != tc.want {
				t.Fatalf("got name %q want %q", pr.Name, tc.want)
			}
		})
	}
}

func TestParse_StringEscape(t *testing.T) {
	n := mustParse(t, `'don''t'`)
	s, ok := n.(*cql2.StringLit)
	if !ok {
		t.Fatalf("expected StringLit, got %T", n)
	}
	if s.Value != "don't" {
		t.Fatalf("got %q want %q", s.Value, "don't")
	}
	encoded, err := Encode(s)
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	if encoded != `'don''t'` {
		t.Fatalf("got %q want %q", encoded, `'don''t'`)
	}
}

func TestParse_TimestampDateInterval(t *testing.T) {
	n := mustParse(t, `TIMESTAMP('2020-01-01T12:30:00Z')`)
	tl, ok := n.(*cql2.TimestampLit)
	if !ok {
		t.Fatalf("expected TimestampLit, got %T", n)
	}
	if !tl.Value.Equal(time.Date(2020, 1, 1, 12, 30, 0, 0, time.UTC)) {
		t.Fatalf("unexpected timestamp value: %v", tl.Value)
	}

	n = mustParse(t, `DATE('2020-01-01')`)
	if _, ok := n.(*cql2.DateLit); !ok {
		t.Fatalf("expected DateLit, got %T", n)
	}

	n = mustParse(t, `INTERVAL('2020-01-01','2020-12-31')`)
	il, ok := n.(*cql2.IntervalLit)
	if !ok {
		t.Fatalf("expected IntervalLit, got %T", n)
	}
	if _, ok := il.Start.(*cql2.DateLit); !ok {
		t.Errorf("expected DateLit start, got %T", il.Start)
	}

	n = mustParse(t, `INTERVAL('..','2020-12-31')`)
	il, ok = n.(*cql2.IntervalLit)
	if !ok {
		t.Fatalf("expected IntervalLit, got %T", n)
	}
	if _, ok := il.Start.(*cql2.Unbounded); !ok {
		t.Errorf("expected Unbounded start, got %T", il.Start)
	}
	roundTrip(t, n)

	n = mustParse(t, `INTERVAL('2020-01-01T00:00:00Z','..')`)
	il, ok = n.(*cql2.IntervalLit)
	if !ok {
		t.Fatalf("expected IntervalLit, got %T", n)
	}
	if _, ok := il.Start.(*cql2.TimestampLit); !ok {
		t.Errorf("expected TimestampLit start, got %T", il.Start)
	}
	if _, ok := il.End.(*cql2.Unbounded); !ok {
		t.Errorf("expected Unbounded end, got %T", il.End)
	}
}

func TestParse_GeometryPoint(t *testing.T) {
	n := mustParse(t, `S_INTERSECTS(g, POINT(1 2))`)
	op, ok := n.(*cql2.Op)
	if !ok || op.Op != cql2.OpSIntersects {
		t.Fatalf("expected S_INTERSECTS op, got %#v", n)
	}
	if _, ok := op.Args[1].(*cql2.GeomLit); !ok {
		t.Fatalf("expected geometry arg, got %T", op.Args[1])
	}
	roundTrip(t, n)
}

func TestParse_GeometryPolygon(t *testing.T) {
	n := mustParse(t, `S_INTERSECTS(g, POLYGON((0 0,1 0,1 1,0 1,0 0)))`)
	op, ok := n.(*cql2.Op)
	if !ok || op.Op != cql2.OpSIntersects {
		t.Fatalf("expected S_INTERSECTS op, got %#v", n)
	}
	if _, ok := op.Args[1].(*cql2.GeomLit); !ok {
		t.Fatalf("expected geometry arg, got %T", op.Args[1])
	}
	roundTrip(t, n)
}

func TestParse_BBox(t *testing.T) {
	n := mustParse(t, `BBOX(-180,-90,180,90)`)
	bb, ok := n.(*cql2.BBoxLit)
	if !ok {
		t.Fatalf("expected BBoxLit, got %T", n)
	}
	if len(bb.Coords) != 4 {
		t.Fatalf("expected 4 coords, got %d", len(bb.Coords))
	}
	roundTrip(t, n)
}

func TestParse_FunctionCall(t *testing.T) {
	n := mustParse(t, `myFunc(a, 5, 'x')`)
	fc, ok := n.(*cql2.FunctionCall)
	if !ok {
		t.Fatalf("expected FunctionCall, got %T", n)
	}
	if fc.Name != "myFunc" {
		t.Fatalf("expected name myFunc, got %q", fc.Name)
	}
	if len(fc.Args) != 3 {
		t.Fatalf("expected 3 args, got %d", len(fc.Args))
	}
	roundTrip(t, n)
}

func TestParse_ErrorUnclosedParen(t *testing.T) {
	_, err := Parse(`(a + b`)
	if err == nil {
		t.Fatal("expected error")
	}
	var se *cql2.SyntaxError
	if !errors.As(err, &se) {
		t.Fatalf("expected *cql2.SyntaxError, got %T: %v", err, err)
	}
	if se.Encoding != cql2.EncodingText {
		t.Errorf("expected EncodingText, got %v", se.Encoding)
	}
	if se.At.Line == 0 || se.At.Column == 0 {
		t.Errorf("expected populated position, got %#v", se.At)
	}
}

func TestParse_ErrorUnknownTokenAfterAnd(t *testing.T) {
	_, err := Parse(`a = 1 AND @`)
	if err == nil {
		t.Fatal("expected error")
	}
	var se *cql2.SyntaxError
	if !errors.As(err, &se) {
		t.Fatalf("expected *cql2.SyntaxError, got %T: %v", err, err)
	}
	if se.Encoding != cql2.EncodingText {
		t.Errorf("expected EncodingText, got %v", se.Encoding)
	}
	if se.At.Line == 0 {
		t.Errorf("expected populated position, got %#v", se.At)
	}
}

func TestParse_ErrorMalformedTimestamp(t *testing.T) {
	_, err := Parse(`TIMESTAMP('not-a-time')`)
	if err == nil {
		t.Fatal("expected error")
	}
	var se *cql2.SyntaxError
	if !errors.As(err, &se) {
		t.Fatalf("expected *cql2.SyntaxError, got %T: %v", err, err)
	}
	if se.Encoding != cql2.EncodingText {
		t.Errorf("expected EncodingText, got %v", se.Encoding)
	}
}

func TestParse_LowercaseKeywords(t *testing.T) {
	// CQL2 keywords are case-insensitive.
	n := mustParse(t, `a = 1 and b = 2 or c is null`)
	if n == nil {
		t.Fatal("nil result")
	}
	roundTrip(t, n)
}

// roundTrip ensures Encode then Parse produces an equal AST.
func roundTrip(t *testing.T, n cql2.Node) {
	t.Helper()
	encoded, err := Encode(n)
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	n2, err := Parse(encoded)
	if err != nil {
		t.Fatalf("re-Parse(%q): %v", encoded, err)
	}
	if !cql2.Equal(n, n2) {
		t.Fatalf("round-trip mismatch.\nencoded: %s\nbefore: %#v\nafter:  %#v", encoded, n, n2)
	}
}
