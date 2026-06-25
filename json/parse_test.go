package json_test

import (
	stdjson "encoding/json"
	"errors"
	"strings"
	"testing"
	"time"

	cql2 "github.com/exergy-dev/go-cql2"
	cqljson "github.com/exergy-dev/go-cql2/json"
	"github.com/exergy-dev/go-topology-suite/geom"
)

func TestParseBool(t *testing.T) {
	for _, tc := range []struct {
		in   string
		want bool
	}{{"true", true}, {"false", false}} {
		n, err := cqljson.Parse([]byte(tc.in))
		if err != nil {
			t.Fatalf("%s: %v", tc.in, err)
		}
		bl, ok := n.(*cql2.BoolLit)
		if !ok || bl.Value != tc.want {
			t.Fatalf("%s: got %#v want BoolLit(%v)", tc.in, n, tc.want)
		}
	}
}

func TestParseNull(t *testing.T) {
	n, err := cqljson.Parse([]byte("null"))
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := n.(*cql2.NullLit); !ok {
		t.Fatalf("got %#v", n)
	}
}

func TestParseString(t *testing.T) {
	n, err := cqljson.Parse([]byte(`"hello\nworld"`))
	if err != nil {
		t.Fatal(err)
	}
	s, ok := n.(*cql2.StringLit)
	if !ok || s.Value != "hello\nworld" {
		t.Fatalf("got %#v", n)
	}
}

func TestParseNumberPreservation(t *testing.T) {
	for _, in := range []string{"5", "1.5", "1e10", "-3.14", "0", "1.0"} {
		n, err := cqljson.Parse([]byte(in))
		if err != nil {
			t.Fatalf("%s: %v", in, err)
		}
		nl, ok := n.(*cql2.NumLit)
		if !ok {
			t.Fatalf("%s: got %#v", in, n)
		}
		if string(nl.Value) != in {
			t.Fatalf("%s preservation failed: got %q", in, string(nl.Value))
		}
	}
}

func TestParseArray(t *testing.T) {
	n, err := cqljson.Parse([]byte(`[1,"x",null,true]`))
	if err != nil {
		t.Fatal(err)
	}
	a, ok := n.(*cql2.ArrayLit)
	if !ok {
		t.Fatalf("got %T", n)
	}
	if len(a.Elements) != 4 {
		t.Fatalf("len=%d", len(a.Elements))
	}
}

func TestParseProperty(t *testing.T) {
	n, err := cqljson.Parse([]byte(`{"property":"city"}`))
	if err != nil {
		t.Fatal(err)
	}
	p, ok := n.(*cql2.PropertyRef)
	if !ok || p.Name != "city" {
		t.Fatalf("got %#v", n)
	}
}

func TestParseTimestamp(t *testing.T) {
	n, err := cqljson.Parse([]byte(`{"timestamp":"2020-01-02T03:04:05Z"}`))
	if err != nil {
		t.Fatal(err)
	}
	ts, ok := n.(*cql2.TimestampLit)
	if !ok {
		t.Fatalf("got %T", n)
	}
	want := time.Date(2020, 1, 2, 3, 4, 5, 0, time.UTC)
	if !ts.Value.Equal(want) {
		t.Fatalf("got %v want %v", ts.Value, want)
	}
}

func TestParseDate(t *testing.T) {
	n, err := cqljson.Parse([]byte(`{"date":"2020-06-15"}`))
	if err != nil {
		t.Fatal(err)
	}
	d, ok := n.(*cql2.DateLit)
	if !ok {
		t.Fatalf("got %T", n)
	}
	want := time.Date(2020, 6, 15, 0, 0, 0, 0, time.UTC)
	if !d.Value.Equal(want) {
		t.Fatalf("got %v want %v", d.Value, want)
	}
}

func TestParseInterval(t *testing.T) {
	n, err := cqljson.Parse([]byte(`{"interval":["2020-01-01","2020-12-31"]}`))
	if err != nil {
		t.Fatal(err)
	}
	iv, ok := n.(*cql2.IntervalLit)
	if !ok {
		t.Fatalf("got %T", n)
	}
	if _, ok := iv.Start.(*cql2.DateLit); !ok {
		t.Fatalf("start: %T", iv.Start)
	}
	if _, ok := iv.End.(*cql2.DateLit); !ok {
		t.Fatalf("end: %T", iv.End)
	}
}

func TestParseIntervalUnbounded(t *testing.T) {
	n, err := cqljson.Parse([]byte(`{"interval":["..","2020-12-31T00:00:00Z"]}`))
	if err != nil {
		t.Fatal(err)
	}
	iv := n.(*cql2.IntervalLit)
	if _, ok := iv.Start.(*cql2.Unbounded); !ok {
		t.Fatalf("start: %T", iv.Start)
	}
	if _, ok := iv.End.(*cql2.TimestampLit); !ok {
		t.Fatalf("end: %T", iv.End)
	}
}

func TestParseBBox4(t *testing.T) {
	n, err := cqljson.Parse([]byte(`{"bbox":[1,2,3,4]}`))
	if err != nil {
		t.Fatal(err)
	}
	bb := n.(*cql2.BBoxLit)
	if len(bb.Coords) != 4 {
		t.Fatalf("len=%d", len(bb.Coords))
	}
}

func TestParseBBox6(t *testing.T) {
	n, err := cqljson.Parse([]byte(`{"bbox":[1,2,3,4,5,6]}`))
	if err != nil {
		t.Fatal(err)
	}
	bb := n.(*cql2.BBoxLit)
	if len(bb.Coords) != 6 {
		t.Fatalf("len=%d", len(bb.Coords))
	}
}

func TestParseGeometryDelegation(t *testing.T) {
	in := `{"type":"Point","coordinates":[1,2]}`
	n, err := cqljson.Parse([]byte(in))
	if err != nil {
		t.Fatal(err)
	}
	gl, ok := n.(*cql2.GeomLit)
	if !ok {
		t.Fatalf("got %T", n)
	}
	if _, ok := gl.Geom.(*geom.Point); !ok {
		t.Fatalf("geom: %T", gl.Geom)
	}
	// Round-trip through Encode and re-Parse.
	out, err := cqljson.Encode(gl)
	if err != nil {
		t.Fatal(err)
	}
	if string(out) != in {
		t.Fatalf("encode mismatch: %q vs %q", out, in)
	}
}

func TestParseOp(t *testing.T) {
	n, err := cqljson.Parse([]byte(`{"op":"=","args":[{"property":"x"},5]}`))
	if err != nil {
		t.Fatal(err)
	}
	o, ok := n.(*cql2.Op)
	if !ok || o.Op != cql2.OpEq || len(o.Args) != 2 {
		t.Fatalf("got %#v", n)
	}
}

func TestParseFunction(t *testing.T) {
	n, err := cqljson.Parse([]byte(`{"function":{"name":"abs","args":[{"property":"x"}]}}`))
	if err != nil {
		t.Fatal(err)
	}
	fc, ok := n.(*cql2.FunctionCall)
	if !ok || fc.Name != "abs" || len(fc.Args) != 1 {
		t.Fatalf("got %#v", n)
	}
}

// Error cases.

func assertSyntaxJSON(t *testing.T, err error, wantPath string) *cql2.SyntaxError {
	t.Helper()
	if err == nil {
		t.Fatal("expected error")
	}
	var se *cql2.SyntaxError
	if !errors.As(err, &se) {
		t.Fatalf("want *SyntaxError, got %T: %v", err, err)
	}
	if se.Encoding != cql2.EncodingJSON {
		t.Fatalf("encoding=%v", se.Encoding)
	}
	if wantPath != "" && se.At.JSONPath != wantPath {
		t.Fatalf("path=%q want %q", se.At.JSONPath, wantPath)
	}
	return se
}

// Unknown ops fall back to FunctionCall (case-preserved).
// This used to error; see fix #3.
func TestParseUnknownOpFallback(t *testing.T) {
	n, err := cqljson.Parse([]byte(`{"op":"foo","args":[1,2]}`))
	if err != nil {
		t.Fatalf("unexpected: %v", err)
	}
	fc, ok := n.(*cql2.FunctionCall)
	if !ok {
		t.Fatalf("got %T, want *FunctionCall", n)
	}
	if fc.Name != "foo" {
		t.Fatalf("name=%q want %q", fc.Name, "foo")
	}
	if len(fc.Args) != 2 {
		t.Fatalf("args len=%d want 2", len(fc.Args))
	}
	if nl, ok := fc.Args[0].(*cql2.NumLit); !ok || string(nl.Value) != "1" {
		t.Fatalf("args[0]=%#v", fc.Args[0])
	}
	if nl, ok := fc.Args[1].(*cql2.NumLit); !ok || string(nl.Value) != "2" {
		t.Fatalf("args[1]=%#v", fc.Args[1])
	}
}

func TestParseUnknownOpFallbackPreservesCase(t *testing.T) {
	// Function names are case-preserved; only operator names are case-insensitive.
	n, err := cqljson.Parse([]byte(`{"op":"Foo","args":[1,2]}`))
	if err != nil {
		t.Fatalf("unexpected: %v", err)
	}
	fc, ok := n.(*cql2.FunctionCall)
	if !ok {
		t.Fatalf("got %T", n)
	}
	if fc.Name != "Foo" {
		t.Fatalf("name=%q want %q (case-preserved)", fc.Name, "Foo")
	}
}

func TestErrorMissingArgs(t *testing.T) {
	_, err := cqljson.Parse([]byte(`{"op":"="}`))
	assertSyntaxJSON(t, err, "")
}

func TestErrorMalformedTimestamp(t *testing.T) {
	_, err := cqljson.Parse([]byte(`{"timestamp":"not-a-time"}`))
	assertSyntaxJSON(t, err, "/timestamp")
}

func TestErrorArgsNotArray(t *testing.T) {
	_, err := cqljson.Parse([]byte(`{"op":"=","args":"oops"}`))
	assertSyntaxJSON(t, err, "/args")
}

func TestErrorUnknownObject(t *testing.T) {
	_, err := cqljson.Parse([]byte(`{"foo":1}`))
	se := assertSyntaxJSON(t, err, "")
	if !strings.Contains(se.Got, "foo") {
		t.Fatalf("got=%q", se.Got)
	}
}

func TestErrorNestedPath(t *testing.T) {
	// Outer op args[0] is itself an op whose args[1] is a malformed timestamp.
	in := `{"op":"and","args":[{"op":"=","args":[{"property":"a"},{"timestamp":"bad"}]},true]}`
	_, err := cqljson.Parse([]byte(in))
	se := assertSyntaxJSON(t, err, "/args/0/args/1/timestamp")
	_ = se
}

func TestErrorOpArgChildHasFullPath(t *testing.T) {
	// A malformed args (not an array) deep inside the tree should report a
	// JSONPath that includes every enclosing args[<i>] segment.
	in := `{"op":"and","args":[{"op":"or","args":[{"op":"=","args":"oops"}]}]}`
	_, err := cqljson.Parse([]byte(in))
	assertSyntaxJSON(t, err, "/args/0/args/0/args")
}

// Top-level node types round-trip via Parse→Encode→Parse with cql2.Equal.

func TestRoundTripScalars(t *testing.T) {
	cases := []string{
		`true`, `false`, `null`, `"hello"`, `5`, `1.5`, `1e10`,
		`{"property":"x"}`,
		`{"timestamp":"2020-01-02T03:04:05Z"}`,
		`{"date":"2020-06-15"}`,
		`{"bbox":[1,2,3,4]}`,
		`{"bbox":[1,2,3,4,5,6]}`,
		`[1,2,3]`,
		`{"interval":["..","2020-12-31"]}`,
		`{"interval":["2020-01-01","2020-12-31"]}`,
		`{"interval":["2020-01-01T00:00:00Z","2020-12-31T00:00:00Z"]}`,
		`{"function":{"name":"abs","args":[5]}}`,
	}
	for _, in := range cases {
		t.Run(in, func(t *testing.T) {
			n1, err := cqljson.Parse([]byte(in))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			out, err := cqljson.Encode(n1)
			if err != nil {
				t.Fatalf("encode: %v", err)
			}
			n2, err := cqljson.Parse(out)
			if err != nil {
				t.Fatalf("re-parse %q: %v", out, err)
			}
			if !cql2.Equal(n1, n2) {
				t.Fatalf("ASTs differ: %q -> %q", in, out)
			}
		})
	}
}

func TestRoundTripOperatorClasses(t *testing.T) {
	// One per operator class.
	cases := []string{
		`{"op":"and","args":[true,false]}`,
		`{"op":"=","args":[1,2]}`,
		`{"op":"like","args":[{"property":"x"},"%foo%"]}`,
		`{"op":"+","args":[1,2]}`,
		`{"op":"s_intersects","args":[{"property":"g"},{"type":"Point","coordinates":[1,2]}]}`,
		`{"op":"t_after","args":[{"property":"t"},{"timestamp":"2020-01-01T00:00:00Z"}]}`,
		`{"op":"a_contains","args":[{"property":"a"},[1,2]]}`,
		`{"op":"casei","args":[{"property":"name"}]}`,
	}
	for _, in := range cases {
		t.Run(in, func(t *testing.T) {
			n1, err := cqljson.Parse([]byte(in))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			out, err := cqljson.Encode(n1)
			if err != nil {
				t.Fatalf("encode: %v", err)
			}
			if string(out) != in {
				t.Fatalf("byte mismatch:\n  in:  %s\n  out: %s", in, out)
			}
		})
	}
}

// §1 example: houses with pools (3+ bedrooms, value < 300k, has pool).
func TestRoundTripSpecExample1(t *testing.T) {
	// CQL2-JSON form of: floors > 5 AND swimming_pool = TRUE
	in := `{"op":"and","args":[{"op":">","args":[{"property":"floors"},5]},{"op":"=","args":[{"property":"swimming_pool"},true]}]}`
	roundTripBytes(t, in)
}

// Appendix A example: 4-clause STAC filter.
func TestRoundTripAppendixA(t *testing.T) {
	in := `{"op":"and","args":[` +
		`{"op":"like","args":[{"property":"id"},"LC08%"]},` +
		`{"op":"<","args":[{"property":"eo:cloud_cover"},10]},` +
		`{"op":"s_intersects","args":[{"property":"geometry"},{"type":"Polygon","coordinates":[[[-180,-90],[180,-90],[180,90],[-180,90],[-180,-90]]]}]},` +
		`{"op":"t_after","args":[{"property":"datetime"},{"timestamp":"2022-11-11T00:00:00Z"}]}` +
		`]}`
	roundTripBytes(t, in)
}

// Fix #2: JSON op names are case-insensitive against the lowercase Operator
// enum. The spec uses camelCase in JSON examples (t_finishedBy, t_metBy,
// t_overlappedBy, t_startedBy); accept those and store the canonical form.
func TestParseOpCamelCaseTemporal(t *testing.T) {
	cases := []struct {
		in   string
		want cql2.Operator
	}{
		{`{"op":"t_finishedBy","args":[{"property":"a"},{"property":"b"}]}`, cql2.OpTFinishedBy},
		{`{"op":"t_metBy","args":[{"property":"a"},{"property":"b"}]}`, cql2.OpTMetBy},
		{`{"op":"t_overlappedBy","args":[{"property":"a"},{"property":"b"}]}`, cql2.OpTOverlappedBy},
		{`{"op":"t_startedBy","args":[{"property":"a"},{"property":"b"}]}`, cql2.OpTStartedBy},
	}
	for _, tc := range cases {
		t.Run(string(tc.want), func(t *testing.T) {
			n, err := cqljson.Parse([]byte(tc.in))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			o, ok := n.(*cql2.Op)
			if !ok {
				t.Fatalf("got %T, want *Op", n)
			}
			if o.Op != tc.want {
				t.Fatalf("op=%q want %q (canonical lowercase)", o.Op, tc.want)
			}
			if len(o.Args) != 2 {
				t.Fatalf("args len=%d", len(o.Args))
			}
		})
	}
}

func TestParseOpUpperCaseIdempotent(t *testing.T) {
	// Pathologically all-caps spelling should normalize the same way.
	n, err := cqljson.Parse([]byte(`{"op":"T_FINISHEDBY","args":[{"property":"a"},{"property":"b"}]}`))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	o, ok := n.(*cql2.Op)
	if !ok {
		t.Fatalf("got %T", n)
	}
	if o.Op != cql2.OpTFinishedBy {
		t.Fatalf("op=%q want %q", o.Op, cql2.OpTFinishedBy)
	}
}

func TestParseOpCanonicalCasePreserved(t *testing.T) {
	// OpIsNull's canonical value is "isNull" (camelCase). Regardless of how
	// the input is cased, the AST must store the canonical form so encoders
	// emit it correctly. This guards the lookup-by-lowercase mechanism.
	cases := []string{
		`{"op":"isNull","args":[{"property":"a"}]}`,
		`{"op":"isnull","args":[{"property":"a"}]}`,
		`{"op":"ISNULL","args":[{"property":"a"}]}`,
		`{"op":"IsNull","args":[{"property":"a"}]}`,
	}
	for _, in := range cases {
		t.Run(in, func(t *testing.T) {
			n, err := cqljson.Parse([]byte(in))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			o, ok := n.(*cql2.Op)
			if !ok {
				t.Fatalf("got %T, want *Op (regression: routed to FunctionCall)", n)
			}
			if o.Op != cql2.OpIsNull {
				t.Fatalf("op=%q want %q (canonical preserved)", o.Op, cql2.OpIsNull)
			}
		})
	}
}

func TestParseUnknownOpMissingArgs(t *testing.T) {
	// Even unknown ops must have an "args" key — missing args is a syntax error,
	// not a silent fallback to a zero-arg FunctionCall.
	_, err := cqljson.Parse([]byte(`{"op":"foo"}`))
	assertSyntaxJSON(t, err, "")
}

func TestParseUnknownOpArgsNotArray(t *testing.T) {
	_, err := cqljson.Parse([]byte(`{"op":"foo","args":"not-an-array"}`))
	assertSyntaxJSON(t, err, "/args")
}

// Fix #4 (json side): interval endpoints may be {"property":"<name>"} objects.
func TestParseIntervalPropertyEndpoints(t *testing.T) {
	in := `{"interval":[{"property":"starts_at"},{"property":"ends_at"}]}`
	n, err := cqljson.Parse([]byte(in))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	iv, ok := n.(*cql2.IntervalLit)
	if !ok {
		t.Fatalf("got %T", n)
	}
	ps, ok := iv.Start.(*cql2.PropertyRef)
	if !ok || ps.Name != "starts_at" {
		t.Fatalf("start=%#v", iv.Start)
	}
	pe, ok := iv.End.(*cql2.PropertyRef)
	if !ok || pe.Name != "ends_at" {
		t.Fatalf("end=%#v", iv.End)
	}
	out, err := cqljson.Encode(n)
	if err != nil {
		t.Fatalf("encode: %v", err)
	}
	if string(out) != in {
		t.Fatalf("round-trip:\n  in:  %s\n  out: %s", in, out)
	}
}

func TestParseIntervalMixedLiteralStartPropertyEnd(t *testing.T) {
	in := `{"interval":["2020-01-01",{"property":"ends_at"}]}`
	n, err := cqljson.Parse([]byte(in))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	iv := n.(*cql2.IntervalLit)
	if _, ok := iv.Start.(*cql2.DateLit); !ok {
		t.Fatalf("start=%T", iv.Start)
	}
	if pe, ok := iv.End.(*cql2.PropertyRef); !ok || pe.Name != "ends_at" {
		t.Fatalf("end=%#v", iv.End)
	}
	out, err := cqljson.Encode(n)
	if err != nil {
		t.Fatalf("encode: %v", err)
	}
	if string(out) != in {
		t.Fatalf("round-trip:\n  in:  %s\n  out: %s", in, out)
	}
}

func TestParseIntervalMixedPropertyStartUnboundedEnd(t *testing.T) {
	in := `{"interval":[{"property":"starts_at"},".."]}`
	n, err := cqljson.Parse([]byte(in))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	iv := n.(*cql2.IntervalLit)
	if ps, ok := iv.Start.(*cql2.PropertyRef); !ok || ps.Name != "starts_at" {
		t.Fatalf("start=%#v", iv.Start)
	}
	if _, ok := iv.End.(*cql2.Unbounded); !ok {
		t.Fatalf("end=%T", iv.End)
	}
	out, err := cqljson.Encode(n)
	if err != nil {
		t.Fatalf("encode: %v", err)
	}
	if string(out) != in {
		t.Fatalf("round-trip:\n  in:  %s\n  out: %s", in, out)
	}
}

func TestParseIntervalPropertyMissingKey(t *testing.T) {
	// Object without "property" key is a syntax error rooted at the endpoint.
	_, err := cqljson.Parse([]byte(`{"interval":[{"foo":"bar"},".."]}`))
	assertSyntaxJSON(t, err, "/interval/0")
}

func TestParseIntervalPropertyWrongType(t *testing.T) {
	// Non-string property value points at /interval/<i>/property.
	_, err := cqljson.Parse([]byte(`{"interval":[{"property":42},".."]}`))
	assertSyntaxJSON(t, err, "/interval/0/property")
}

// roundTripBytes asserts Parse→Encode→Parse→Encode produces byte-identical
// output to the second-stage encoding (canonicalization stable).
func roundTripBytes(t *testing.T, in string) {
	t.Helper()
	n1, err := cqljson.Parse([]byte(in))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	enc1, err := cqljson.Encode(n1)
	if err != nil {
		t.Fatalf("encode1: %v", err)
	}
	n2, err := cqljson.Parse(enc1)
	if err != nil {
		t.Fatalf("re-parse: %v", err)
	}
	enc2, err := cqljson.Encode(n2)
	if err != nil {
		t.Fatalf("encode2: %v", err)
	}
	if string(enc1) != string(enc2) {
		t.Fatalf("non-canonical:\n  enc1: %s\n  enc2: %s", enc1, enc2)
	}
	if !cql2.Equal(n1, n2) {
		t.Fatalf("AST differ")
	}
	// Also assert valid JSON.
	var any stdjson.RawMessage
	if err := stdjson.Unmarshal(enc1, &any); err != nil {
		t.Fatalf("invalid JSON: %v", err)
	}
}
