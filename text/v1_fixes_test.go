package text

import (
	"encoding/json"
	"errors"
	"strings"
	"testing"

	cql2 "github.com/exergy-dev/go-cql2"
)

// Regression tests for the v1 readiness fixes. Each subtest pins the
// behaviour described in CHANGELOG.md.

func TestNotNotParses(t *testing.T) {
	for _, in := range []string{
		"NOT NOT a",
		"NOT NOT NOT a",
		"NOT NOT TRUE",
	} {
		n, err := Parse(in)
		if err != nil {
			t.Fatalf("Parse(%q) error: %v", in, err)
		}
		op, ok := n.(*cql2.Op)
		if !ok || op.Op != cql2.OpNot {
			t.Fatalf("Parse(%q) outer = %#v, want *Op{OpNot,…}", in, n)
		}
		// Round-trip.
		out, err := Encode(n)
		if err != nil {
			t.Fatalf("Encode(%q) error: %v", in, err)
		}
		n2, err := Parse(out)
		if err != nil {
			t.Fatalf("re-Parse(%q) error: %v", out, err)
		}
		if !cql2.Equal(n, n2) {
			t.Fatalf("round-trip mismatch: in=%q out=%q", in, out)
		}
	}
}

func TestNumberLexerRejectsJSONIncompatibleForms(t *testing.T) {
	for _, in := range []string{
		"x = 1.",
		"x = .5",
		"x = 1.e5",
		"x = 1.E-3",
	} {
		_, err := Parse(in)
		var se *cql2.SyntaxError
		if !errors.As(err, &se) {
			t.Fatalf("Parse(%q) err = %v, want *cql2.SyntaxError", in, err)
		}
	}
	// Counter-cases that should still parse.
	for _, in := range []string{
		"x = 0",
		"x = 0.0",
		"x = 1.5",
		"x = 1e10",
		"x = 1.5e2",
		"x = -3.14",
	} {
		if _, err := Parse(in); err != nil {
			t.Fatalf("Parse(%q) unexpected error: %v", in, err)
		}
	}
}

func TestReservedKeywordsRejectedAsIdentifiers(t *testing.T) {
	cases := []string{
		"and = 1",
		"or = 1",
		"not = 1",
		"like = 1",
		"between = 1",
		"in = 1",
		"is = 1",
		"div = 1",
		"date = 1",
		"timestamp = 1",
		"interval = 1",
		"bbox = 1",
		"AND(a, b)",
		"OR(a, b)",
		"IN(a, 1)",
		"BETWEEN(1, 2, 3)",
		"LIKE(a, 'b')",
		"DIV(1, 2)",
		"NULL(x)",
	}
	for _, in := range cases {
		_, err := Parse(in)
		var se *cql2.SyntaxError
		if !errors.As(err, &se) {
			t.Errorf("Parse(%q) err = %v, want *cql2.SyntaxError", in, err)
		}
	}
	// Quoting allows reserved words as property names.
	if _, err := Parse(`"and" = 1`); err != nil {
		t.Fatalf("quoted reserved word should parse: %v", err)
	}
}

func TestBBoxArityErrorAtKeyword(t *testing.T) {
	_, err := Parse("BBOX(1,2,3)")
	var se *cql2.SyntaxError
	if !errors.As(err, &se) {
		t.Fatalf("err = %v, want *cql2.SyntaxError", err)
	}
	if se.At.Column != 1 {
		t.Fatalf("error column = %d, want 1 (BBOX keyword)", se.At.Column)
	}
}

func TestMaxDepthRejectsDeepNesting(t *testing.T) {
	deep := strings.Repeat("(", 1000) + "1=1" + strings.Repeat(")", 1000)
	_, err := Parse(deep)
	if err == nil {
		t.Fatalf("expected depth error, got nil")
	}
	var se *cql2.SyntaxError
	if !errors.As(err, &se) {
		t.Fatalf("err = %v, want *cql2.SyntaxError", err)
	}
	if !strings.Contains(se.Msg, "nesting") {
		t.Fatalf("err msg = %q, want depth-related", se.Msg)
	}
	// With the limit raised, the same input should parse.
	if _, err := Parse(deep, cql2.WithMaxDepth(2000)); err != nil {
		t.Fatalf("WithMaxDepth(2000) should accept: %v", err)
	}
}

// TestUnicodeIdentifiers covers Annex B's propertyName grammar, which
// admits letters from many Unicode blocks beyond ASCII. The parser used
// to be byte-wise ASCII-only.
func TestUnicodeIdentifiers(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{"greek", `Οδος = 'foo'`},
		{"cyrillic", `Москва = 1`},
		{"cjk", `城市 = '北京'`},
		{"latin1-supplement", `straße = 'Berlin'`},
		{"combining-mark-in-middle", `niño = 1`},
		{"colon-prefix", `:gml:id = 'x'`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			n, err := Parse(tc.src)
			if err != nil {
				t.Fatalf("Parse(%q): %v", tc.src, err)
			}
			out, err := Encode(n)
			if err != nil {
				t.Fatalf("Encode: %v", err)
			}
			n2, err := Parse(string(out))
			if err != nil {
				t.Fatalf("re-Parse(%q): %v", string(out), err)
			}
			if !cql2.Equal(n, n2) {
				t.Fatalf("round-trip mismatch: %q -> %q", tc.src, string(out))
			}
		})
	}
}

// TestStringLiteralBackslashEscapes covers /req/cql2-text/escaping (B + C):
// the parser must recognize \', \\, and the seven C-style control escapes.
func TestStringLiteralBackslashEscapes(t *testing.T) {
	cases := []struct {
		in   string
		want string
	}{
		{`x = 'a\'b'`, "a'b"},
		{`x = 'a\\b'`, `a\b`},
		{`x = 'a\nb'`, "a\nb"},
		{`x = 'a\tb'`, "a\tb"},
		{`x = 'a\rb'`, "a\rb"},
		{`x = 'a\fb'`, "a\fb"},
		{`x = 'a\vb'`, "a\vb"},
		{`x = 'a\bb'`, "a\bb"},
		{`x = 'a\ab'`, "a\ab"},
		// '' must continue to work alongside the backslash form.
		{`x = 'Via dell''Avvento'`, `Via dell'Avvento`},
	}
	for _, tc := range cases {
		n, err := Parse(tc.in)
		if err != nil {
			t.Fatalf("Parse(%q): %v", tc.in, err)
		}
		op, ok := n.(*cql2.Op)
		if !ok || op.Op != cql2.OpEq {
			t.Fatalf("Parse(%q) outer = %#v", tc.in, n)
		}
		lit, ok := op.Args[1].(*cql2.StringLit)
		if !ok {
			t.Fatalf("Parse(%q) RHS = %T", tc.in, op.Args[1])
		}
		if lit.Value != tc.want {
			t.Errorf("Parse(%q) value = %q, want %q", tc.in, lit.Value, tc.want)
		}
	}
}

func TestStringLiteralUnknownEscapeIsError(t *testing.T) {
	_, err := Parse(`x = 'foo\zbar'`)
	var se *cql2.SyntaxError
	if !errors.As(err, &se) {
		t.Fatalf("err = %v, want *cql2.SyntaxError", err)
	}
}

// TestStringLiteralRoundTripControlChars verifies that strings containing
// control characters and embedded quotes round-trip cleanly through the
// encoder/parser pair, and that the encoded form uses backslash escapes
// rather than raw bytes.
func TestStringLiteralRoundTripControlChars(t *testing.T) {
	cases := []struct {
		value string
		want  string // exact text encoding (after the leading `x = `)
	}{
		{value: "a\nb", want: `'a\nb'`},
		{value: "a\tb", want: `'a\tb'`},
		{value: "a\rb", want: `'a\rb'`},
		{value: "a\\b", want: `'a\\b'`},
		{value: "a'b", want: `'a''b'`}, // single-quote uses '' form for compactness
		{value: "all\a\b\t\n\v\f\r", want: `'all\a\b\t\n\v\f\r'`},
	}
	for _, tc := range cases {
		ast := &cql2.Op{Op: cql2.OpEq, Args: []cql2.Node{
			&cql2.PropertyRef{Name: "x"},
			&cql2.StringLit{Value: tc.value},
		}}
		out, err := Encode(ast)
		if err != nil {
			t.Fatalf("Encode(%q): %v", tc.value, err)
		}
		got := strings.TrimPrefix(string(out), "x = ")
		if got != tc.want {
			t.Errorf("Encode(%q) = %q, want %q", tc.value, got, tc.want)
		}
		// Round-trip: parsing the encoded form recovers the original value.
		parsed, err := Parse(string(out))
		if err != nil {
			t.Fatalf("Parse(%q): %v", string(out), err)
		}
		if !cql2.Equal(ast, parsed) {
			t.Errorf("round-trip mismatch for %q: encoded=%q parsed=%#v", tc.value, string(out), parsed)
		}
	}
}

// TestIntervalFunctionEndpoint covers Annex B's instantParameter rule and
// Annex C's intervalArray schema, both of which permit a function call as
// an interval endpoint.
func TestIntervalFunctionEndpoint(t *testing.T) {
	in := `T_AFTER(event_time, INTERVAL(now(), '..'))`
	n, err := Parse(in)
	if err != nil {
		t.Fatalf("Parse(%q): %v", in, err)
	}
	out, err := Encode(n)
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	n2, err := Parse(out)
	if err != nil {
		t.Fatalf("re-Parse(%q): %v", out, err)
	}
	if !cql2.Equal(n, n2) {
		t.Fatalf("round-trip mismatch: %q -> %q", in, out)
	}
	// Confirm the endpoint actually landed as a *FunctionCall.
	op, ok := n.(*cql2.Op)
	if !ok || op.Op != cql2.OpTAfter {
		t.Fatalf("outer = %#v", n)
	}
	iv, ok := op.Args[1].(*cql2.IntervalLit)
	if !ok {
		t.Fatalf("interval = %T", op.Args[1])
	}
	if _, ok := iv.Start.(*cql2.FunctionCall); !ok {
		t.Fatalf("interval start = %T, want *FunctionCall", iv.Start)
	}
}

func TestNumLitsAreValidJSONNumbers(t *testing.T) {
	// Property: every NumLit produced by the text parser carries a verbatim
	// source spelling that is a valid JSON number — so emitting it inside a
	// CQL2-JSON document never produces JSON the standard library rejects.
	for _, in := range []string{
		"x = 0",
		"x = 0.0",
		"x = 12345",
		"x = -3.14",
		"x = 1e10",
		"x = 1.5e-2",
	} {
		n, err := Parse(in)
		if err != nil {
			t.Fatalf("Parse(%q): %v", in, err)
		}
		var lit *cql2.NumLit
		cql2.Inspect(n, func(node cql2.Node) bool {
			if x, ok := node.(*cql2.NumLit); ok {
				lit = x
				return false
			}
			return true
		})
		if lit == nil {
			t.Fatalf("Parse(%q): no NumLit in AST", in)
		}
		var dec any
		if err := json.Unmarshal([]byte(string(lit.Value)), &dec); err != nil {
			t.Fatalf("NumLit value %q is not valid JSON: %v", string(lit.Value), err)
		}
	}
}
