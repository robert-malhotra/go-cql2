package json

import (
	"errors"
	"strings"
	"testing"

	cql2 "github.com/exergy-dev/go-cql2"
)

func TestRejectsExtraKeys(t *testing.T) {
	cases := []string{
		`{"op":"=","args":[{"property":"a"},1],"extra":"junk"}`,
		`{"property":"a","extra":"junk"}`,
		`{"timestamp":"2024-01-01T00:00:00Z","extra":"junk"}`,
		`{"date":"2024-01-01","extra":"junk"}`,
		`{"interval":["..","2024-01-01"],"extra":"junk"}`,
		`{"bbox":[-180,-90,180,90],"extra":"junk"}`,
		`{"function":{"name":"abs","args":[1]},"extra":"junk"}`,
	}
	for _, in := range cases {
		_, err := Parse([]byte(in))
		var se *cql2.SyntaxError
		if !errors.As(err, &se) {
			t.Errorf("Parse(%s) err = %v, want *cql2.SyntaxError", in, err)
		}
	}
}

func TestGeometryForeignMembersAccepted(t *testing.T) {
	// RFC 7946 permits foreign members on geometry objects.
	in := `{"type":"Point","coordinates":[1,2],"crs":"EPSG:4326"}`
	if _, err := Parse([]byte(in)); err != nil {
		t.Fatalf("Parse(%s) unexpected error: %v", in, err)
	}
}

// TestArrayOpCanonicalSpelling enforces Annex C's enum spelling for
// the array predicates. a_containedBy must round-trip in camelCase
// to match the schema.
func TestArrayOpCanonicalSpelling(t *testing.T) {
	in := `{"op":"a_containedby","args":[{"property":"layers"},["a","b"]]}`
	n, err := Parse([]byte(in))
	if err != nil {
		t.Fatalf("Parse(lowercase): %v", err)
	}
	out, err := Encode(n)
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	if !strings.Contains(string(out), `"a_containedBy"`) {
		t.Fatalf("encoded output = %s, want canonical a_containedBy", string(out))
	}
	op, ok := n.(*cql2.Op)
	if !ok || op.Op != cql2.OpAContainedBy {
		t.Fatalf("AST op = %#v, want OpAContainedBy", op)
	}
}

// TestIntervalFunctionEndpointJSON covers Annex C's intervalArray schema
// which permits a functionRef object as an interval endpoint.
func TestIntervalFunctionEndpointJSON(t *testing.T) {
	cases := []string{
		// Inline-op form (parsed by FunctionCall fallback).
		`{"op":"t_after","args":[{"property":"event_time"},{"interval":[{"op":"now","args":[]},".."]}]}`,
		// Function form.
		`{"op":"t_after","args":[{"property":"event_time"},{"interval":[{"function":{"name":"now","args":[]}},".."]}]}`,
	}
	for _, in := range cases {
		n, err := Parse([]byte(in))
		if err != nil {
			t.Fatalf("Parse(%s): %v", in, err)
		}
		op, ok := n.(*cql2.Op)
		if !ok || op.Op != cql2.OpTAfter {
			t.Fatalf("Parse(%s) outer = %#v", in, n)
		}
		iv, ok := op.Args[1].(*cql2.IntervalLit)
		if !ok {
			t.Fatalf("interval = %T", op.Args[1])
		}
		if _, ok := iv.Start.(*cql2.FunctionCall); !ok {
			t.Fatalf("Parse(%s) interval start = %T, want *FunctionCall", in, iv.Start)
		}
		// Round-trip: encoded JSON re-parses to an equal AST.
		out, err := Encode(n)
		if err != nil {
			t.Fatalf("Encode: %v", err)
		}
		n2, err := Parse(out)
		if err != nil {
			t.Fatalf("re-Parse(%s): %v", string(out), err)
		}
		if !cql2.Equal(n, n2) {
			t.Fatalf("round-trip mismatch: in=%s out=%s", in, string(out))
		}
	}
}

func TestMaxDepthRejectsDeepNesting(t *testing.T) {
	deep := strings.Repeat(`{"op":"not","args":[`, 1000) + "true" + strings.Repeat("]}", 1000)
	_, err := Parse([]byte(deep))
	if err == nil {
		t.Fatalf("expected depth error, got nil")
	}
	var se *cql2.SyntaxError
	if !errors.As(err, &se) {
		t.Fatalf("err = %v, want *cql2.SyntaxError", err)
	}
	// With the limit raised, the same input should parse.
	if _, err := Parse([]byte(deep), cql2.WithMaxDepth(2000)); err != nil {
		t.Fatalf("WithMaxDepth(2000) should accept: %v", err)
	}
}
