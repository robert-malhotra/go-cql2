package cql2_test

import (
	"encoding/json"
	"errors"
	"testing"

	cql2 "github.com/exergy-dev/go-cql2"
	_ "github.com/exergy-dev/go-cql2/codecs"
	cqljson "github.com/exergy-dev/go-cql2/json"
)

// TestValidate_GoodInputs covers ASTs that must pass validation. The
// builder helpers all emit shapes Validate accepts.
func TestValidate_GoodInputs(t *testing.T) {
	cases := []cql2.Node{
		cql2.Eq("a", 1).Node(),
		cql2.And(cql2.Eq("a", 1), cql2.Eq("b", 2)).Node(),
		cql2.Or(cql2.Eq("a", 1), cql2.Eq("b", 2), cql2.Eq("c", 3)).Node(),
		cql2.Not(cql2.Eq("a", 1)).Node(),
		cql2.Between("a", 0, 10).Node(),
		cql2.In("a", 1, 2, 3).Node(),
		cql2.IsNull("a").Node(),
		cql2.Like("name", "A%").Node(),
		&cql2.BoolLit{Value: true},
		&cql2.NumLit{Value: json.Number("1")},
		&cql2.StringLit{Value: "x"},
		&cql2.NullLit{},
		&cql2.PropertyRef{Name: "a"},
		&cql2.ArrayLit{Elements: []cql2.Node{
			&cql2.NumLit{Value: json.Number("1")},
			&cql2.NumLit{Value: json.Number("2")},
		}},
	}
	for i, n := range cases {
		if err := cql2.Validate(n); err != nil {
			t.Errorf("case %d: Validate returned %v for valid AST %#v", i, err, n)
		}
	}
}

// TestValidate_NilNode is the documented contract: Validate(nil) is an error.
func TestValidate_NilNode(t *testing.T) {
	if err := cql2.Validate(nil); err == nil {
		t.Fatalf("Validate(nil) returned nil error")
	}
}

// TestValidate_OpArity covers the most common arity violations. Each input
// is a hand-built AST (parsers won't emit these shapes; that's the whole
// point of having Validate).
func TestValidate_OpArity(t *testing.T) {
	cases := []struct {
		name string
		n    cql2.Node
	}{
		{
			name: "and with 1 arg",
			n:    &cql2.Op{Op: cql2.OpAnd, Args: []cql2.Node{&cql2.BoolLit{Value: true}}},
		},
		{
			name: "or with 0 args",
			n:    &cql2.Op{Op: cql2.OpOr},
		},
		{
			name: "not with 2 args",
			n: &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{
				&cql2.BoolLit{Value: true}, &cql2.BoolLit{Value: false},
			}},
		},
		{
			name: "= with 3 args",
			n: &cql2.Op{Op: cql2.OpEq, Args: []cql2.Node{
				&cql2.NumLit{Value: json.Number("1")},
				&cql2.NumLit{Value: json.Number("2")},
				&cql2.NumLit{Value: json.Number("3")},
			}},
		},
		{
			name: "between with 2 args",
			n: &cql2.Op{Op: cql2.OpBetween, Args: []cql2.Node{
				&cql2.PropertyRef{Name: "x"},
				&cql2.NumLit{Value: json.Number("1")},
			}},
		},
		{
			name: "in with non-array RHS",
			n: &cql2.Op{Op: cql2.OpIn, Args: []cql2.Node{
				&cql2.PropertyRef{Name: "x"},
				&cql2.NumLit{Value: json.Number("5")},
			}},
		},
		{
			name: "in with empty array",
			n: &cql2.Op{Op: cql2.OpIn, Args: []cql2.Node{
				&cql2.PropertyRef{Name: "x"},
				&cql2.ArrayLit{},
			}},
		},
		{
			name: "isNull with 0 args",
			n:    &cql2.Op{Op: cql2.OpIsNull},
		},
		{
			name: "casei with 2 args",
			n: &cql2.Op{Op: cql2.OpCaseI, Args: []cql2.Node{
				&cql2.PropertyRef{Name: "x"},
				&cql2.PropertyRef{Name: "y"},
			}},
		},
		{
			name: "s_intersects with 1 arg",
			n: &cql2.Op{Op: cql2.OpSIntersects, Args: []cql2.Node{
				&cql2.PropertyRef{Name: "g"},
			}},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			err := cql2.Validate(tc.n)
			if err == nil {
				t.Fatalf("expected validation error, got nil")
			}
			var ve *cql2.ValidationError
			if !errors.As(err, &ve) {
				t.Fatalf("expected *ValidationError, got %T: %v", err, err)
			}
			if ve.Op == "" {
				t.Errorf("ValidationError.Op was empty for op-shaped violation")
			}
		})
	}
}

// TestValidate_RecurseIntoChildren ensures a violation deep in the tree
// surfaces from the root.
func TestValidate_RecurseIntoChildren(t *testing.T) {
	// Outer AND is fine; one of its operands has bad arity.
	root := &cql2.Op{Op: cql2.OpAnd, Args: []cql2.Node{
		&cql2.Op{Op: cql2.OpEq, Args: []cql2.Node{
			&cql2.PropertyRef{Name: "a"},
			&cql2.NumLit{Value: json.Number("1")},
		}},
		&cql2.Op{Op: cql2.OpIsNull}, // arity violation
	}}
	err := cql2.Validate(root)
	if err == nil {
		t.Fatal("expected nested validation error")
	}
	var ve *cql2.ValidationError
	if !errors.As(err, &ve) {
		t.Fatalf("got %T: %v", err, err)
	}
	if ve.Op != cql2.OpIsNull {
		t.Errorf("Op=%q want %q", ve.Op, cql2.OpIsNull)
	}
}

// TestValidate_IntegratedWithJSONParse confirms that the JSON parser now
// rejects arity-violating inputs that previously slipped through.
func TestValidate_IntegratedWithJSONParse(t *testing.T) {
	// Each input parses as JSON without a SyntaxError but violates
	// arity / shape — the late Validate pass must reject them.
	cases := []string{
		`{"op":"=","args":[1,2,3]}`,
		`{"op":"between","args":[1,2]}`,
		`{"op":"in","args":[{"property":"a"},5]}`,
		`{"op":"in","args":[{"property":"a"},[]]}`,
		`{"op":"isNull","args":[1,2]}`,
		`{"op":"and","args":[true]}`,
	}
	for _, in := range cases {
		t.Run(in, func(t *testing.T) {
			_, err := cqljson.Parse([]byte(in))
			if err == nil {
				t.Fatalf("expected error for %q, got none", in)
			}
			var ve *cql2.ValidationError
			if !errors.As(err, &ve) {
				t.Fatalf("expected *ValidationError, got %T: %v", err, err)
			}
		})
	}
}

// TestValidate_FunctionCall_MissingName covers the FunctionCall branch.
func TestValidate_FunctionCall_MissingName(t *testing.T) {
	n := &cql2.FunctionCall{}
	err := cql2.Validate(n)
	var ve *cql2.ValidationError
	if !errors.As(err, &ve) {
		t.Fatalf("expected *ValidationError, got %T: %v", err, err)
	}
}

// Function-call names that are not valid CQL2 identifiers would re-emit
// as text the parser cannot consume; Validate should reject them upfront.
func TestValidate_FunctionCall_BadName(t *testing.T) {
	for _, name := range []string{"1bad", "has space", "$dollar", "weird-name"} {
		err := cql2.Validate(&cql2.FunctionCall{Name: name})
		var ve *cql2.ValidationError
		if !errors.As(err, &ve) {
			t.Fatalf("Validate FunctionCall %q: err=%v, want *ValidationError", name, err)
		}
	}
	// Sanity: a normal name passes.
	if err := cql2.Validate(&cql2.FunctionCall{Name: "abs"}); err != nil {
		t.Fatalf("Validate FunctionCall abs: unexpected error %v", err)
	}
}

// Function-call names that match a reserved CQL2 keyword would be parsed
// back as a typed literal or *Op, breaking round-trip for any builder-
// constructed AST. Validate must reject them up front.
func TestValidate_FunctionCall_ReservedName(t *testing.T) {
	for _, name := range []string{
		"s_intersects", "S_INTERSECTS",
		"t_after", "T_FinishedBy",
		"a_contains",
		"casei", "ACCENTI",
		"date", "TIMESTAMP", "interval", "BBOX",
		"point", "POLYGON",
	} {
		err := cql2.Validate(&cql2.FunctionCall{Name: name})
		var ve *cql2.ValidationError
		if !errors.As(err, &ve) {
			t.Errorf("Validate FunctionCall %q: err=%v, want *ValidationError", name, err)
		}
	}
}
