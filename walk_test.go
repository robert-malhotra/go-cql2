package cql2

import (
	"encoding/json"
	"reflect"
	"testing"
	"time"
)

// buildSampleAST returns a non-trivial AST mixing Op/FunctionCall/literals
// /PropertyRef/ArrayLit. Returned alongside the expected pre-order kind
// sequence (just for the non-nil visits) so tests can sanity-check order.
//
// Tree shape:
//
//	Op[and]
//	├── Op[=]
//	│   ├── PropertyRef("name")
//	│   └── StringLit("foo")
//	└── Op[<]
//	    ├── FunctionCall("abs", [NumLit(-3)])
//	    └── Op[+]
//	        ├── NumLit(1)
//	        └── ArrayLit[NumLit(2), BoolLit(true)]
func buildSampleAST() (Node, []NodeKind) {
	root := &Op{
		Op: OpAnd,
		Args: []Node{
			&Op{Op: OpEq, Args: []Node{
				&PropertyRef{Name: "name"},
				&StringLit{Value: "foo"},
			}},
			&Op{Op: OpLt, Args: []Node{
				&FunctionCall{Name: "abs", Args: []Node{
					&NumLit{Value: json.Number("-3")},
				}},
				&Op{Op: OpAdd, Args: []Node{
					&NumLit{Value: json.Number("1")},
					&ArrayLit{Elements: []Node{
						&NumLit{Value: json.Number("2")},
						&BoolLit{Value: true},
					}},
				}},
			}},
		},
	}
	expected := []NodeKind{
		KindOp,       // and
		KindOp,       // =
		KindProperty, // name
		KindString,   // foo
		KindOp,       // <
		KindFunction, // abs
		KindNumber,   // -3
		KindOp,       // +
		KindNumber,   // 1
		KindArray,    // [...]
		KindNumber,   // 2
		KindBool,     // true
	}
	return root, expected
}

type counter struct{ n int }

func (c *counter) Visit(n Node) Visitor {
	if n == nil {
		return c
	}
	c.n++
	return c
}

func TestWalkVisitsEveryNodeInPreOrder(t *testing.T) {
	root, expected := buildSampleAST()

	var got []NodeKind
	Inspect(root, func(n Node) bool {
		if n == nil {
			return true
		}
		got = append(got, n.Kind())
		return true
	})
	if !reflect.DeepEqual(got, expected) {
		t.Fatalf("pre-order mismatch:\n want %v\n got  %v", expected, got)
	}

	c := &counter{}
	Walk(c, root)
	if c.n != len(expected) {
		t.Fatalf("Walk visited %d nodes, want %d", c.n, len(expected))
	}
}

func TestInspectSkipSubtree(t *testing.T) {
	root, _ := buildSampleAST()
	// Skip into the right (Op[<]) subtree by returning false on it.
	var visited []NodeKind
	Inspect(root, func(n Node) bool {
		if n == nil {
			return true
		}
		visited = append(visited, n.Kind())
		if op, ok := n.(*Op); ok && op.Op == OpLt {
			return false
		}
		return true
	})
	// Expect: and, =, name, foo, < — and nothing under <.
	want := []NodeKind{KindOp, KindOp, KindProperty, KindString, KindOp}
	if !reflect.DeepEqual(visited, want) {
		t.Fatalf("Inspect skip: got %v want %v", visited, want)
	}
}

func TestChildrenMatchesWalk(t *testing.T) {
	root, _ := buildSampleAST()
	// For every node visited, Children(n) must equal the immediate children
	// Walk descends into. We verify by collecting children via a custom
	// visitor recording parent->child pairs.
	type pair struct {
		parent Node
		child  Node
	}
	var walkPairs []pair
	var stack []Node
	Inspect(root, func(n Node) bool {
		if n == nil {
			if len(stack) > 0 {
				stack = stack[:len(stack)-1]
			}
			return true
		}
		if len(stack) > 0 {
			walkPairs = append(walkPairs, pair{stack[len(stack)-1], n})
		}
		stack = append(stack, n)
		return true
	})

	var childrenPairs []pair
	var collect func(Node)
	collect = func(n Node) {
		for _, c := range Children(n) {
			childrenPairs = append(childrenPairs, pair{n, c})
			collect(c)
		}
	}
	collect(root)

	if !reflect.DeepEqual(walkPairs, childrenPairs) {
		t.Fatalf("Walk children differ from Children():\n walk:     %v\n children: %v", walkPairs, childrenPairs)
	}
}

func TestTransformIdentityReturnsSamePointer(t *testing.T) {
	root, _ := buildSampleAST()
	out := Transform(root, func(n Node) Node { return nil })
	if out != root {
		t.Fatalf("Transform identity must return same pointer; got different node")
	}
}

func TestTransformReplacesPropertyRefs(t *testing.T) {
	root, _ := buildSampleAST()
	out := Transform(root, func(n Node) Node {
		if _, ok := n.(*PropertyRef); ok {
			return &StringLit{Value: "X"}
		}
		return nil
	})
	// The original should still contain a PropertyRef.
	foundOrigProp := false
	Inspect(root, func(n Node) bool {
		if _, ok := n.(*PropertyRef); ok {
			foundOrigProp = true
		}
		return true
	})
	if !foundOrigProp {
		t.Fatalf("original mutated by Transform")
	}
	// The result should have no PropertyRef and at least one StringLit("X").
	foundProp := false
	foundX := false
	Inspect(out, func(n Node) bool {
		if _, ok := n.(*PropertyRef); ok {
			foundProp = true
		}
		if s, ok := n.(*StringLit); ok && s.Value == "X" {
			foundX = true
		}
		return true
	})
	if foundProp {
		t.Fatalf("Transform output still contains *PropertyRef")
	}
	if !foundX {
		t.Fatalf("Transform output missing replacement StringLit{X}")
	}
}

func TestEqualSelfAndClone(t *testing.T) {
	root, _ := buildSampleAST()
	if !Equal(root, root) {
		t.Fatalf("Equal(n,n) should be true")
	}
	cl := Clone(root)
	if !Equal(root, cl) {
		t.Fatalf("Equal(n, Clone(n)) should be true")
	}
	// Mutate the clone and check inequality + non-disturbance.
	clOp := cl.(*Op)
	clOp.Args = append(clOp.Args, &BoolLit{Value: false})
	if Equal(root, cl) {
		t.Fatalf("Equal(n, modifiedClone) should be false")
	}
	if len(root.(*Op).Args) != 2 {
		t.Fatalf("mutating Clone affected original")
	}
}

func TestEqualNilSafety(t *testing.T) {
	if !Equal(nil, nil) {
		t.Fatalf("Equal(nil,nil) should be true")
	}
	if Equal(nil, &BoolLit{Value: true}) {
		t.Fatalf("Equal(nil,x) should be false")
	}
	if Equal(&BoolLit{Value: true}, nil) {
		t.Fatalf("Equal(x,nil) should be false")
	}
}

func TestEqualLiteralValueSemantics(t *testing.T) {
	a := &NumLit{Value: json.Number("1.0")}
	b := &NumLit{Value: json.Number("1.00")}
	if Equal(a, b) {
		t.Fatalf("NumLit equality should be string-form sensitive")
	}
	t1 := time.Date(2024, 1, 2, 3, 4, 5, 0, time.UTC)
	t2 := t1.In(time.FixedZone("X", 3600))
	if !Equal(&TimestampLit{Value: t1}, &TimestampLit{Value: t2}) {
		t.Fatalf("TimestampLit should use time.Time.Equal semantics")
	}
}

func TestCloneDeepDisjoint(t *testing.T) {
	src := &Op{Op: OpAnd, Args: []Node{
		&BoolLit{Value: true},
		&BoolLit{Value: false},
	}}
	cl := Clone(src).(*Op)
	if &cl.Args[0] == &src.Args[0] {
		t.Fatalf("Clone shared Args slice backing")
	}
	cl.Args[0] = &BoolLit{Value: false}
	if src.Args[0].(*BoolLit).Value != true {
		t.Fatalf("mutating clone modified original")
	}
}

func TestCloneNil(t *testing.T) {
	if Clone(nil) != nil {
		t.Fatalf("Clone(nil) should be nil")
	}
}

func TestResultKind(t *testing.T) {
	cases := []struct {
		name string
		n    Node
		want Kind
	}{
		{"and", &Op{Op: OpAnd, Args: []Node{&BoolLit{}, &BoolLit{}}}, KindResultBoolean},
		{"add", &Op{Op: OpAdd, Args: []Node{&NumLit{Value: "1"}, &NumLit{Value: "2"}}}, KindResultNumber},
		{"bool lit", &BoolLit{}, KindResultBoolean},
		{"num lit", &NumLit{Value: "1"}, KindResultNumber},
		{"prop ref", &PropertyRef{Name: "x"}, KindUnknown},
		{"function", &FunctionCall{Name: "f"}, KindUnknown},
		{"casei passthru", &Op{Op: OpCaseI, Args: []Node{&StringLit{Value: "x"}}}, KindResultString},
		{"accenti passthru", &Op{Op: OpAccentI, Args: []Node{&StringLit{Value: "x"}}}, KindResultString},
		{"string lit", &StringLit{}, KindResultString},
		{"null lit", &NullLit{}, KindUnknown},
		{"timestamp", &TimestampLit{}, KindResultTemporal},
		{"date", &DateLit{}, KindResultTemporal},
		{"interval", &IntervalLit{}, KindResultTemporal},
		{"geom", &GeomLit{}, KindResultGeometry},
		{"bbox", &BBoxLit{}, KindResultGeometry},
		{"array", &ArrayLit{}, KindResultArray},
		{"intersects", &Op{Op: OpSIntersects}, KindResultBoolean},
		{"t_after", &Op{Op: OpTAfter}, KindResultBoolean},
		{"a_contains", &Op{Op: OpAContains}, KindResultBoolean},
	}
	for _, c := range cases {
		got := ResultKind(c.n)
		if got != c.want {
			t.Errorf("%s: ResultKind = %d, want %d", c.name, got, c.want)
		}
	}
}

func TestIsBoolean(t *testing.T) {
	cases := []struct {
		n    Node
		want bool
	}{
		{&BoolLit{}, true},
		{&NumLit{Value: "1"}, false},
		{&Op{Op: OpAnd}, true},
		{&Op{Op: OpAdd}, false},
		{&PropertyRef{Name: "x"}, false},
	}
	for _, c := range cases {
		if got := IsBoolean(c.n); got != c.want {
			t.Errorf("IsBoolean(%T) = %v, want %v", c.n, got, c.want)
		}
	}
}

func TestAllIteratorMatchesWalkCount(t *testing.T) {
	root, expected := buildSampleAST()
	var n int
	for range All(root) {
		n++
	}
	if n != len(expected) {
		t.Fatalf("All iterated %d nodes, want %d", n, len(expected))
	}

	// Early-stop should also work without panic.
	stopAt := 3
	seen := 0
	for range All(root) {
		seen++
		if seen == stopAt {
			break
		}
	}
	if seen != stopAt {
		t.Fatalf("early break visited %d, want %d", seen, stopAt)
	}
}
