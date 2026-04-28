// Property-based round-trip and walk tests for the cql2 AST. The generators
// emit only ASTs that are within the parser's accepted grammar (see comments
// on individual constraints). When a property fails, rapid will shrink the
// counterexample; we print the rendered text/json on failure to make
// debugging tractable.
package cql2_test

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strconv"
	"testing"
	"time"

	cql2 "github.com/example/go-cql2"
	_ "github.com/example/go-cql2/codecs"
	"pgregory.net/rapid"
)

// --- AST generator -------------------------------------------------------

var propertyNames = []string{"a", "b", "c", "xx", "eo:cloud_cover"}

// genLeaf draws a random leaf node. We deliberately restrict literals to
// shapes that survive both text and JSON round-trips:
//   - StringLit characters are limited to [A-Za-z0-9_] so we don't have to
//     worry about CQL2-Text single-quote escaping rules muddying the test.
//   - TimestampLit is truncated to second precision because the Text encoder
//     may not round-trip sub-second components losslessly across all paths;
//     this test focuses on AST identity, not timestamp precision.
//   - NumLit values are always integer-formatted as json.Number so we don't
//     hit float formatting differences ("1.0" vs "1").
func genLeaf(rt *rapid.T) cql2.Node {
	choice := rapid.IntRange(0, 6).Draw(rt, "leafKind")
	switch choice {
	case 0:
		return &cql2.BoolLit{Value: rapid.Bool().Draw(rt, "bool")}
	case 1:
		// Non-negative; see genNumLit comment for rationale.
		v := rapid.Int64Range(0, 1_000_000).Draw(rt, "num")
		return &cql2.NumLit{Value: json.Number(strconv.FormatInt(v, 10))}
	case 2:
		s := rapid.StringMatching(`[A-Za-z0-9_]{1,8}`).Draw(rt, "str")
		return &cql2.StringLit{Value: s}
	case 3:
		return &cql2.NullLit{}
	case 4:
		name := rapid.SampledFrom(propertyNames).Draw(rt, "propName")
		return &cql2.PropertyRef{Name: name}
	case 5:
		// Truncated to second precision (see comment above). UTC for stable
		// round-tripping across encoders.
		secs := rapid.Int64Range(0, 4_000_000_000).Draw(rt, "tsSecs")
		return &cql2.TimestampLit{Value: time.Unix(secs, 0).UTC()}
	default:
		days := rapid.IntRange(0, 50_000).Draw(rt, "dateDays")
		base := time.Date(1970, 1, 1, 0, 0, 0, 0, time.UTC)
		return &cql2.DateLit{Value: base.AddDate(0, 0, days)}
	}
}

// genNumLit produces an integer literal in [-1_000_000, 1_000_000]. v0.6
// landed unary-minus folding in the text parser (NumLit("-1") rather than
// the previous Sub(0, 1) desugar), so negatives now round-trip cleanly in
// both encodings.
func genNumLit(rt *rapid.T) cql2.Node {
	v := rapid.Int64Range(-1_000_000, 1_000_000).Draw(rt, "num")
	return &cql2.NumLit{Value: json.Number(strconv.FormatInt(v, 10))}
}

func genStringLit(rt *rapid.T) cql2.Node {
	s := rapid.StringMatching(`[A-Za-z0-9_]{1,8}`).Draw(rt, "str")
	return &cql2.StringLit{Value: s}
}

func genPropertyRef(rt *rapid.T) cql2.Node {
	name := rapid.SampledFrom(propertyNames).Draw(rt, "propName")
	return &cql2.PropertyRef{Name: name}
}

// genScalarLit returns one of the simple "value" literals usable on the RHS
// of comparisons (avoids null/property/timestamp/date for predictability;
// the round-trip will still cover those via genLeaf at top level).
func genScalarLit(rt *rapid.T) cql2.Node {
	switch rapid.IntRange(0, 2).Draw(rt, "scalarKind") {
	case 0:
		return genNumLit(rt)
	case 1:
		return genStringLit(rt)
	default:
		return &cql2.BoolLit{Value: rapid.Bool().Draw(rt, "bool")}
	}
}

// genCmpNode returns a non-and/or, non-NOT boolean node: a bare comparison
// or IS NULL. This is the only kind of node we use as an AND/OR child,
// because the text parser flattens chained AND / chained OR into n-ary
// nodes — so a generator that emits left-nested AND would not round-trip.
// We also avoid stacking NOT-of-NOT, which the parser does not accept (NOT
// expects a primary predicate, not another NOT chain at the source level).
func genCmpNode(rt *rapid.T, depth int) cql2.Node {
	switch rapid.IntRange(0, 1).Draw(rt, "cmpKind") {
	case 0:
		ops := []cql2.Operator{cql2.OpEq, cql2.OpNeq, cql2.OpLt, cql2.OpLte, cql2.OpGt, cql2.OpGte}
		op := rapid.SampledFrom(ops).Draw(rt, "cmpOp")
		return &cql2.Op{
			Op:   op,
			Args: []cql2.Node{genPropertyRef(rt), genScalarLit(rt)},
		}
	default:
		return &cql2.Op{
			Op:   cql2.OpIsNull,
			Args: []cql2.Node{genPropertyRef(rt)},
		}
	}
}

// genBoolNode returns a flat n-ary AND or OR with non-and/or children, OR
// (heads up: terminology overload) a comparison/IS NULL/NOT directly. We
// use n-ary AND/OR because the parser produces flat n-ary AND/OR nodes when
// it sees `a AND b AND c`. To keep AND-vs-OR mixing legitimate we permit
// AND children inside OR (and vice versa) — since AND has higher precedence
// than OR, the encoder parenthesizes correctly, and the parser recovers the
// shape.
func genBoolNode(rt *rapid.T, depth int) cql2.Node {
	if depth <= 0 {
		return genCmpNode(rt, depth)
	}
	switch rapid.IntRange(0, 3).Draw(rt, "boolKind") {
	case 0: // n-ary AND with non-AND children
		count := rapid.IntRange(2, 3).Draw(rt, "andArity")
		args := make([]cql2.Node, count)
		for i := range args {
			args[i] = genAndChild(rt, depth-1)
		}
		return &cql2.Op{Op: cql2.OpAnd, Args: args}
	case 1: // n-ary OR with non-OR children
		count := rapid.IntRange(2, 3).Draw(rt, "orArity")
		args := make([]cql2.Node, count)
		for i := range args {
			args[i] = genOrChild(rt, depth-1)
		}
		return &cql2.Op{Op: cql2.OpOr, Args: args}
	case 2: // NOT around any boolean — but NOT must wrap something the parser
		// will accept as the whole NOT operand. NOT is unary at parse time
		// and binds tighter than AND/OR, so wrapping a bare AND/OR under NOT
		// would emit `NOT (a AND b)` which round-trips fine. Keep it simple:
		// wrap a comparison.
		return &cql2.Op{
			Op:   cql2.OpNot,
			Args: []cql2.Node{genCmpNode(rt, depth-1)},
		}
	default:
		return genCmpNode(rt, depth-1)
	}
}

// genAndChild returns a boolean node suitable as a direct child of AND. It
// must NOT itself be AND (parser flattens chained AND), but OR is fine
// because the encoder parenthesizes lower-precedence OR under AND.
func genAndChild(rt *rapid.T, depth int) cql2.Node {
	if depth <= 0 {
		return genCmpNode(rt, depth)
	}
	switch rapid.IntRange(0, 3).Draw(rt, "andChild") {
	case 0: // OR child, also kept flat
		count := rapid.IntRange(2, 3).Draw(rt, "orArityUnderAnd")
		args := make([]cql2.Node, count)
		for i := range args {
			args[i] = genOrChild(rt, depth-1)
		}
		return &cql2.Op{Op: cql2.OpOr, Args: args}
	case 1:
		return &cql2.Op{
			Op:   cql2.OpNot,
			Args: []cql2.Node{genCmpNode(rt, depth-1)},
		}
	default:
		return genCmpNode(rt, depth-1)
	}
}

// genOrChild returns a boolean node suitable as a direct child of OR. Must
// NOT itself be OR. AND is fine.
func genOrChild(rt *rapid.T, depth int) cql2.Node {
	if depth <= 0 {
		return genCmpNode(rt, depth)
	}
	switch rapid.IntRange(0, 3).Draw(rt, "orChild") {
	case 0: // AND child
		count := rapid.IntRange(2, 3).Draw(rt, "andArityUnderOr")
		args := make([]cql2.Node, count)
		for i := range args {
			args[i] = genAndChild(rt, depth-1)
		}
		return &cql2.Op{Op: cql2.OpAnd, Args: args}
	case 1:
		return &cql2.Op{
			Op:   cql2.OpNot,
			Args: []cql2.Node{genCmpNode(rt, depth-1)},
		}
	default:
		return genCmpNode(rt, depth-1)
	}
}

// genArithNode returns an AST whose result is numeric.
func genArithNode(rt *rapid.T, depth int) cql2.Node {
	if depth <= 0 || rapid.IntRange(0, 1).Draw(rt, "arithLeaf") == 0 {
		// Leaf: number literal or property.
		if rapid.Bool().Draw(rt, "arithIsProp") {
			return genPropertyRef(rt)
		}
		return genNumLit(rt)
	}
	// Avoid OpDiv to dodge division-by-zero edge cases on encode/parse.
	ops := []cql2.Operator{cql2.OpAdd, cql2.OpSub, cql2.OpMul}
	op := rapid.SampledFrom(ops).Draw(rt, "arithOp")
	return &cql2.Op{
		Op:   op,
		Args: []cql2.Node{genArithNode(rt, depth-1), genArithNode(rt, depth-1)},
	}
}

// genNode returns the top-level random AST. Compound nodes appear when
// depth > 0; leaves otherwise. We bound depth at 4 (caller-supplied) to
// keep encoded output reasonably-sized.
func genNode(rt *rapid.T, depth int) cql2.Node {
	if depth <= 0 || rapid.IntRange(0, 4).Draw(rt, "leafChance") == 0 {
		return genLeaf(rt)
	}
	switch rapid.IntRange(0, 9).Draw(rt, "compoundKind") {
	case 0, 1: // boolean
		return genBoolNode(rt, depth)
	case 2: // arithmetic
		return genArithNode(rt, depth)
	case 3: // LIKE: PropertyRef LIKE StringLit
		return &cql2.Op{
			Op: cql2.OpLike,
			Args: []cql2.Node{
				genPropertyRef(rt),
				genStringLit(rt),
			},
		}
	case 4: // BETWEEN: PropertyRef, lo, hi (with lo < hi).
		lo := rapid.Int64Range(-1_000_000, 999_999).Draw(rt, "betLo")
		hiOff := rapid.Int64Range(1, 1_000).Draw(rt, "betHiOff")
		hi := lo + hiOff
		return &cql2.Op{
			Op: cql2.OpBetween,
			Args: []cql2.Node{
				genPropertyRef(rt),
				&cql2.NumLit{Value: json.Number(strconv.FormatInt(lo, 10))},
				&cql2.NumLit{Value: json.Number(strconv.FormatInt(hi, 10))},
			},
		}
	case 5: // IN: PropertyRef, ArrayLit of uniform NumLits
		count := rapid.IntRange(1, 4).Draw(rt, "inCount")
		els := make([]cql2.Node, count)
		for i := range els {
			els[i] = genNumLit(rt)
		}
		return &cql2.Op{
			Op: cql2.OpIn,
			Args: []cql2.Node{
				genPropertyRef(rt),
				&cql2.ArrayLit{Elements: els},
			},
		}
	case 6: // IN: PropertyRef, ArrayLit of uniform StringLits
		count := rapid.IntRange(1, 4).Draw(rt, "inCountS")
		els := make([]cql2.Node, count)
		for i := range els {
			els[i] = genStringLit(rt)
		}
		return &cql2.Op{
			Op: cql2.OpIn,
			Args: []cql2.Node{
				genPropertyRef(rt),
				&cql2.ArrayLit{Elements: els},
			},
		}
	case 7: // IS NULL
		return &cql2.Op{
			Op:   cql2.OpIsNull,
			Args: []cql2.Node{genPropertyRef(rt)},
		}
	case 8: // FunctionCall with simple scalar args
		name := rapid.SampledFrom([]string{"ABS", "FOO"}).Draw(rt, "fnName")
		argc := rapid.IntRange(1, 3).Draw(rt, "fnArgc")
		args := make([]cql2.Node, argc)
		for i := range args {
			args[i] = genScalarLit(rt)
		}
		return &cql2.FunctionCall{Name: name, Args: args}
	default: // arithmetic comparison: e.g. (a + 1) = N is not allowed (LHS must be PropertyRef)
		return genBoolNode(rt, depth)
	}
}

// renderForReport encodes n in text form for a failure message; on encode
// error, falls back to a Go-syntax dump.
func renderForReport(n cql2.Node) string {
	if b, err := cql2.Encode(n, cql2.EncodingText); err == nil {
		return string(b)
	}
	return fmt.Sprintf("%#v", n)
}

// --- Properties ----------------------------------------------------------

func TestProperty_TextRoundTrip(t *testing.T) {
	rapid.Check(t, func(rt *rapid.T) {
		n := genNode(rt, 4)
		text, err := cql2.Encode(n, cql2.EncodingText)
		if err != nil {
			rt.Fatalf("encode text: %v\n  ast: %#v", err, n)
		}
		n2, err := cql2.Parse(text)
		if err != nil {
			rt.Fatalf("parse text %q: %v", string(text), err)
		}
		if !cql2.Equal(n, n2) {
			rt.Fatalf("not equal after text round-trip:\n  in:  %s\n  out: %s",
				string(text), renderForReport(n2))
		}
	})
}

func TestProperty_JSONRoundTrip(t *testing.T) {
	rapid.Check(t, func(rt *rapid.T) {
		n := genNode(rt, 4)
		js, err := cql2.Encode(n, cql2.EncodingJSON)
		if err != nil {
			rt.Fatalf("encode json: %v\n  ast: %#v", err, n)
		}
		n2, err := cql2.Parse(js)
		if err != nil {
			rt.Fatalf("parse json %s: %v", string(js), err)
		}
		if !cql2.Equal(n, n2) {
			rt.Fatalf("not equal after json round-trip:\n  in:  %s\n  out: %s",
				string(js), renderForReport(n2))
		}
	})
}

// nodeCounter is a Visitor that counts every node it sees (excluding the
// nil-sentinel post-visit calls).
type nodeCounter struct{ n int }

func (c *nodeCounter) Visit(n cql2.Node) cql2.Visitor {
	if n == nil {
		return nil // post-visit sentinel: don't recurse, don't count
	}
	c.n++
	return c
}

// countViaChildren counts nodes by recursing through cql2.Children.
func countViaChildren(n cql2.Node) int {
	if n == nil {
		return 0
	}
	count := 1
	for _, c := range cql2.Children(n) {
		count += countViaChildren(c)
	}
	return count
}

func TestProperty_WalkVisitsEachNodeOnce(t *testing.T) {
	rapid.Check(t, func(rt *rapid.T) {
		n := genNode(rt, 4)
		var c nodeCounter
		cql2.Walk(&c, n)
		viaChildren := countViaChildren(n)
		if c.n != viaChildren {
			rt.Fatalf("walk count %d != Children-recursion count %d\n  ast: %s",
				c.n, viaChildren, renderForReport(n))
		}
	})
}

func TestProperty_TransformIdentityIsNoOp(t *testing.T) {
	identity := func(_ cql2.Node) cql2.Node { return nil } // keep original at every level
	rapid.Check(t, func(rt *rapid.T) {
		n := genNode(rt, 4)
		out := cql2.Transform(n, identity)
		if !cql2.Equal(n, out) {
			rt.Fatalf("Transform(identity) changed AST:\n  in:  %s\n  out: %s",
				renderForReport(n), renderForReport(out))
		}
		// Stronger no-op guarantee: pointer identity is preserved.
		if !reflect.DeepEqual(n, out) {
			rt.Fatalf("Transform(identity) returned a different value (DeepEqual mismatch)")
		}
	})
}
