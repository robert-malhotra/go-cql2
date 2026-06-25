package cql2

import "iter"

// Visitor is the interface for Walk's pre-order callback.
//
// Visit is called for each node encountered. If Visit returns a non-nil
// visitor w, Walk descends into n's children with w; after the children
// are walked, w.Visit(nil) is called as a sentinel — useful for stack-style
// visitors that want to know when a subtree is finished. This mirrors
// go/ast.Walk.
type Visitor interface {
	Visit(n Node) Visitor
}

// Walk traverses an AST in pre-order. v.Visit(n) is invoked for n; if it
// returns a non-nil visitor w, Walk recurses on each direct child with w
// and finally calls w.Visit(nil).
//
// This shape mirrors go/ast.Walk.
func Walk(v Visitor, n Node) {
	if v = v.Visit(n); v == nil {
		return
	}
	// Children owns the per-node-type child enumeration; reuse it so the two
	// can't drift (guarded by TestChildrenMatchesWalk).
	for _, c := range Children(n) {
		Walk(v, c)
	}
	v.Visit(nil)
}

// inspector adapts a func(Node) bool to the Visitor interface.
type inspector func(Node) bool

func (f inspector) Visit(n Node) Visitor {
	if f(n) {
		return f
	}
	return nil
}

// Inspect walks n in pre-order, calling f for each node. If f returns
// false on a node, its subtree is skipped. f is also called with nil
// after each subtree is walked, mirroring go/ast.Inspect — predicates
// should be nil-safe.
func Inspect(n Node, f func(Node) bool) {
	Walk(inspector(f), n)
}

// Transform walks n in post-order, invoking f on each node after its
// children have been transformed. If f returns a non-nil node, it
// replaces the original; otherwise the original is kept.
//
// Transform never mutates its input. When children change, a new container
// node (e.g. *Op, *FunctionCall, *ArrayLit, *IntervalLit) is allocated with
// fresh slices. When no descendant changes and f returns nil at every
// level, Transform returns the original n pointer (the no-op identity
// guarantee).
func Transform(n Node, f func(Node) Node) Node {
	if n == nil {
		return nil
	}
	var rebuilt Node
	switch x := n.(type) {
	case *Op:
		if args, changed := transformSlice(x.Args, f); changed {
			rebuilt = &Op{Op: x.Op, Args: args}
		} else {
			rebuilt = x
		}
	case *FunctionCall:
		if args, changed := transformSlice(x.Args, f); changed {
			rebuilt = &FunctionCall{Name: x.Name, Args: args}
		} else {
			rebuilt = x
		}
	case *ArrayLit:
		if els, changed := transformSlice(x.Elements, f); changed {
			rebuilt = &ArrayLit{Elements: els}
		} else {
			rebuilt = x
		}
	case *IntervalLit:
		ns := Transform(x.Start, f)
		ne := Transform(x.End, f)
		if ns != x.Start || ne != x.End {
			rebuilt = &IntervalLit{Start: ns, End: ne}
		} else {
			rebuilt = x
		}
	default:
		rebuilt = n
	}
	if r := f(rebuilt); r != nil {
		return r
	}
	return rebuilt
}

// transformSlice applies Transform(_, f) to each node in in, allocating a new
// slice only when some element actually changes. It returns (in, false) when
// nothing changed, preserving Transform's no-op identity guarantee.
func transformSlice(in []Node, f func(Node) Node) ([]Node, bool) {
	var out []Node
	for i, a := range in {
		na := Transform(a, f)
		if na != a && out == nil {
			out = make([]Node, len(in))
			copy(out, in[:i])
		}
		if out != nil {
			out[i] = na
		}
	}
	return out, out != nil
}

// All returns an iterator yielding every node in n in pre-order.
// Requires Go 1.23+.
func All(n Node) iter.Seq[Node] {
	return func(yield func(Node) bool) {
		allWalk(n, yield)
	}
}

// allWalk is the recursive helper for All; returns false if iteration
// was stopped early.
func allWalk(n Node, yield func(Node) bool) bool {
	if n == nil {
		return true
	}
	if !yield(n) {
		return false
	}
	for _, c := range Children(n) {
		if !allWalk(c, yield) {
			return false
		}
	}
	return true
}
