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
	switch x := n.(type) {
	case *Op:
		for _, a := range x.Args {
			Walk(v, a)
		}
	case *FunctionCall:
		for _, a := range x.Args {
			Walk(v, a)
		}
	case *ArrayLit:
		for _, e := range x.Elements {
			Walk(v, e)
		}
	case *IntervalLit:
		if n2 := endpointAsNode(x.Start); n2 != nil {
			Walk(v, n2)
		}
		if n2 := endpointAsNode(x.End); n2 != nil {
			Walk(v, n2)
		}
	case *BoolLit, *NumLit, *StringLit, *NullLit,
		*TimestampLit, *DateLit, *GeomLit, *BBoxLit,
		*PropertyRef:
		// leaves
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
		var newArgs []Node
		changed := false
		for i, a := range x.Args {
			na := Transform(a, f)
			if na != a && !changed {
				newArgs = make([]Node, len(x.Args))
				copy(newArgs, x.Args[:i])
				changed = true
			}
			if changed {
				newArgs[i] = na
			}
		}
		if changed {
			rebuilt = &Op{Op: x.Op, Args: newArgs}
		} else {
			rebuilt = x
		}
	case *FunctionCall:
		var newArgs []Node
		changed := false
		for i, a := range x.Args {
			na := Transform(a, f)
			if na != a && !changed {
				newArgs = make([]Node, len(x.Args))
				copy(newArgs, x.Args[:i])
				changed = true
			}
			if changed {
				newArgs[i] = na
			}
		}
		if changed {
			rebuilt = &FunctionCall{Name: x.Name, Args: newArgs}
		} else {
			rebuilt = x
		}
	case *ArrayLit:
		var newEls []Node
		changed := false
		for i, e := range x.Elements {
			ne := Transform(e, f)
			if ne != e && !changed {
				newEls = make([]Node, len(x.Elements))
				copy(newEls, x.Elements[:i])
				changed = true
			}
			if changed {
				newEls[i] = ne
			}
		}
		if changed {
			rebuilt = &ArrayLit{Elements: newEls}
		} else {
			rebuilt = x
		}
	case *IntervalLit:
		var ns, ne IntervalEndpoint = x.Start, x.End
		changed := false
		if sn := endpointAsNode(x.Start); sn != nil {
			t := Transform(sn, f)
			if t != sn {
				if ep, ok := t.(IntervalEndpoint); ok {
					ns = ep
					changed = true
				}
			}
		}
		if en := endpointAsNode(x.End); en != nil {
			t := Transform(en, f)
			if t != en {
				if ep, ok := t.(IntervalEndpoint); ok {
					ne = ep
					changed = true
				}
			}
		}
		if changed {
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

// All returns an iterator yielding every node in n in pre-order.
// Requires Go 1.23+.
func All(n Node) iter.Seq[Node] {
	return func(yield func(Node) bool) {
		allWalk(n, yield)
	}
}

// endpointAsNode returns the Node view of an IntervalEndpoint, or nil
// for nil/*Unbounded endpoints (which carry no AST identity to visit).
func endpointAsNode(e IntervalEndpoint) Node {
	switch v := e.(type) {
	case *TimestampLit:
		return v
	case *DateLit:
		return v
	}
	return nil
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
