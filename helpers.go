package cql2

// Kind classifies the runtime result type of evaluating a Node.
// This is distinct from NodeKind, which classifies the AST shape.
type Kind uint8

const (
	KindUnknown Kind = iota
	KindResultBoolean
	KindResultNumber
	KindResultString
	KindResultTemporal
	KindResultGeometry
	KindResultArray
)

// ResultKind reports the result kind of evaluating n.
func ResultKind(n Node) Kind {
	panic("not implemented")
}

// IsBoolean reports whether n evaluates to a boolean.
func IsBoolean(n Node) bool {
	return ResultKind(n) == KindResultBoolean
}

// Children returns the direct child nodes of n in source order.
func Children(n Node) []Node {
	return nil
}

// Equal reports structural equality of two AST nodes.
func Equal(a, b Node) bool {
	panic("not implemented")
}

// Clone returns a deep copy of n.
func Clone(n Node) Node {
	panic("not implemented")
}
