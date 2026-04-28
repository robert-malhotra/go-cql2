// Package cql2 implements parsing and encoding of OGC CQL2 filter expressions
// in both Text and JSON encodings.
package cql2

// Parse auto-detects the encoding and returns an AST.
func Parse(input []byte, opts ...Option) (Node, error) {
	panic("not implemented")
}

// MustParse panics on error. Convenience wrapper around Parse.
func MustParse(input string, opts ...Option) Node {
	n, err := Parse([]byte(input), opts...)
	if err != nil {
		panic(err)
	}
	return n
}

// Encode emits the AST in the given encoding.
func Encode(n Node, enc Encoding) ([]byte, error) {
	panic("not implemented")
}
