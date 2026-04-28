package cql2

import "fmt"

// Pos identifies a location in source input. For text input, Line/Column/Offset
// are populated; for JSON input, JSONPath is populated.
type Pos struct {
	Line, Column int
	JSONPath     string
	Offset       int
}

func (p Pos) String() string {
	if p.JSONPath != "" {
		return p.JSONPath
	}
	if p.Line == 0 && p.Column == 0 {
		return fmt.Sprintf("offset %d", p.Offset)
	}
	return fmt.Sprintf("%d:%d", p.Line, p.Column)
}

// PositionMap maps AST nodes to their source positions.
// The zero value is usable but tracks nothing; parsers allocate one
// when WithPositions is supplied.
type PositionMap struct {
	m map[Node]Pos
}

// Of returns the recorded position for n, if any.
func (m *PositionMap) Of(n Node) (Pos, bool) {
	if m == nil || m.m == nil {
		return Pos{}, false
	}
	p, ok := m.m[n]
	return p, ok
}

// Set records a position. Called by parsers in subpackages.
func (m *PositionMap) Set(n Node, p Pos) {
	if m == nil {
		return
	}
	if m.m == nil {
		m.m = make(map[Node]Pos)
	}
	m.m[n] = p
}
