package cql2

import (
	"encoding/json"
	"time"
)

// Node is the root interface implemented by every AST node.
type Node interface {
	Kind() NodeKind
	isNode() // sealed
}

// NodeKind identifies the concrete variant of a Node.
type NodeKind uint8

const (
	KindInvalid NodeKind = iota
	KindBool
	KindNumber
	KindString
	KindNull
	KindTimestamp
	KindDate
	KindInterval
	KindGeometry
	KindBBox
	KindArray
	KindProperty
	KindOp
	KindFunction
)

// BoolLit is a boolean literal.
type BoolLit struct{ Value bool }

// NumLit is a numeric literal preserved as json.Number for fidelity.
type NumLit struct{ Value json.Number }

// StringLit is a string literal.
type StringLit struct{ Value string }

// NullLit is the null literal.
type NullLit struct{}

// TimestampLit is an RFC 3339 timestamp literal.
type TimestampLit struct{ Value time.Time }

// DateLit is a calendar date literal (UTC midnight).
type DateLit struct{ Value time.Time }

// IntervalLit is a temporal interval with two endpoints.
type IntervalLit struct {
	Start IntervalEndpoint
	End   IntervalEndpoint
}

// IntervalEndpoint is implemented by valid endpoint kinds for IntervalLit.
type IntervalEndpoint interface{ isIntervalEndpoint() }

// Unbounded marks an open-ended interval endpoint ("..").
type Unbounded struct{}

// GeomLit wraps a parsed geometry value.
type GeomLit struct{ Geom Geometry }

// BBoxLit is a bounding box literal (4 floats for 2D, 6 for 3D).
type BBoxLit struct{ Coords []float64 }

// ArrayLit is an array literal.
type ArrayLit struct{ Elements []Node }

// PropertyRef references a property by name.
type PropertyRef struct{ Name string }

// Op is an operator application with ordered arguments.
type Op struct {
	Op   Operator
	Args []Node
}

// FunctionCall is a named function invocation with ordered arguments.
type FunctionCall struct {
	Name string
	Args []Node
}

// Kind / isNode implementations (pointer receivers).

func (*BoolLit) Kind() NodeKind      { return KindBool }
func (*BoolLit) isNode()             {}
func (*NumLit) Kind() NodeKind       { return KindNumber }
func (*NumLit) isNode()              {}
func (*StringLit) Kind() NodeKind    { return KindString }
func (*StringLit) isNode()           {}
func (*NullLit) Kind() NodeKind      { return KindNull }
func (*NullLit) isNode()             {}
func (*TimestampLit) Kind() NodeKind { return KindTimestamp }
func (*TimestampLit) isNode()        {}
func (*DateLit) Kind() NodeKind      { return KindDate }
func (*DateLit) isNode()             {}
func (*IntervalLit) Kind() NodeKind  { return KindInterval }
func (*IntervalLit) isNode()         {}
func (*GeomLit) Kind() NodeKind      { return KindGeometry }
func (*GeomLit) isNode()             {}
func (*BBoxLit) Kind() NodeKind      { return KindBBox }
func (*BBoxLit) isNode()             {}
func (*ArrayLit) Kind() NodeKind     { return KindArray }
func (*ArrayLit) isNode()            {}
func (*PropertyRef) Kind() NodeKind  { return KindProperty }
func (*PropertyRef) isNode()         {}
func (*Op) Kind() NodeKind           { return KindOp }
func (*Op) isNode()                  {}
func (*FunctionCall) Kind() NodeKind { return KindFunction }
func (*FunctionCall) isNode()        {}

// IntervalEndpoint markers.

func (*TimestampLit) isIntervalEndpoint() {}
func (*DateLit) isIntervalEndpoint()      {}
func (*Unbounded) isIntervalEndpoint()    {}
func (*PropertyRef) isIntervalEndpoint()  {}
func (*FunctionCall) isIntervalEndpoint() {}
