package cql2

import "reflect"

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
	if n == nil {
		return KindUnknown
	}
	switch x := n.(type) {
	case *BoolLit:
		return KindResultBoolean
	case *NumLit:
		return KindResultNumber
	case *StringLit:
		return KindResultString
	case *NullLit:
		return KindUnknown
	case *TimestampLit, *DateLit, *IntervalLit:
		return KindResultTemporal
	case *GeomLit, *BBoxLit:
		return KindResultGeometry
	case *ArrayLit:
		return KindResultArray
	case *PropertyRef, *FunctionCall:
		return KindUnknown
	case *Op:
		return opResultKind(x)
	}
	return KindUnknown
}

func opResultKind(x *Op) Kind {
	switch x.Op {
	case OpAnd, OpOr, OpNot,
		OpEq, OpNeq, OpLt, OpLte, OpGt, OpGte,
		OpLike, OpBetween, OpIn, OpIsNull,
		OpSIntersects, OpSEquals, OpSDisjoint, OpSTouches,
		OpSWithin, OpSOverlaps, OpSCrosses, OpSContains,
		OpTAfter, OpTBefore, OpTContains, OpTDisjoint,
		OpTDuring, OpTEquals, OpTFinishedBy, OpTFinishes,
		OpTIntersects, OpTMeets, OpTMetBy, OpTOverlappedBy,
		OpTOverlaps, OpTStartedBy, OpTStarts,
		OpAContains, OpAContainedBy, OpAEquals, OpAOverlaps:
		return KindResultBoolean
	case OpAdd, OpSub, OpMul, OpDiv, OpMod, OpPow, OpIDiv:
		return KindResultNumber
	case OpCaseI, OpAccentI:
		if len(x.Args) > 0 {
			return ResultKind(x.Args[0])
		}
		return KindUnknown
	}
	return KindUnknown
}

// IsBoolean reports whether n evaluates to a boolean.
func IsBoolean(n Node) bool {
	return ResultKind(n) == KindResultBoolean
}

// Children returns the direct child nodes of n in source order.
// The returned slice mirrors what Walk descends into; leaves return nil.
func Children(n Node) []Node {
	if n == nil {
		return nil
	}
	switch x := n.(type) {
	case *Op:
		if len(x.Args) == 0 {
			return nil
		}
		out := make([]Node, len(x.Args))
		copy(out, x.Args)
		return out
	case *FunctionCall:
		if len(x.Args) == 0 {
			return nil
		}
		out := make([]Node, len(x.Args))
		copy(out, x.Args)
		return out
	case *ArrayLit:
		if len(x.Elements) == 0 {
			return nil
		}
		out := make([]Node, len(x.Elements))
		copy(out, x.Elements)
		return out
	case *IntervalLit:
		var out []Node
		if n2 := endpointAsNode(x.Start); n2 != nil {
			out = append(out, n2)
		}
		if n2 := endpointAsNode(x.End); n2 != nil {
			out = append(out, n2)
		}
		return out
	}
	return nil
}

// Equal reports structural equality of two AST nodes. Positions in any
// associated PositionMap are not considered.
func Equal(a, b Node) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	if reflect.TypeOf(a) != reflect.TypeOf(b) {
		return false
	}
	switch x := a.(type) {
	case *BoolLit:
		return x.Value == b.(*BoolLit).Value
	case *NumLit:
		// Compare by source string form for fidelity.
		return string(x.Value) == string(b.(*NumLit).Value)
	case *StringLit:
		return x.Value == b.(*StringLit).Value
	case *NullLit:
		return true
	case *TimestampLit:
		return x.Value.Equal(b.(*TimestampLit).Value)
	case *DateLit:
		return x.Value.Equal(b.(*DateLit).Value)
	case *IntervalLit:
		y := b.(*IntervalLit)
		return equalEndpoint(x.Start, y.Start) && equalEndpoint(x.End, y.End)
	case *GeomLit:
		return reflect.DeepEqual(x.Geom, b.(*GeomLit).Geom)
	case *BBoxLit:
		y := b.(*BBoxLit)
		if len(x.Coords) != len(y.Coords) {
			return false
		}
		for i := range x.Coords {
			if x.Coords[i] != y.Coords[i] {
				return false
			}
		}
		return true
	case *ArrayLit:
		y := b.(*ArrayLit)
		if len(x.Elements) != len(y.Elements) {
			return false
		}
		for i := range x.Elements {
			if !Equal(x.Elements[i], y.Elements[i]) {
				return false
			}
		}
		return true
	case *PropertyRef:
		return x.Name == b.(*PropertyRef).Name
	case *Op:
		y := b.(*Op)
		if x.Op != y.Op || len(x.Args) != len(y.Args) {
			return false
		}
		for i := range x.Args {
			if !Equal(x.Args[i], y.Args[i]) {
				return false
			}
		}
		return true
	case *FunctionCall:
		y := b.(*FunctionCall)
		if x.Name != y.Name || len(x.Args) != len(y.Args) {
			return false
		}
		for i := range x.Args {
			if !Equal(x.Args[i], y.Args[i]) {
				return false
			}
		}
		return true
	}
	return false
}

func equalEndpoint(a, b IntervalEndpoint) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	if reflect.TypeOf(a) != reflect.TypeOf(b) {
		return false
	}
	if _, ok := a.(*Unbounded); ok {
		return true
	}
	return Equal(endpointAsNode(a), endpointAsNode(b))
}

// Clone returns a deep copy of n. The result shares no slices or pointers
// with n, so mutating Clone(n) cannot affect the original.
func Clone(n Node) Node {
	if n == nil {
		return nil
	}
	switch x := n.(type) {
	case *BoolLit:
		c := *x
		return &c
	case *NumLit:
		c := *x
		return &c
	case *StringLit:
		c := *x
		return &c
	case *NullLit:
		return &NullLit{}
	case *TimestampLit:
		c := *x
		return &c
	case *DateLit:
		c := *x
		return &c
	case *IntervalLit:
		out := &IntervalLit{}
		out.Start = cloneEndpoint(x.Start)
		out.End = cloneEndpoint(x.End)
		return out
	case *GeomLit:
		return &GeomLit{Geom: cloneGeom(x.Geom)}
	case *BBoxLit:
		coords := make([]float64, len(x.Coords))
		copy(coords, x.Coords)
		return &BBoxLit{Coords: coords}
	case *ArrayLit:
		els := make([]Node, len(x.Elements))
		for i, e := range x.Elements {
			els[i] = Clone(e)
		}
		return &ArrayLit{Elements: els}
	case *PropertyRef:
		c := *x
		return &c
	case *Op:
		args := make([]Node, len(x.Args))
		for i, a := range x.Args {
			args[i] = Clone(a)
		}
		return &Op{Op: x.Op, Args: args}
	case *FunctionCall:
		args := make([]Node, len(x.Args))
		for i, a := range x.Args {
			args[i] = Clone(a)
		}
		return &FunctionCall{Name: x.Name, Args: args}
	}
	return n
}

func cloneEndpoint(e IntervalEndpoint) IntervalEndpoint {
	switch v := e.(type) {
	case nil:
		return nil
	case *TimestampLit:
		c := *v
		return &c
	case *DateLit:
		c := *v
		return &c
	case *Unbounded:
		return &Unbounded{}
	}
	return e
}

func cloneCoords(in []Coord) []Coord {
	if in == nil {
		return nil
	}
	out := make([]Coord, len(in))
	copy(out, in)
	return out
}

func cloneGeom(g Geometry) Geometry {
	if g == nil {
		return nil
	}
	switch x := g.(type) {
	case *Point:
		c := *x
		return &c
	case *LineString:
		return &LineString{Coords: cloneCoords(x.Coords)}
	case *Polygon:
		rings := make([][]Coord, len(x.Rings))
		for i, r := range x.Rings {
			rings[i] = cloneCoords(r)
		}
		return &Polygon{Rings: rings}
	case *MultiPoint:
		pts := make([]Point, len(x.Points))
		copy(pts, x.Points)
		return &MultiPoint{Points: pts}
	case *MultiLineStr:
		lines := make([]LineString, len(x.Lines))
		for i, l := range x.Lines {
			lines[i] = LineString{Coords: cloneCoords(l.Coords)}
		}
		return &MultiLineStr{Lines: lines}
	case *MultiPolygon:
		polys := make([]Polygon, len(x.Polys))
		for i, p := range x.Polys {
			rings := make([][]Coord, len(p.Rings))
			for j, r := range p.Rings {
				rings[j] = cloneCoords(r)
			}
			polys[i] = Polygon{Rings: rings}
		}
		return &MultiPolygon{Polys: polys}
	case *GeometryColl:
		geoms := make([]Geometry, len(x.Geoms))
		for i, gg := range x.Geoms {
			geoms[i] = cloneGeom(gg)
		}
		return &GeometryColl{Geoms: geoms}
	}
	return g
}
