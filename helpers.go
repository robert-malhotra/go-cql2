package cql2

import (
	"encoding/json"
	"reflect"
	"slices"
	"time"

	"github.com/exergy-dev/go-topology-suite/geom"
)

// FormatTimestamp formats t in RFC 3339 form for use as a CQL2 timestamp
// literal. Sub-second precision is preserved when present; the zero-nanosecond
// case emits the shorter `YYYY-MM-DDTHH:MM:SSZ` form. Both encoders share
// this so cross-encoding round-trip is byte-stable.
func FormatTimestamp(t time.Time) string {
	t = t.UTC()
	if t.Nanosecond() == 0 {
		return t.Format("2006-01-02T15:04:05Z")
	}
	return t.Format(time.RFC3339Nano)
}

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
	case *PropertyRef, *FunctionCall, *Unbounded:
		return KindUnknown
	case *Op:
		return opResultKind(x)
	}
	return KindUnknown
}

func opResultKind(x *Op) Kind {
	m := opTable[x.Op]
	if m.passthru {
		// CASEI / ACCENTI inherit the result kind of their operand.
		if len(x.Args) > 0 {
			return ResultKind(x.Args[0])
		}
		return KindUnknown
	}
	return m.result
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
		return slices.Clone(x.Args)
	case *FunctionCall:
		return slices.Clone(x.Args)
	case *ArrayLit:
		return slices.Clone(x.Elements)
	case *IntervalLit:
		var out []Node
		if x.Start != nil {
			out = append(out, x.Start)
		}
		if x.End != nil {
			out = append(out, x.End)
		}
		return out
	}
	return nil
}

// Equal reports structural equality of two AST nodes with value-based
// semantics for numeric literals: NumLit("1.0") and NumLit("1") are equal.
// Use EqualVerbatim to compare the source-string spellings of NumLits.
// Positions in any associated PositionMap are not considered.
func Equal(a, b Node) bool {
	return equal(a, b, false)
}

// EqualVerbatim reports structural equality with source-string-sensitive
// numeric comparison: NumLit("1.0") and NumLit("1") are NOT equal. Use this
// when round-tripping requires byte-identical numeric spellings; use Equal
// for the more common "do these mean the same thing" check.
func EqualVerbatim(a, b Node) bool {
	return equal(a, b, true)
}

func equal(a, b Node, verbatim bool) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	if reflect.TypeOf(a) != reflect.TypeOf(b) {
		return false
	}
	eq := func(p, q Node) bool { return equal(p, q, verbatim) }
	switch x := a.(type) {
	case *BoolLit:
		return x.Value == b.(*BoolLit).Value
	case *NumLit:
		return numLitEqual(x.Value, b.(*NumLit).Value, verbatim)
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
		return equal(x.Start, y.Start, verbatim) && equal(x.End, y.End, verbatim)
	case *Unbounded:
		return true
	case *GeomLit:
		return geomEqual(x.Geom, b.(*GeomLit).Geom)
	case *BBoxLit:
		return slices.Equal(x.Coords, b.(*BBoxLit).Coords)
	case *ArrayLit:
		return slices.EqualFunc(x.Elements, b.(*ArrayLit).Elements, eq)
	case *PropertyRef:
		return x.Name == b.(*PropertyRef).Name
	case *Op:
		y := b.(*Op)
		return x.Op == y.Op && slices.EqualFunc(x.Args, y.Args, eq)
	case *FunctionCall:
		y := b.(*FunctionCall)
		return x.Name == y.Name && slices.EqualFunc(x.Args, y.Args, eq)
	}
	return false
}

// numLitEqual compares two json.Number literals. Verbatim mode requires
// byte-identical source spellings; otherwise the values are compared
// numerically: integers via Int64 (covers 64-bit precision exactly) and
// otherwise via Float64 (subject to IEEE 754 rounding for very large or
// high-precision decimals — acceptable for filter equality).
func numLitEqual(a, b json.Number, verbatim bool) bool {
	if string(a) == string(b) {
		return true
	}
	if verbatim {
		return false
	}
	if ai, aerr := a.Int64(); aerr == nil {
		if bi, berr := b.Int64(); berr == nil {
			return ai == bi
		}
	}
	af, aerr := a.Float64()
	bf, berr := b.Float64()
	return aerr == nil && berr == nil && af == bf
}

// Clone returns a deep copy of n. The result shares no slices or pointers
// with n, so mutating Clone(n) cannot affect the original.
func Clone(n Node) Node {
	if n == nil {
		return nil
	}
	switch x := n.(type) {
	case *BoolLit:
		return clonePtr(x)
	case *NumLit:
		return clonePtr(x)
	case *StringLit:
		return clonePtr(x)
	case *NullLit:
		return &NullLit{}
	case *TimestampLit:
		return clonePtr(x)
	case *DateLit:
		return clonePtr(x)
	case *IntervalLit:
		return &IntervalLit{Start: Clone(x.Start), End: Clone(x.End)}
	case *Unbounded:
		return &Unbounded{}
	case *GeomLit:
		// gts geometries are immutable after construction, so the same
		// pointer is safe to share across cloned ASTs.
		return &GeomLit{Geom: x.Geom}
	case *BBoxLit:
		return &BBoxLit{Coords: slices.Clone(x.Coords)}
	case *ArrayLit:
		return &ArrayLit{Elements: mapSlice(x.Elements, Clone)}
	case *PropertyRef:
		return clonePtr(x)
	case *Op:
		return &Op{Op: x.Op, Args: mapSlice(x.Args, Clone)}
	case *FunctionCall:
		return &FunctionCall{Name: x.Name, Args: mapSlice(x.Args, Clone)}
	}
	return n
}

// geomEqual compares two gts geometries structurally — same concrete type,
// same layout, same flat coordinates, same children for collection types.
// Cannot use reflect.DeepEqual because baseGeom embeds an atomic.Pointer
// for the lazy envelope cache whose contents diverge after first read.
func geomEqual(a, b geom.Geometry) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	if a.Type() != b.Type() || a.Layout() != b.Layout() {
		return false
	}
	switch x := a.(type) {
	case *geom.Point:
		y := b.(*geom.Point)
		if x.IsEmpty() != y.IsEmpty() {
			return false
		}
		return slices.Equal(x.FlatCoords(), y.FlatCoords())
	case *geom.LineString:
		return slices.Equal(x.FlatCoords(), b.(*geom.LineString).FlatCoords())
	case *geom.LinearRing:
		return slices.Equal(x.FlatCoords(), b.(*geom.LinearRing).FlatCoords())
	case *geom.Polygon:
		y := b.(*geom.Polygon)
		if x.NumRings() != y.NumRings() {
			return false
		}
		for i := 0; i < x.NumRings(); i++ {
			if !slices.Equal(x.Ring(i), y.Ring(i)) {
				return false
			}
		}
		return true
	case *geom.MultiPoint:
		return slices.Equal(x.FlatCoords(), b.(*geom.MultiPoint).FlatCoords())
	case *geom.MultiLineString:
		y := b.(*geom.MultiLineString)
		if x.NumGeometries() != y.NumGeometries() {
			return false
		}
		for i := 0; i < x.NumGeometries(); i++ {
			if !geomEqual(x.LineStringAt(i), y.LineStringAt(i)) {
				return false
			}
		}
		return true
	case *geom.MultiPolygon:
		y := b.(*geom.MultiPolygon)
		if x.NumGeometries() != y.NumGeometries() {
			return false
		}
		for i := 0; i < x.NumGeometries(); i++ {
			if !geomEqual(x.PolygonAt(i), y.PolygonAt(i)) {
				return false
			}
		}
		return true
	case *geom.GeometryCollection:
		y := b.(*geom.GeometryCollection)
		if x.NumGeometries() != y.NumGeometries() {
			return false
		}
		for i := 0; i < x.NumGeometries(); i++ {
			if !geomEqual(x.GeometryAt(i), y.GeometryAt(i)) {
				return false
			}
		}
		return true
	}
	return reflect.DeepEqual(a, b)
}
