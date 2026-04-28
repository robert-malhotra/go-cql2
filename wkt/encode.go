package wkt

import (
	"fmt"
	"strconv"
	"strings"

	cql2 "github.com/example/go-cql2"
)

// Encode serialises a cql2.Geometry to its canonical WKT representation.
func Encode(g cql2.Geometry) (string, error) {
	if g == nil {
		return "", encodeError("nil geometry")
	}
	var b strings.Builder
	if err := writeGeometry(&b, g); err != nil {
		return "", err
	}
	return b.String(), nil
}

func encodeError(msg string, args ...any) error {
	if len(args) > 0 {
		msg = fmt.Sprintf(msg, args...)
	}
	return &cql2.GeometryError{
		Encoding: cql2.EncodingText,
		Msg:      msg,
	}
}

func writeGeometry(b *strings.Builder, g cql2.Geometry) error {
	switch v := g.(type) {
	case *cql2.Point:
		return writePoint(b, v)
	case *cql2.LineString:
		return writeLineString(b, v)
	case *cql2.Polygon:
		return writePolygon(b, v)
	case *cql2.MultiPoint:
		return writeMultiPoint(b, v)
	case *cql2.MultiLineStr:
		return writeMultiLineString(b, v)
	case *cql2.MultiPolygon:
		return writeMultiPolygon(b, v)
	case *cql2.GeometryColl:
		return writeGeometryCollection(b, v)
	default:
		return encodeError("unsupported geometry type %T", g)
	}
}

func writePoint(b *strings.Builder, p *cql2.Point) error {
	if p == nil {
		return encodeError("nil Point")
	}
	b.WriteString("POINT")
	if p.Empty {
		b.WriteString(" EMPTY")
		return nil
	}
	b.WriteByte('(')
	writeCoord(b, p.Coord)
	b.WriteByte(')')
	return nil
}

func writeLineString(b *strings.Builder, ls *cql2.LineString) error {
	if ls == nil {
		return encodeError("nil LineString")
	}
	if len(ls.Coords) < 2 {
		return encodeError("LINESTRING requires at least 2 points")
	}
	b.WriteString("LINESTRING")
	writeCoordList(b, ls.Coords)
	return nil
}

func writePolygon(b *strings.Builder, pg *cql2.Polygon) error {
	if pg == nil {
		return encodeError("nil Polygon")
	}
	if len(pg.Rings) == 0 {
		return encodeError("POLYGON requires at least 1 ring")
	}
	for i, r := range pg.Rings {
		if len(r) < 4 {
			return encodeError("polygon ring %d requires at least 4 points", i)
		}
	}
	b.WriteString("POLYGON")
	writeRings(b, pg.Rings)
	return nil
}

func writeMultiPoint(b *strings.Builder, mp *cql2.MultiPoint) error {
	if mp == nil {
		return encodeError("nil MultiPoint")
	}
	b.WriteString("MULTIPOINT")
	if len(mp.Points) == 0 {
		b.WriteString(" EMPTY")
		return nil
	}
	b.WriteByte('(')
	for i, pt := range mp.Points {
		if i > 0 {
			b.WriteString(", ")
		}
		if pt.Empty {
			return encodeError("MULTIPOINT cannot contain POINT EMPTY")
		}
		b.WriteByte('(')
		writeCoord(b, pt.Coord)
		b.WriteByte(')')
	}
	b.WriteByte(')')
	return nil
}

func writeMultiLineString(b *strings.Builder, ml *cql2.MultiLineStr) error {
	if ml == nil {
		return encodeError("nil MultiLineString")
	}
	b.WriteString("MULTILINESTRING")
	if len(ml.Lines) == 0 {
		b.WriteString(" EMPTY")
		return nil
	}
	b.WriteByte('(')
	for i, ls := range ml.Lines {
		if i > 0 {
			b.WriteString(", ")
		}
		if len(ls.Coords) < 2 {
			return encodeError("LINESTRING requires at least 2 points")
		}
		writeCoordList(b, ls.Coords)
	}
	b.WriteByte(')')
	return nil
}

func writeMultiPolygon(b *strings.Builder, mp *cql2.MultiPolygon) error {
	if mp == nil {
		return encodeError("nil MultiPolygon")
	}
	b.WriteString("MULTIPOLYGON")
	if len(mp.Polys) == 0 {
		b.WriteString(" EMPTY")
		return nil
	}
	b.WriteByte('(')
	for i, pg := range mp.Polys {
		if i > 0 {
			b.WriteString(", ")
		}
		if len(pg.Rings) == 0 {
			return encodeError("POLYGON requires at least 1 ring")
		}
		for j, r := range pg.Rings {
			if len(r) < 4 {
				return encodeError("polygon ring %d requires at least 4 points", j)
			}
		}
		writeRings(b, pg.Rings)
	}
	b.WriteByte(')')
	return nil
}

func writeGeometryCollection(b *strings.Builder, gc *cql2.GeometryColl) error {
	if gc == nil {
		return encodeError("nil GeometryCollection")
	}
	b.WriteString("GEOMETRYCOLLECTION")
	if len(gc.Geoms) == 0 {
		b.WriteString(" EMPTY")
		return nil
	}
	b.WriteByte('(')
	for i, g := range gc.Geoms {
		if i > 0 {
			b.WriteString(", ")
		}
		if err := writeGeometry(b, g); err != nil {
			return err
		}
	}
	b.WriteByte(')')
	return nil
}

func writeRings(b *strings.Builder, rings [][]cql2.Coord) {
	b.WriteByte('(')
	for i, r := range rings {
		if i > 0 {
			b.WriteString(", ")
		}
		writeCoordList(b, r)
	}
	b.WriteByte(')')
}

func writeCoordList(b *strings.Builder, coords []cql2.Coord) {
	b.WriteByte('(')
	for i, c := range coords {
		if i > 0 {
			b.WriteString(", ")
		}
		writeCoord(b, c)
	}
	b.WriteByte(')')
}

func writeCoord(b *strings.Builder, c cql2.Coord) {
	b.WriteString(strconv.FormatFloat(c.X, 'g', -1, 64))
	b.WriteByte(' ')
	b.WriteString(strconv.FormatFloat(c.Y, 'g', -1, 64))
	if c.HasZ {
		b.WriteByte(' ')
		b.WriteString(strconv.FormatFloat(c.Z, 'g', -1, 64))
	}
}
