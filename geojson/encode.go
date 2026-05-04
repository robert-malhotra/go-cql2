package geojson

import (
	"bytes"
	"fmt"
	"strconv"

	cql2 "github.com/exergy-dev/go-cql2"
)

// Encode serializes a cql2.Geometry as compact, deterministic GeoJSON
// conforming to RFC 7946. Object keys are emitted as "type" followed by either
// "coordinates" or, for GeometryCollection, "geometries". Numeric components
// use strconv.FormatFloat with 'g'/-1 precision.
//
// Encoding a Point with Empty=true returns *cql2.GeometryError because
// GeoJSON has no representation for an empty Point.
func Encode(g cql2.Geometry) ([]byte, error) {
	var buf bytes.Buffer
	if err := encodeGeom(&buf, g); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func encodeGeom(buf *bytes.Buffer, g cql2.Geometry) error {
	switch v := g.(type) {
	case *cql2.Point:
		if v.Empty {
			return &cql2.GeometryError{
				Encoding: cql2.EncodingJSON,
				Msg:      "GeoJSON cannot represent an empty Point",
			}
		}
		buf.WriteString(`{"type":"Point","coordinates":`)
		writeCoord(buf, v.Coord)
		buf.WriteByte('}')
		return nil
	case *cql2.LineString:
		buf.WriteString(`{"type":"LineString","coordinates":`)
		writeCoordList(buf, v.Coords)
		buf.WriteByte('}')
		return nil
	case *cql2.Polygon:
		buf.WriteString(`{"type":"Polygon","coordinates":`)
		writeRingList(buf, v.Rings)
		buf.WriteByte('}')
		return nil
	case *cql2.MultiPoint:
		buf.WriteString(`{"type":"MultiPoint","coordinates":[`)
		for i, p := range v.Points {
			if i > 0 {
				buf.WriteByte(',')
			}
			if p.Empty {
				return &cql2.GeometryError{
					Encoding: cql2.EncodingJSON,
					At:       cql2.Pos{JSONPath: fmt.Sprintf("/coordinates/%d", i)},
					Msg:      "GeoJSON cannot represent an empty Point inside MultiPoint",
				}
			}
			writeCoord(buf, p.Coord)
		}
		buf.WriteString(`]}`)
		return nil
	case *cql2.MultiLineString:
		buf.WriteString(`{"type":"MultiLineString","coordinates":[`)
		for i, ls := range v.Lines {
			if i > 0 {
				buf.WriteByte(',')
			}
			writeCoordList(buf, ls.Coords)
		}
		buf.WriteString(`]}`)
		return nil
	case *cql2.MultiPolygon:
		buf.WriteString(`{"type":"MultiPolygon","coordinates":[`)
		for i, p := range v.Polys {
			if i > 0 {
				buf.WriteByte(',')
			}
			writeRingList(buf, p.Rings)
		}
		buf.WriteString(`]}`)
		return nil
	case *cql2.GeometryCollection:
		buf.WriteString(`{"type":"GeometryCollection","geometries":[`)
		for i, child := range v.Geoms {
			if i > 0 {
				buf.WriteByte(',')
			}
			if err := encodeGeom(buf, child); err != nil {
				return err
			}
		}
		buf.WriteString(`]}`)
		return nil
	default:
		return &cql2.GeometryError{
			Encoding: cql2.EncodingJSON,
			Msg:      fmt.Sprintf("unsupported geometry type %T", g),
		}
	}
}

func writeFloat(buf *bytes.Buffer, f float64) {
	buf.WriteString(strconv.FormatFloat(f, 'g', -1, 64))
}

func writeCoord(buf *bytes.Buffer, c cql2.Coord) {
	buf.WriteByte('[')
	writeFloat(buf, c.X)
	buf.WriteByte(',')
	writeFloat(buf, c.Y)
	if c.HasZ {
		buf.WriteByte(',')
		writeFloat(buf, c.Z)
	}
	buf.WriteByte(']')
}

func writeCoordList(buf *bytes.Buffer, cs []cql2.Coord) {
	buf.WriteByte('[')
	for i, c := range cs {
		if i > 0 {
			buf.WriteByte(',')
		}
		writeCoord(buf, c)
	}
	buf.WriteByte(']')
}

func writeRingList(buf *bytes.Buffer, rings [][]cql2.Coord) {
	buf.WriteByte('[')
	for i, r := range rings {
		if i > 0 {
			buf.WriteByte(',')
		}
		writeCoordList(buf, r)
	}
	buf.WriteByte(']')
}
