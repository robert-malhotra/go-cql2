package wkt_test

import (
	"errors"
	"testing"

	cql2 "github.com/exergy-dev/go-cql2"
	"github.com/exergy-dev/go-cql2/wkt"
)

func TestEncodeCanonical(t *testing.T) {
	tests := []struct {
		name string
		in   cql2.Geometry
		want string
	}{
		{"Point2D", &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}}, "POINT(1 2)"},
		{"Point3D", &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2, Z: 3, HasZ: true}}, "POINT(1 2 3)"},
		{"PointEmpty", &cql2.Point{Empty: true}, "POINT EMPTY"},
		{
			"LineString",
			&cql2.LineString{Coords: []cql2.Coord{{X: 1, Y: 2}, {X: 3, Y: 4}}},
			"LINESTRING(1 2, 3 4)",
		},
		{
			"Polygon",
			&cql2.Polygon{Rings: [][]cql2.Coord{{{X: 0, Y: 0}, {X: 1, Y: 0}, {X: 1, Y: 1}, {X: 0, Y: 0}}}},
			"POLYGON((0 0, 1 0, 1 1, 0 0))",
		},
		{
			"MultiPointCanonicalForm",
			&cql2.MultiPoint{Points: []cql2.Point{
				{Coord: cql2.Coord{X: 1, Y: 2}},
				{Coord: cql2.Coord{X: 3, Y: 4}},
			}},
			"MULTIPOINT((1 2), (3 4))",
		},
		{
			"MultiLineString",
			&cql2.MultiLineString{Lines: []cql2.LineString{
				{Coords: []cql2.Coord{{X: 1, Y: 2}, {X: 3, Y: 4}}},
			}},
			"MULTILINESTRING((1 2, 3 4))",
		},
		{
			"MultiPolygon",
			&cql2.MultiPolygon{Polys: []cql2.Polygon{
				{Rings: [][]cql2.Coord{{{X: 0, Y: 0}, {X: 1, Y: 0}, {X: 1, Y: 1}, {X: 0, Y: 0}}}},
			}},
			"MULTIPOLYGON(((0 0, 1 0, 1 1, 0 0)))",
		},
		{
			"GeometryCollection",
			&cql2.GeometryCollection{Geoms: []cql2.Geometry{
				&cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}},
				&cql2.LineString{Coords: []cql2.Coord{{X: 3, Y: 4}, {X: 5, Y: 6}}},
			}},
			"GEOMETRYCOLLECTION(POINT(1 2), LINESTRING(3 4, 5 6))",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := wkt.Encode(tt.in)
			if err != nil {
				t.Fatalf("encode: %v", err)
			}
			if got != tt.want {
				t.Fatalf("got %q want %q", got, tt.want)
			}
		})
	}
}

func TestEncodeErrors(t *testing.T) {
	tests := []struct {
		name string
		in   cql2.Geometry
	}{
		{"nilGeometry", nil},
		{"polygonNoRings", &cql2.Polygon{}},
		{"polygonShortRing", &cql2.Polygon{Rings: [][]cql2.Coord{{{}, {}, {}}}}},
		{"lineStringTooShort", &cql2.LineString{Coords: []cql2.Coord{{X: 1, Y: 2}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := wkt.Encode(tt.in)
			if err == nil {
				t.Fatal("expected error")
			}
			var ge *cql2.GeometryError
			if !errors.As(err, &ge) {
				t.Fatalf("expected *cql2.GeometryError, got %T", err)
			}
			if ge.Encoding != cql2.EncodingText {
				t.Fatalf("expected EncodingText, got %v", ge.Encoding)
			}
		})
	}
}
