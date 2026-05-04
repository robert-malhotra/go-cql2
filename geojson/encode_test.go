package geojson_test

import (
	"errors"
	"reflect"
	"testing"

	cql2 "github.com/exergy-dev/go-cql2"
	"github.com/exergy-dev/go-cql2/geojson"
)

func TestEncodePoint2D(t *testing.T) {
	g := &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}}
	b, err := geojson.Encode(g)
	if err != nil {
		t.Fatal(err)
	}
	want := `{"type":"Point","coordinates":[1,2]}`
	if string(b) != want {
		t.Fatalf("got %q want %q", b, want)
	}
}

func TestEncodePoint3D(t *testing.T) {
	g := &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2, Z: 3, HasZ: true}}
	b, err := geojson.Encode(g)
	if err != nil {
		t.Fatal(err)
	}
	want := `{"type":"Point","coordinates":[1,2,3]}`
	if string(b) != want {
		t.Fatalf("got %q want %q", b, want)
	}
}

func TestEncodePointEmptyError(t *testing.T) {
	g := &cql2.Point{Empty: true}
	_, err := geojson.Encode(g)
	if err == nil {
		t.Fatal("expected error")
	}
	var ge *cql2.GeometryError
	if !errors.As(err, &ge) {
		t.Fatalf("want *GeometryError, got %T", err)
	}
}

func TestEncodeMultiPointEmptyCollectionOK(t *testing.T) {
	g := &cql2.MultiPoint{Points: nil}
	b, err := geojson.Encode(g)
	if err != nil {
		t.Fatal(err)
	}
	want := `{"type":"MultiPoint","coordinates":[]}`
	if string(b) != want {
		t.Fatalf("got %q want %q", b, want)
	}
}

func TestEncodeFloatFormatting(t *testing.T) {
	// 'g' format with -1 prec strips trailing zeros and avoids exponent for normal magnitudes.
	g := &cql2.Point{Coord: cql2.Coord{X: 1.5, Y: -2.25}}
	b, err := geojson.Encode(g)
	if err != nil {
		t.Fatal(err)
	}
	want := `{"type":"Point","coordinates":[1.5,-2.25]}`
	if string(b) != want {
		t.Fatalf("got %q want %q", b, want)
	}
}

// roundTrip parses bytes, re-encodes them, and re-parses, asserting AST equality.
func roundTrip(t *testing.T, in string) {
	t.Helper()
	g1, err := geojson.Parse([]byte(in))
	if err != nil {
		t.Fatalf("first parse: %v", err)
	}
	enc, err := geojson.Encode(g1)
	if err != nil {
		t.Fatalf("encode: %v", err)
	}
	g2, err := geojson.Parse(enc)
	if err != nil {
		t.Fatalf("second parse of %q: %v", enc, err)
	}
	if !reflect.DeepEqual(g1, g2) {
		t.Fatalf("AST mismatch:\n  in:  %#v\n  out: %#v", g1, g2)
	}
}

func TestRoundTripAll(t *testing.T) {
	cases := []string{
		`{"type":"Point","coordinates":[1,2]}`,
		`{"type":"Point","coordinates":[1,2,3]}`,
		`{"type":"LineString","coordinates":[[1,2],[3,4]]}`,
		`{"type":"LineString","coordinates":[[1,2,3],[4,5,6]]}`,
		`{"type":"Polygon","coordinates":[[[0,0],[1,0],[1,1],[0,1],[0,0]]]}`,
		`{"type":"Polygon","coordinates":[[[0,0,0],[1,0,0],[1,1,0],[0,1,0],[0,0,0]]]}`,
		`{"type":"MultiPoint","coordinates":[[1,2],[3,4]]}`,
		`{"type":"MultiLineString","coordinates":[[[1,2],[3,4]],[[5,6],[7,8]]]}`,
		`{"type":"MultiPolygon","coordinates":[[[[0,0],[1,0],[1,1],[0,0]]],[[[2,2],[3,2],[3,3],[2,2]]]]}`,
		`{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[1,2]},{"type":"LineString","coordinates":[[3,4],[5,6]]}]}`,
	}
	for _, c := range cases {
		c := c
		t.Run(c, func(t *testing.T) {
			roundTrip(t, c)
		})
	}
}

func TestEncodeGeometryCollectionMixed(t *testing.T) {
	g := &cql2.GeometryCollection{Geoms: []cql2.Geometry{
		&cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}},
		&cql2.LineString{Coords: []cql2.Coord{{X: 3, Y: 4}, {X: 5, Y: 6}}},
	}}
	b, err := geojson.Encode(g)
	if err != nil {
		t.Fatal(err)
	}
	want := `{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[1,2]},{"type":"LineString","coordinates":[[3,4],[5,6]]}]}`
	if string(b) != want {
		t.Fatalf("got %q want %q", b, want)
	}
}
