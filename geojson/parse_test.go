package geojson_test

import (
	"errors"
	"reflect"
	"strings"
	"testing"

	cql2 "github.com/example/go-cql2"
	"github.com/example/go-cql2/geojson"
)

func TestParsePoint2D(t *testing.T) {
	g, err := geojson.Parse([]byte(`{"type":"Point","coordinates":[1,2]}`))
	if err != nil {
		t.Fatal(err)
	}
	want := &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}}
	if !reflect.DeepEqual(g, want) {
		t.Fatalf("got %#v want %#v", g, want)
	}
}

func TestParsePoint3D(t *testing.T) {
	g, err := geojson.Parse([]byte(`{"type":"Point","coordinates":[1,2,3]}`))
	if err != nil {
		t.Fatal(err)
	}
	want := &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2, Z: 3, HasZ: true}}
	if !reflect.DeepEqual(g, want) {
		t.Fatalf("got %#v want %#v", g, want)
	}
}

func TestParseLineString(t *testing.T) {
	g, err := geojson.Parse([]byte(`{"type":"LineString","coordinates":[[1,2],[3,4],[5,6]]}`))
	if err != nil {
		t.Fatal(err)
	}
	want := &cql2.LineString{Coords: []cql2.Coord{{X: 1, Y: 2}, {X: 3, Y: 4}, {X: 5, Y: 6}}}
	if !reflect.DeepEqual(g, want) {
		t.Fatalf("got %#v want %#v", g, want)
	}
}

func TestParseLineString3D(t *testing.T) {
	g, err := geojson.Parse([]byte(`{"type":"LineString","coordinates":[[1,2,3],[4,5,6]]}`))
	if err != nil {
		t.Fatal(err)
	}
	want := &cql2.LineString{Coords: []cql2.Coord{
		{X: 1, Y: 2, Z: 3, HasZ: true},
		{X: 4, Y: 5, Z: 6, HasZ: true},
	}}
	if !reflect.DeepEqual(g, want) {
		t.Fatalf("got %#v want %#v", g, want)
	}
}

func TestParsePolygon(t *testing.T) {
	g, err := geojson.Parse([]byte(`{"type":"Polygon","coordinates":[[[0,0],[1,0],[1,1],[0,1],[0,0]]]}`))
	if err != nil {
		t.Fatal(err)
	}
	want := &cql2.Polygon{Rings: [][]cql2.Coord{{
		{X: 0, Y: 0}, {X: 1, Y: 0}, {X: 1, Y: 1}, {X: 0, Y: 1}, {X: 0, Y: 0},
	}}}
	if !reflect.DeepEqual(g, want) {
		t.Fatalf("got %#v want %#v", g, want)
	}
}

func TestParsePolygon3D(t *testing.T) {
	g, err := geojson.Parse([]byte(`{"type":"Polygon","coordinates":[[[0,0,0],[1,0,0],[1,1,0],[0,1,0],[0,0,0]]]}`))
	if err != nil {
		t.Fatal(err)
	}
	poly, ok := g.(*cql2.Polygon)
	if !ok {
		t.Fatalf("want *Polygon, got %T", g)
	}
	if !poly.Rings[0][0].HasZ {
		t.Fatal("expected HasZ on 3D polygon coords")
	}
}

func TestParseMultiPoint(t *testing.T) {
	g, err := geojson.Parse([]byte(`{"type":"MultiPoint","coordinates":[[1,2],[3,4]]}`))
	if err != nil {
		t.Fatal(err)
	}
	want := &cql2.MultiPoint{Points: []cql2.Point{
		{Coord: cql2.Coord{X: 1, Y: 2}},
		{Coord: cql2.Coord{X: 3, Y: 4}},
	}}
	if !reflect.DeepEqual(g, want) {
		t.Fatalf("got %#v want %#v", g, want)
	}
}

func TestParseMultiLineString(t *testing.T) {
	g, err := geojson.Parse([]byte(`{"type":"MultiLineString","coordinates":[[[1,2],[3,4]],[[5,6],[7,8]]]}`))
	if err != nil {
		t.Fatal(err)
	}
	want := &cql2.MultiLineStr{Lines: []cql2.LineString{
		{Coords: []cql2.Coord{{X: 1, Y: 2}, {X: 3, Y: 4}}},
		{Coords: []cql2.Coord{{X: 5, Y: 6}, {X: 7, Y: 8}}},
	}}
	if !reflect.DeepEqual(g, want) {
		t.Fatalf("got %#v want %#v", g, want)
	}
}

func TestParseMultiPolygon(t *testing.T) {
	g, err := geojson.Parse([]byte(`{"type":"MultiPolygon","coordinates":[[[[0,0],[1,0],[1,1],[0,0]]]]}`))
	if err != nil {
		t.Fatal(err)
	}
	want := &cql2.MultiPolygon{Polys: []cql2.Polygon{{
		Rings: [][]cql2.Coord{{{X: 0, Y: 0}, {X: 1, Y: 0}, {X: 1, Y: 1}, {X: 0, Y: 0}}},
	}}}
	if !reflect.DeepEqual(g, want) {
		t.Fatalf("got %#v want %#v", g, want)
	}
}

func TestParseGeometryCollectionMixed(t *testing.T) {
	in := `{"type":"GeometryCollection","geometries":[
		{"type":"Point","coordinates":[1,2]},
		{"type":"LineString","coordinates":[[3,4],[5,6]]},
		{"type":"Polygon","coordinates":[[[0,0],[1,0],[1,1],[0,1],[0,0]]]}
	]}`
	g, err := geojson.Parse([]byte(in))
	if err != nil {
		t.Fatal(err)
	}
	gc, ok := g.(*cql2.GeometryColl)
	if !ok {
		t.Fatalf("want *GeometryColl, got %T", g)
	}
	if len(gc.Geoms) != 3 {
		t.Fatalf("want 3 geometries, got %d", len(gc.Geoms))
	}
	if _, ok := gc.Geoms[0].(*cql2.Point); !ok {
		t.Errorf("geom 0: want Point, got %T", gc.Geoms[0])
	}
	if _, ok := gc.Geoms[1].(*cql2.LineString); !ok {
		t.Errorf("geom 1: want LineString, got %T", gc.Geoms[1])
	}
	if _, ok := gc.Geoms[2].(*cql2.Polygon); !ok {
		t.Errorf("geom 2: want Polygon, got %T", gc.Geoms[2])
	}
}

func TestParseTopLevelBboxIgnored(t *testing.T) {
	in := `{"type":"Point","coordinates":[1,2],"bbox":[1,2,1,2]}`
	g, err := geojson.Parse([]byte(in))
	if err != nil {
		t.Fatal(err)
	}
	want := &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}}
	if !reflect.DeepEqual(g, want) {
		t.Fatalf("got %#v want %#v", g, want)
	}
}

func TestParseForeignMembersIgnored(t *testing.T) {
	in := `{"type":"Point","coordinates":[1,2],"name":"foo","extra":{"k":1}}`
	if _, err := geojson.Parse([]byte(in)); err != nil {
		t.Fatal(err)
	}
}

func assertGeomError(t *testing.T, err error, wantPath, wantMsgSubstr string) {
	t.Helper()
	if err == nil {
		t.Fatal("expected error, got nil")
	}
	var ge *cql2.GeometryError
	if !errors.As(err, &ge) {
		t.Fatalf("want *cql2.GeometryError, got %T: %v", err, err)
	}
	if ge.Encoding != cql2.EncodingJSON {
		t.Errorf("want EncodingJSON, got %v", ge.Encoding)
	}
	if ge.At.JSONPath != wantPath {
		t.Errorf("path: want %q, got %q", wantPath, ge.At.JSONPath)
	}
	if wantMsgSubstr != "" && !strings.Contains(ge.Msg, wantMsgSubstr) {
		t.Errorf("msg %q does not contain %q", ge.Msg, wantMsgSubstr)
	}
}

func TestParseErrorMissingType(t *testing.T) {
	_, err := geojson.Parse([]byte(`{"coordinates":[1,2]}`))
	assertGeomError(t, err, "", "missing")
}

func TestParseErrorUnknownType(t *testing.T) {
	_, err := geojson.Parse([]byte(`{"type":"Hexagon","coordinates":[1,2]}`))
	assertGeomError(t, err, "/type", "unknown")
}

func TestParseErrorMalformedCoords(t *testing.T) {
	_, err := geojson.Parse([]byte(`{"type":"Point","coordinates":["a","b"]}`))
	assertGeomError(t, err, "/coordinates/0", "")
}

func TestParseErrorPointTooFewComponents(t *testing.T) {
	_, err := geojson.Parse([]byte(`{"type":"Point","coordinates":[1]}`))
	assertGeomError(t, err, "/coordinates", "at least 2")
}

func TestParseErrorPointNullCoords(t *testing.T) {
	_, err := geojson.Parse([]byte(`{"type":"Point","coordinates":null}`))
	assertGeomError(t, err, "/coordinates", "null")
}

func TestParseErrorRingNotClosed(t *testing.T) {
	_, err := geojson.Parse([]byte(`{"type":"Polygon","coordinates":[[[0,0],[1,0],[1,1],[0,1]]]}`))
	assertGeomError(t, err, "/coordinates/0", "not closed")
}

func TestParseErrorRingTooFewPoints(t *testing.T) {
	_, err := geojson.Parse([]byte(`{"type":"Polygon","coordinates":[[[0,0],[1,0],[0,0]]]}`))
	assertGeomError(t, err, "/coordinates/0", "at least 4")
}

func TestParseErrorNestedPath(t *testing.T) {
	in := `{"type":"GeometryCollection","geometries":[
		{"type":"Point","coordinates":[1,2]},
		{"type":"Hexagon"}
	]}`
	_, err := geojson.Parse([]byte(in))
	assertGeomError(t, err, "/geometries/1/type", "unknown")
}
