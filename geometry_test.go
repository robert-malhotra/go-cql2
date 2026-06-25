package cql2_test

import (
	"encoding/json"
	"testing"

	cql2 "github.com/exergy-dev/go-cql2"
	"github.com/exergy-dev/go-topology-suite/geom"
)

func TestGeometryPolymorphicUnmarshal(t *testing.T) {
	cases := []struct {
		name string
		data string
		want geom.Type
	}{
		{"point", `{"type":"Point","coordinates":[1,2]}`, geom.PointType},
		{"linestring", `{"type":"LineString","coordinates":[[0,0],[1,1]]}`, geom.LineStringType},
		{"polygon", `{"type":"Polygon","coordinates":[[[0,0],[1,0],[1,1],[0,0]]]}`, geom.PolygonType},
		{"multipoint", `{"type":"MultiPoint","coordinates":[[0,0],[1,1]]}`, geom.MultiPointType},
		{"multilinestring", `{"type":"MultiLineString","coordinates":[[[0,0],[1,1]]]}`, geom.MultiLineStringType},
		{"multipolygon", `{"type":"MultiPolygon","coordinates":[[[[0,0],[1,0],[1,1],[0,0]]]]}`, geom.MultiPolygonType},
		{"collection", `{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[1,2]}]}`, geom.GeometryCollectionType},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			var w cql2.Geometry
			if err := json.Unmarshal([]byte(tc.data), &w); err != nil {
				t.Fatalf("Unmarshal: %v", err)
			}
			if w.G == nil {
				t.Fatal("G is nil")
			}
			if w.G.Type() != tc.want {
				t.Fatalf("type: got %v want %v", w.G.Type(), tc.want)
			}
		})
	}
}

func TestGeometryRoundTrip(t *testing.T) {
	in := cql2.Geometry{G: geom.NewPoint(nil, geom.XY{X: 1.5, Y: 2.5})}
	data, err := json.Marshal(in)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	var out cql2.Geometry
	if err := json.Unmarshal(data, &out); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	p, ok := out.G.(*geom.Point)
	if !ok {
		t.Fatalf("wrong type: %T", out.G)
	}
	xy := p.XY()
	if xy.X != 1.5 || xy.Y != 2.5 {
		t.Fatalf("coords: got %v", xy)
	}
}

func TestGeometryNullMarshal(t *testing.T) {
	var w cql2.Geometry
	data, err := json.Marshal(w)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	if string(data) != "null" {
		t.Fatalf("got %q want null", data)
	}
}

func TestGeometryNullUnmarshal(t *testing.T) {
	w := cql2.Geometry{G: geom.NewPoint(nil, geom.XY{X: 1, Y: 2})}
	if err := json.Unmarshal([]byte("null"), &w); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if w.G != nil {
		t.Fatalf("G should be nil after null unmarshal, got %v", w.G)
	}
}

func TestGeometryEmbeddedInStruct(t *testing.T) {
	type record struct {
		ID   int           `json:"id"`
		Geom cql2.Geometry `json:"geom"`
	}
	data := []byte(`{"id":7,"geom":{"type":"LineString","coordinates":[[0,0],[1,1]]}}`)
	var r record
	if err := json.Unmarshal(data, &r); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if r.ID != 7 {
		t.Fatalf("id: got %d", r.ID)
	}
	if _, ok := r.Geom.G.(*geom.LineString); !ok {
		t.Fatalf("geom: %T", r.Geom.G)
	}
	out, err := json.Marshal(r)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	var r2 record
	if err := json.Unmarshal(out, &r2); err != nil {
		t.Fatalf("re-Unmarshal: %v", err)
	}
	if r2.ID != 7 {
		t.Fatalf("id round-trip: got %d", r2.ID)
	}
}
