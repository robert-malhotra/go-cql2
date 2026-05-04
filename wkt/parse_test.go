package wkt_test

import (
	"errors"
	"reflect"
	"strings"
	"testing"

	cql2 "github.com/exergy-dev/go-cql2"
	"github.com/exergy-dev/go-cql2/wkt"
)

func TestParseRoundTrip(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want cql2.Geometry
	}{
		{
			name: "Point2D",
			in:   "POINT(1 2)",
			want: &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}},
		},
		{
			name: "Point3D",
			in:   "POINT(1 2 3)",
			want: &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2, Z: 3, HasZ: true}},
		},
		{
			name: "PointEmpty",
			in:   "POINT EMPTY",
			want: &cql2.Point{Empty: true},
		},
		{
			name: "PointNegativeAndDecimal",
			in:   "POINT(-1.5 2.25)",
			want: &cql2.Point{Coord: cql2.Coord{X: -1.5, Y: 2.25}},
		},
		{
			name: "LineString2D",
			in:   "LINESTRING(1 2, 3 4, 5 6)",
			want: &cql2.LineString{Coords: []cql2.Coord{
				{X: 1, Y: 2}, {X: 3, Y: 4}, {X: 5, Y: 6},
			}},
		},
		{
			name: "LineString3D",
			in:   "LINESTRING(1 2 3, 4 5 6)",
			want: &cql2.LineString{Coords: []cql2.Coord{
				{X: 1, Y: 2, Z: 3, HasZ: true},
				{X: 4, Y: 5, Z: 6, HasZ: true},
			}},
		},
		{
			name: "Polygon2D",
			in:   "POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))",
			want: &cql2.Polygon{Rings: [][]cql2.Coord{{
				{X: 0, Y: 0}, {X: 1, Y: 0}, {X: 1, Y: 1}, {X: 0, Y: 1}, {X: 0, Y: 0},
			}}},
		},
		{
			name: "Polygon3D",
			in:   "POLYGON((0 0 0, 1 0 0, 1 1 0, 0 0 0))",
			want: &cql2.Polygon{Rings: [][]cql2.Coord{{
				{X: 0, Y: 0, Z: 0, HasZ: true},
				{X: 1, Y: 0, Z: 0, HasZ: true},
				{X: 1, Y: 1, Z: 0, HasZ: true},
				{X: 0, Y: 0, Z: 0, HasZ: true},
			}}},
		},
		{
			name: "PolygonMultiRing",
			in:   "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 2 1, 2 2, 1 2, 1 1))",
			want: &cql2.Polygon{Rings: [][]cql2.Coord{
				{{X: 0, Y: 0}, {X: 10, Y: 0}, {X: 10, Y: 10}, {X: 0, Y: 10}, {X: 0, Y: 0}},
				{{X: 1, Y: 1}, {X: 2, Y: 1}, {X: 2, Y: 2}, {X: 1, Y: 2}, {X: 1, Y: 1}},
			}},
		},
		{
			name: "MultiPointCanonical",
			in:   "MULTIPOINT((1 2), (3 4))",
			want: &cql2.MultiPoint{Points: []cql2.Point{
				{Coord: cql2.Coord{X: 1, Y: 2}},
				{Coord: cql2.Coord{X: 3, Y: 4}},
			}},
		},
		{
			name: "MultiLineString",
			in:   "MULTILINESTRING((1 2, 3 4), (5 6, 7 8))",
			want: &cql2.MultiLineString{Lines: []cql2.LineString{
				{Coords: []cql2.Coord{{X: 1, Y: 2}, {X: 3, Y: 4}}},
				{Coords: []cql2.Coord{{X: 5, Y: 6}, {X: 7, Y: 8}}},
			}},
		},
		{
			name: "MultiPolygon",
			in:   "MULTIPOLYGON(((0 0, 1 0, 1 1, 0 0)), ((10 10, 11 10, 11 11, 10 10)))",
			want: &cql2.MultiPolygon{Polys: []cql2.Polygon{
				{Rings: [][]cql2.Coord{{{X: 0, Y: 0}, {X: 1, Y: 0}, {X: 1, Y: 1}, {X: 0, Y: 0}}}},
				{Rings: [][]cql2.Coord{{{X: 10, Y: 10}, {X: 11, Y: 10}, {X: 11, Y: 11}, {X: 10, Y: 10}}}},
			}},
		},
		{
			name: "GeometryCollection",
			in:   "GEOMETRYCOLLECTION(POINT(1 2), LINESTRING(3 4, 5 6))",
			want: &cql2.GeometryCollection{Geoms: []cql2.Geometry{
				&cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}},
				&cql2.LineString{Coords: []cql2.Coord{{X: 3, Y: 4}, {X: 5, Y: 6}}},
			}},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := wkt.Parse(tt.in)
			if err != nil {
				t.Fatalf("parse %q: %v", tt.in, err)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("parse mismatch\n got: %#v\nwant: %#v", got, tt.want)
			}

			out, err := wkt.Encode(got)
			if err != nil {
				t.Fatalf("encode: %v", err)
			}

			got2, err := wkt.Parse(out)
			if err != nil {
				t.Fatalf("re-parse %q: %v", out, err)
			}
			if !reflect.DeepEqual(got2, tt.want) {
				t.Fatalf("round-trip mismatch\n got: %#v\nwant: %#v\nencoded: %s", got2, tt.want, out)
			}
		})
	}
}

func TestParseCaseInsensitive(t *testing.T) {
	cases := []string{
		"point(1 2)",
		"Point(1 2)",
		"POINT(1 2)",
		"PoInT(1 2)",
	}
	want := &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}}
	for _, in := range cases {
		got, err := wkt.Parse(in)
		if err != nil {
			t.Fatalf("parse %q: %v", in, err)
		}
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("case %q: got %#v", in, got)
		}
	}
}

func TestParsePointEmptyCases(t *testing.T) {
	for _, in := range []string{"POINT EMPTY", "point empty", "POINT  EMPTY", "POINT\tEMPTY"} {
		got, err := wkt.Parse(in)
		if err != nil {
			t.Fatalf("parse %q: %v", in, err)
		}
		want := &cql2.Point{Empty: true}
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%q: got %#v", in, got)
		}
	}
}

func TestMultiPointBothForms(t *testing.T) {
	canonical := "MULTIPOINT((1 2), (3 4))"
	pragmatic := "MULTIPOINT(1 2, 3 4)"
	want := &cql2.MultiPoint{Points: []cql2.Point{
		{Coord: cql2.Coord{X: 1, Y: 2}},
		{Coord: cql2.Coord{X: 3, Y: 4}},
	}}
	for _, in := range []string{canonical, pragmatic} {
		got, err := wkt.Parse(in)
		if err != nil {
			t.Fatalf("parse %q: %v", in, err)
		}
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%q: got %#v want %#v", in, got, want)
		}
	}
	// Pragmatic form should encode to canonical form.
	got, _ := wkt.Parse(pragmatic)
	out, err := wkt.Encode(got)
	if err != nil {
		t.Fatalf("encode: %v", err)
	}
	const wantOut = "MULTIPOINT((1 2), (3 4))"
	if out != wantOut {
		t.Fatalf("canonical encode: got %q want %q", out, wantOut)
	}
}

func TestParseWhitespaceTolerance(t *testing.T) {
	in := "  LINESTRING (\n  1 2 ,\n  3 4 ,\n  5 6\n) \t"
	got, err := wkt.Parse(in)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	want := &cql2.LineString{Coords: []cql2.Coord{
		{X: 1, Y: 2}, {X: 3, Y: 4}, {X: 5, Y: 6},
	}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("got %#v want %#v", got, want)
	}
}

func TestParseErrors(t *testing.T) {
	tests := []struct {
		name string
		in   string
	}{
		{"unclosedParen", "POINT(1 2"},
		{"missingComma", "LINESTRING(1 2 3 4)"}, // becomes 3D point + extra "4"
		{"unknownType", "FOOBAR(1 2)"},
		{"polygonRingNotClosed", "POLYGON((0 0, 1 0, 1 1, 0 1))"},
		{"emptyInput", ""},
		{"trailingJunk", "POINT(1 2) extra"},
		{"lineStringTooFewPoints", "LINESTRING(1 2)"},
		{"polygonTooFewPoints", "POLYGON((0 0, 1 0, 0 0))"},
		{"badNumber", "POINT(a b)"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := wkt.Parse(tt.in)
			if err == nil {
				t.Fatalf("expected error for %q", tt.in)
			}
			var ge *cql2.GeometryError
			if !errors.As(err, &ge) {
				t.Fatalf("expected *cql2.GeometryError, got %T (%v)", err, err)
			}
			if ge.Encoding != cql2.EncodingText {
				t.Fatalf("expected EncodingText, got %v", ge.Encoding)
			}
			// Pos should be populated for non-empty input.
			if tt.in != "" {
				if ge.At == (cql2.Pos{}) {
					t.Fatalf("expected populated Pos, got zero value (msg=%q)", ge.Msg)
				}
				if ge.At.Line == 0 || ge.At.Column == 0 {
					t.Fatalf("expected Line/Column populated, got %+v", ge.At)
				}
			}
		})
	}
}

func TestParseErrorPositionMidLine(t *testing.T) {
	in := "LINESTRING(1 2,\n  bogus)"
	_, err := wkt.Parse(in)
	var ge *cql2.GeometryError
	if !errors.As(err, &ge) {
		t.Fatalf("expected GeometryError, got %v", err)
	}
	if ge.At.Line != 2 {
		t.Fatalf("expected line 2, got %d (col %d)", ge.At.Line, ge.At.Column)
	}
}

func TestParseZTag(t *testing.T) {
	tests := []struct {
		name    string
		in      string
		want    cql2.Geometry
		wantEnc string
	}{
		{
			name:    "PointZ",
			in:      "POINT Z (1 2 3)",
			want:    &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2, Z: 3, HasZ: true}},
			wantEnc: "POINT(1 2 3)",
		},
		{
			name: "LineStringZ",
			in:   "LINESTRING Z (1 2 3, 4 5 6)",
			want: &cql2.LineString{Coords: []cql2.Coord{
				{X: 1, Y: 2, Z: 3, HasZ: true},
				{X: 4, Y: 5, Z: 6, HasZ: true},
			}},
			wantEnc: "LINESTRING(1 2 3, 4 5 6)",
		},
		{
			name: "PolygonZ",
			in:   "POLYGON Z ((0 0 0, 1 0 0, 1 1 0, 0 0 0))",
			want: &cql2.Polygon{Rings: [][]cql2.Coord{{
				{X: 0, Y: 0, Z: 0, HasZ: true},
				{X: 1, Y: 0, Z: 0, HasZ: true},
				{X: 1, Y: 1, Z: 0, HasZ: true},
				{X: 0, Y: 0, Z: 0, HasZ: true},
			}}},
			wantEnc: "POLYGON((0 0 0, 1 0 0, 1 1 0, 0 0 0))",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := wkt.Parse(tt.in)
			if err != nil {
				t.Fatalf("parse %q: %v", tt.in, err)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("parse mismatch\n got: %#v\nwant: %#v", got, tt.want)
			}
			out, err := wkt.Encode(got)
			if err != nil {
				t.Fatalf("encode: %v", err)
			}
			if out != tt.wantEnc {
				t.Fatalf("encode = %q, want %q", out, tt.wantEnc)
			}
			// Re-parse the canonical (no-Z) encoding.
			got2, err := wkt.Parse(out)
			if err != nil {
				t.Fatalf("re-parse %q: %v", out, err)
			}
			if !reflect.DeepEqual(got2, tt.want) {
				t.Fatalf("round-trip mismatch\n got: %#v\nwant: %#v", got2, tt.want)
			}
		})
	}
}

func TestParseMTagRejected(t *testing.T) {
	for _, in := range []string{"POINT M (1 2 3)", "POINT ZM (1 2 3 4)"} {
		_, err := wkt.Parse(in)
		if err == nil {
			t.Fatalf("expected error for %q", in)
		}
		var ge *cql2.GeometryError
		if !errors.As(err, &ge) {
			t.Fatalf("expected *cql2.GeometryError for %q, got %T (%v)", in, err, err)
		}
		if !strings.Contains(ge.Msg, "measure") {
			t.Errorf("expected message about measure dimension, got %q", ge.Msg)
		}
	}
}

func TestParseZTagRequires3D(t *testing.T) {
	_, err := wkt.Parse("POINT Z (1 2)")
	if err == nil {
		t.Fatal("expected error for POINT Z (1 2)")
	}
	var ge *cql2.GeometryError
	if !errors.As(err, &ge) {
		t.Fatalf("expected *cql2.GeometryError, got %T (%v)", err, err)
	}
	if !strings.Contains(ge.Msg, "3D") && !strings.Contains(ge.Msg, "Z") {
		t.Errorf("expected message about Z/3D requirement, got %q", ge.Msg)
	}
}

func TestParseBytes(t *testing.T) {
	got, err := wkt.ParseBytes([]byte("POINT(1 2)"))
	if err != nil {
		t.Fatalf("ParseBytes: %v", err)
	}
	want := &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("got %#v want %#v", got, want)
	}
}

// TestParseEmptyMultiTypes covers the four EMPTY multi-geometry forms
// that the encoder emits for zero-length collections. Without parser
// support, encode → parse round-trip on an empty multi geometry fails.
func TestParseEmptyMultiTypes(t *testing.T) {
	tests := []struct {
		in   string
		want cql2.Geometry
	}{
		{"MULTIPOINT EMPTY", &cql2.MultiPoint{}},
		{"multipoint empty", &cql2.MultiPoint{}},
		{"MULTILINESTRING EMPTY", &cql2.MultiLineString{}},
		{"MULTIPOLYGON EMPTY", &cql2.MultiPolygon{}},
		{"GEOMETRYCOLLECTION EMPTY", &cql2.GeometryCollection{}},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			got, err := wkt.Parse(tc.in)
			if err != nil {
				t.Fatalf("Parse(%q): %v", tc.in, err)
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("Parse(%q): got %#v want %#v", tc.in, got, tc.want)
			}
			// Round-trip: Encode(parsed) re-parses to an equal value.
			out, err := wkt.Encode(got)
			if err != nil {
				t.Fatalf("Encode: %v", err)
			}
			again, err := wkt.Parse(out)
			if err != nil {
				t.Fatalf("re-Parse(%q): %v", out, err)
			}
			if !reflect.DeepEqual(again, tc.want) {
				t.Fatalf("round-trip mismatch: %q -> %q -> %#v", tc.in, out, again)
			}
		})
	}
}
