package text

import (
	"encoding/json"
	"strings"
	"testing"
	"time"

	cql2 "github.com/example/go-cql2"
)

func TestEncode_Booleans(t *testing.T) {
	s, err := Encode(&cql2.BoolLit{Value: true})
	if err != nil {
		t.Fatal(err)
	}
	if s != "TRUE" {
		t.Errorf("got %q", s)
	}
	s, _ = Encode(&cql2.BoolLit{Value: false})
	if s != "FALSE" {
		t.Errorf("got %q", s)
	}
}

func TestEncode_Null(t *testing.T) {
	s, err := Encode(&cql2.NullLit{})
	if err != nil {
		t.Fatal(err)
	}
	if s != "NULL" {
		t.Errorf("got %q", s)
	}
}

func TestEncode_Number(t *testing.T) {
	s, err := Encode(&cql2.NumLit{Value: json.Number("3.14")})
	if err != nil {
		t.Fatal(err)
	}
	if s != "3.14" {
		t.Errorf("got %q", s)
	}
}

func TestEncode_String(t *testing.T) {
	s, err := Encode(&cql2.StringLit{Value: "it's"})
	if err != nil {
		t.Fatal(err)
	}
	if s != `'it''s'` {
		t.Errorf("got %q", s)
	}
}

func TestEncode_PropertyForms(t *testing.T) {
	s, _ := Encode(&cql2.PropertyRef{Name: "foo"})
	if s != "foo" {
		t.Errorf("got %q", s)
	}
	s, _ = Encode(&cql2.PropertyRef{Name: "eo:cloud_cover"})
	if s != "eo:cloud_cover" {
		t.Errorf("got %q", s)
	}
	s, _ = Encode(&cql2.PropertyRef{Name: "with space"})
	if s != `"with space"` {
		t.Errorf("got %q", s)
	}
	s, _ = Encode(&cql2.PropertyRef{Name: `quote"in`})
	if s != `"quote""in"` {
		t.Errorf("got %q", s)
	}
	// Reserved word must be quoted.
	s, _ = Encode(&cql2.PropertyRef{Name: "AND"})
	if s != `"AND"` {
		t.Errorf("got %q", s)
	}
}

func TestEncode_Timestamp(t *testing.T) {
	tm := time.Date(2020, 5, 1, 10, 0, 0, 0, time.UTC)
	s, err := Encode(&cql2.TimestampLit{Value: tm})
	if err != nil {
		t.Fatal(err)
	}
	if s != "TIMESTAMP('2020-05-01T10:00:00Z')" {
		t.Errorf("got %q", s)
	}
}

func TestEncode_Date(t *testing.T) {
	tm := time.Date(2020, 5, 1, 0, 0, 0, 0, time.UTC)
	s, err := Encode(&cql2.DateLit{Value: tm})
	if err != nil {
		t.Fatal(err)
	}
	if s != "DATE('2020-05-01')" {
		t.Errorf("got %q", s)
	}
}

func TestEncode_Interval(t *testing.T) {
	il := &cql2.IntervalLit{
		Start: &cql2.DateLit{Value: time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)},
		End:   &cql2.Unbounded{},
	}
	s, err := Encode(il)
	if err != nil {
		t.Fatal(err)
	}
	if s != "INTERVAL('2020-01-01','..')" {
		t.Errorf("got %q", s)
	}
}

func TestEncode_IntervalWithProperty(t *testing.T) {
	il := &cql2.IntervalLit{
		Start: &cql2.PropertyRef{Name: "starts_at"},
		End:   &cql2.PropertyRef{Name: "ends_at"},
	}
	s, err := Encode(il)
	if err != nil {
		t.Fatal(err)
	}
	if s != "INTERVAL(starts_at,ends_at)" {
		t.Errorf("got %q, want %q", s, "INTERVAL(starts_at,ends_at)")
	}

	il = &cql2.IntervalLit{
		Start: &cql2.DateLit{Value: time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)},
		End:   &cql2.PropertyRef{Name: "ends_at"},
	}
	s, err = Encode(il)
	if err != nil {
		t.Fatal(err)
	}
	if s != "INTERVAL('2020-01-01',ends_at)" {
		t.Errorf("got %q, want %q", s, "INTERVAL('2020-01-01',ends_at)")
	}
}

func TestEncode_BBox(t *testing.T) {
	s, err := Encode(&cql2.BBoxLit{Coords: []float64{-180, -90, 180, 90}})
	if err != nil {
		t.Fatal(err)
	}
	if s != "BBOX(-180,-90,180,90)" {
		t.Errorf("got %q", s)
	}
}

func TestEncode_NestedNotIsNull(t *testing.T) {
	n := &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{
		&cql2.Op{Op: cql2.OpIsNull, Args: []cql2.Node{&cql2.PropertyRef{Name: "x"}}},
	}}
	s, err := Encode(n)
	if err != nil {
		t.Fatal(err)
	}
	if s != "x IS NOT NULL" {
		t.Errorf("got %q", s)
	}
}

func TestEncode_NestedNotBetween(t *testing.T) {
	n := &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{
		&cql2.Op{Op: cql2.OpBetween, Args: []cql2.Node{
			&cql2.PropertyRef{Name: "x"},
			&cql2.NumLit{Value: json.Number("0")},
			&cql2.NumLit{Value: json.Number("10")},
		}},
	}}
	s, err := Encode(n)
	if err != nil {
		t.Fatal(err)
	}
	if s != "x NOT BETWEEN 0 AND 10" {
		t.Errorf("got %q", s)
	}
}

func TestEncode_SpatialOpFunctionForm(t *testing.T) {
	n := &cql2.Op{Op: cql2.OpSIntersects, Args: []cql2.Node{
		&cql2.PropertyRef{Name: "g"},
		&cql2.GeomLit{Geom: &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}}},
	}}
	s, err := Encode(n)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.HasPrefix(s, "S_INTERSECTS(") {
		t.Errorf("got %q", s)
	}
	if !strings.Contains(s, "POINT(1 2)") {
		t.Errorf("got %q", s)
	}
}

func TestEncode_PrecedenceWrapping(t *testing.T) {
	// (a OR b) AND c — OR has lower precedence than AND, so the OR must be wrapped.
	n := &cql2.Op{Op: cql2.OpAnd, Args: []cql2.Node{
		&cql2.Op{Op: cql2.OpOr, Args: []cql2.Node{
			&cql2.PropertyRef{Name: "a"}, &cql2.PropertyRef{Name: "b"},
		}},
		&cql2.PropertyRef{Name: "c"},
	}}
	s, err := Encode(n)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(s, "(a OR b)") {
		t.Errorf("expected wrapped OR, got %q", s)
	}
}
