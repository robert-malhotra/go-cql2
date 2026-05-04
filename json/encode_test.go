package json_test

import (
	stdjson "encoding/json"
	"testing"
	"time"

	cql2 "github.com/exergy-dev/go-cql2"
	cqljson "github.com/exergy-dev/go-cql2/json"
)

func TestEncodeBool(t *testing.T) {
	b, err := cqljson.Encode(&cql2.BoolLit{Value: true})
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != "true" {
		t.Fatalf("got %q", b)
	}
}

func TestEncodeNull(t *testing.T) {
	b, err := cqljson.Encode(&cql2.NullLit{})
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != "null" {
		t.Fatalf("got %q", b)
	}
}

func TestEncodeStringEscape(t *testing.T) {
	b, err := cqljson.Encode(&cql2.StringLit{Value: "he\"llo\n"})
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != `"he\"llo\n"` {
		t.Fatalf("got %s", b)
	}
}

func TestEncodeNumberPreserved(t *testing.T) {
	for _, in := range []string{"5", "1.0", "1e10", "-3.14"} {
		b, err := cqljson.Encode(&cql2.NumLit{Value: stdjson.Number(in)})
		if err != nil {
			t.Fatal(err)
		}
		if string(b) != in {
			t.Fatalf("input %q got %q", in, b)
		}
	}
}

func TestEncodeOpKeyOrder(t *testing.T) {
	n := &cql2.Op{Op: cql2.OpEq, Args: []cql2.Node{
		&cql2.PropertyRef{Name: "x"},
		&cql2.NumLit{Value: stdjson.Number("5")},
	}}
	b, err := cqljson.Encode(n)
	if err != nil {
		t.Fatal(err)
	}
	got := string(b)
	want := `{"op":"=","args":[{"property":"x"},5]}`
	if got != want {
		t.Fatalf("got %q want %q", got, want)
	}
	// "op" must precede "args".
	opIdx := indexOf(got, `"op"`)
	argsIdx := indexOf(got, `"args"`)
	if !(opIdx >= 0 && argsIdx >= 0 && opIdx < argsIdx) {
		t.Fatalf("op-before-args violated: op=%d args=%d", opIdx, argsIdx)
	}
}

func TestEncodeFunctionKeyOrder(t *testing.T) {
	n := &cql2.FunctionCall{Name: "abs", Args: []cql2.Node{
		&cql2.NumLit{Value: stdjson.Number("5")},
	}}
	b, err := cqljson.Encode(n)
	if err != nil {
		t.Fatal(err)
	}
	want := `{"function":{"name":"abs","args":[5]}}`
	if string(b) != want {
		t.Fatalf("got %s", b)
	}
}

func TestEncodeIntervalUnbounded(t *testing.T) {
	n := &cql2.IntervalLit{
		Start: &cql2.Unbounded{},
		End:   &cql2.DateLit{Value: mustParseDate("2020-12-31")},
	}
	b, err := cqljson.Encode(n)
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != `{"interval":["..","2020-12-31"]}` {
		t.Fatalf("got %s", b)
	}
}

func TestEncodeBBox(t *testing.T) {
	b, err := cqljson.Encode(&cql2.BBoxLit{Coords: []float64{1, 2, 3, 4}})
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != `{"bbox":[1,2,3,4]}` {
		t.Fatalf("got %s", b)
	}
}

func TestEncodeArray(t *testing.T) {
	a := &cql2.ArrayLit{Elements: []cql2.Node{
		&cql2.NumLit{Value: stdjson.Number("1")},
		&cql2.StringLit{Value: "x"},
		&cql2.NullLit{},
	}}
	b, err := cqljson.Encode(a)
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != `[1,"x",null]` {
		t.Fatalf("got %s", b)
	}
}

func TestEncodeDeterminism(t *testing.T) {
	n := &cql2.Op{Op: cql2.OpAnd, Args: []cql2.Node{
		&cql2.BoolLit{Value: true},
		&cql2.Op{Op: cql2.OpEq, Args: []cql2.Node{
			&cql2.PropertyRef{Name: "x"},
			&cql2.NumLit{Value: stdjson.Number("1")},
		}},
	}}
	b1, _ := cqljson.Encode(n)
	b2, _ := cqljson.Encode(n)
	if string(b1) != string(b2) {
		t.Fatalf("nondeterministic:\n  %s\n  %s", b1, b2)
	}
}

func TestEncodeGeometryDelegation(t *testing.T) {
	gl := &cql2.GeomLit{Geom: &cql2.Point{Coord: cql2.Coord{X: 1, Y: 2}}}
	b, err := cqljson.Encode(gl)
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != `{"type":"Point","coordinates":[1,2]}` {
		t.Fatalf("got %s", b)
	}
}

// TestEncodeDateNormalizesToUTC pins that a DateLit carrying a non-UTC
// time.Time encodes to the UTC calendar-date string in JSON, matching the
// text encoder. Without UTC normalization the JSON and text encoders
// emitted different dates for the same DateLit.
func TestEncodeDateNormalizesToUTC(t *testing.T) {
	loc, err := time.LoadLocation("America/New_York")
	if err != nil {
		t.Skipf("tzdata unavailable: %v", err)
	}
	// 2024-01-01 22:00 New York is 2024-01-02 03:00 UTC.
	d := &cql2.DateLit{Value: time.Date(2024, 1, 1, 22, 0, 0, 0, loc)}

	got, err := cqljson.Encode(d)
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	want := `{"date":"2024-01-02"}`
	if string(got) != want {
		t.Fatalf("Encode: got %s want %s", got, want)
	}

	// Same DateLit inside an INTERVAL endpoint must also normalize.
	iv := &cql2.IntervalLit{Start: d, End: &cql2.Unbounded{}}
	gotIv, err := cqljson.Encode(iv)
	if err != nil {
		t.Fatalf("Encode interval: %v", err)
	}
	wantIv := `{"interval":["2024-01-02",".."]}`
	if string(gotIv) != wantIv {
		t.Fatalf("Encode interval: got %s want %s", gotIv, wantIv)
	}
}

// helpers

func indexOf(haystack, needle string) int {
	for i := 0; i+len(needle) <= len(haystack); i++ {
		if haystack[i:i+len(needle)] == needle {
			return i
		}
	}
	return -1
}

func mustParseDate(s string) time.Time {
	tt, err := time.ParseInLocation("2006-01-02", s, time.UTC)
	if err != nil {
		panic(err)
	}
	return tt
}
