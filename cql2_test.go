package cql2_test

import (
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	cql2 "github.com/exergy-dev/go-cql2"
	_ "github.com/exergy-dev/go-cql2/codecs"
	cqljson "github.com/exergy-dev/go-cql2/json"
	cqltext "github.com/exergy-dev/go-cql2/text"
)

// --- auto-detect ---------------------------------------------------------

func TestParse_AutoDetectJSONObject(t *testing.T) {
	jsonIn := []byte(`{"op":"=","args":[{"property":"a"},1]}`)
	textIn := []byte(`a = 1`)
	nJ, err := cql2.Parse(jsonIn)
	if err != nil {
		t.Fatalf("parse json: %v", err)
	}
	nT, err := cql2.Parse(textIn)
	if err != nil {
		t.Fatalf("parse text: %v", err)
	}
	if !cql2.Equal(nJ, nT) {
		t.Fatalf("auto-detected ASTs differ:\n json=%#v\n text=%#v", nJ, nT)
	}
}

func TestParse_AutoDetectJSONArray(t *testing.T) {
	// Array form is a CQL2-JSON ArrayLit.
	n, err := cql2.Parse([]byte(`[1,2,3]`))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if _, ok := n.(*cql2.ArrayLit); !ok {
		t.Fatalf("want *ArrayLit, got %T", n)
	}
}

func TestParse_AmbiguousBareLiteral(t *testing.T) {
	// "true" parses as JSON BoolLit.
	n, err := cql2.Parse([]byte(`true`))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if b, ok := n.(*cql2.BoolLit); !ok || !b.Value {
		t.Fatalf("want BoolLit(true), got %#v", n)
	}
	// A bare property reference in Text falls through (JSON parse fails).
	n, err = cql2.Parse([]byte(`some_prop`))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if pr, ok := n.(*cql2.PropertyRef); !ok || pr.Name != "some_prop" {
		t.Fatalf("want PropertyRef(some_prop), got %#v", n)
	}
}

func TestParse_EmptyInput(t *testing.T) {
	if _, err := cql2.Parse(nil); err == nil {
		t.Fatalf("expected error for empty input")
	}
}

// --- MustParse -----------------------------------------------------------

func TestMustParse_Success(t *testing.T) {
	n := cql2.MustParse([]byte(`a = 1`))
	if n == nil {
		t.Fatalf("nil node")
	}
}

func TestMustParse_Panics(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Fatalf("expected panic")
		}
	}()
	_ = cql2.MustParse([]byte(`@@@`))
}

// --- Encode dispatch -----------------------------------------------------

func TestEncodeDispatch(t *testing.T) {
	n := cql2.MustParse([]byte(`a = 1`))

	gotT, err := cql2.Encode(n, cql2.EncodingText)
	if err != nil {
		t.Fatalf("encode text: %v", err)
	}
	wantT, err := cqltext.Encode(n)
	if err != nil {
		t.Fatalf("text.Encode: %v", err)
	}
	if string(gotT) != wantT {
		t.Fatalf("text mismatch:\n got=%q\nwant=%q", gotT, wantT)
	}

	gotJ, err := cql2.Encode(n, cql2.EncodingJSON)
	if err != nil {
		t.Fatalf("encode json: %v", err)
	}
	wantJ, err := cqljson.Encode(n)
	if err != nil {
		t.Fatalf("json.Encode: %v", err)
	}
	if string(gotJ) != string(wantJ) {
		t.Fatalf("json mismatch:\n got=%s\nwant=%s", gotJ, wantJ)
	}
}

// --- end-to-end: §1 example ---------------------------------------------

func TestEndToEnd_HousesWithPools(t *testing.T) {
	// "Find houses with at least 4 bedrooms and a swimming pool"
	src := `bedrooms >= 4 AND pool = TRUE`
	n1 := cql2.MustParse([]byte(src))

	asJSON, err := cql2.Encode(n1, cql2.EncodingJSON)
	if err != nil {
		t.Fatalf("encode json: %v", err)
	}
	n2, err := cql2.Parse(asJSON)
	if err != nil {
		t.Fatalf("re-parse json: %v", err)
	}
	asText, err := cql2.Encode(n2, cql2.EncodingText)
	if err != nil {
		t.Fatalf("encode text: %v", err)
	}
	n3, err := cql2.Parse(asText)
	if err != nil {
		t.Fatalf("re-parse text: %v", err)
	}
	if !cql2.Equal(n1, n2) {
		t.Fatalf("n1 != n2 after json round-trip:\n n1=%#v\n n2=%#v", n1, n2)
	}
	if !cql2.Equal(n2, n3) {
		t.Fatalf("n2 != n3 after text round-trip")
	}
}

// --- end-to-end: Appendix A 4-clause STAC filter ------------------------

func TestEndToEnd_StacFilter(t *testing.T) {
	src := `S_INTERSECTS(geometry, POLYGON((-77.05 38.87, -77.04 38.87, -77.04 38.88, -77.05 38.88, -77.05 38.87))) AND ` +
		`T_AFTER(datetime, TIMESTAMP('2020-01-01T00:00:00Z')) AND ` +
		`collection = 'sentinel-2' AND ` +
		`identifier LIKE 'S2%'`
	n1 := cql2.MustParse([]byte(src))

	asJSON, err := cql2.Encode(n1, cql2.EncodingJSON)
	if err != nil {
		t.Fatalf("encode json: %v", err)
	}
	n2, err := cql2.Parse(asJSON)
	if err != nil {
		t.Fatalf("re-parse json: %v", err)
	}
	asText, err := cql2.Encode(n2, cql2.EncodingText)
	if err != nil {
		t.Fatalf("encode text: %v", err)
	}
	n3, err := cql2.Parse(asText)
	if err != nil {
		t.Fatalf("re-parse text: %v\n input=%s", err, asText)
	}
	if !cql2.Equal(n1, n2) {
		t.Fatalf("n1 != n2 after json round-trip")
	}
	if !cql2.Equal(n2, n3) {
		t.Fatalf("n2 != n3 after text round-trip")
	}
}

// --- conformance gating --------------------------------------------------

func confErr(t *testing.T, err error) *cql2.ConformanceError {
	t.Helper()
	if err == nil {
		t.Fatalf("expected *ConformanceError, got nil")
	}
	var ce *cql2.ConformanceError
	if !errors.As(err, &ce) {
		t.Fatalf("expected *ConformanceError, got %T: %v", err, err)
	}
	return ce
}

func TestConformance_BasicRejectsLike(t *testing.T) {
	_, err := cql2.Parse([]byte(`name LIKE 'A%'`),
		cql2.WithConformance(cql2.ConfBasic))
	ce := confErr(t, err)
	if ce.Feature != "LIKE" {
		t.Fatalf("Feature=%q, want LIKE", ce.Feature)
	}
}

func TestConformance_BasicRejectsBetween(t *testing.T) {
	_, err := cql2.Parse([]byte(`a BETWEEN 1 AND 2`),
		cql2.WithConformance(cql2.ConfBasic))
	ce := confErr(t, err)
	if ce.Feature != "BETWEEN" {
		t.Fatalf("Feature=%q, want BETWEEN", ce.Feature)
	}
}

func TestConformance_BasicRejectsSpatial(t *testing.T) {
	_, err := cql2.Parse([]byte(`s_intersects(g, POINT(1 2))`),
		cql2.WithConformance(cql2.ConfBasic))
	ce := confErr(t, err)
	if !strings.Contains(ce.Feature, "S_INTERSECTS") {
		t.Fatalf("Feature=%q, want S_INTERSECTS", ce.Feature)
	}
}

func TestConformance_AdvCmpAcceptsLikeRejectsPropProp(t *testing.T) {
	c := cql2.ConfBasic | cql2.ConfAdvancedComparison
	if _, err := cql2.Parse([]byte(`name LIKE 'A%'`), cql2.WithConformance(c)); err != nil {
		t.Fatalf("LIKE rejected: %v", err)
	}
	_, err := cql2.Parse([]byte(`a > b`), cql2.WithConformance(c))
	ce := confErr(t, err)
	if !strings.Contains(ce.Feature, "property") {
		t.Fatalf("Feature=%q, want property-related", ce.Feature)
	}
}

// TestConformance_PropertyProperty covers the expanded property-property
// shape detection — beyond the legacy "both args are PropertyRef" case,
// we also flag literal-LHS comparisons and property-on-RHS spatial /
// temporal / array predicates per /req/property-property.
func TestConformance_PropertyProperty(t *testing.T) {
	cases := []struct {
		name string
		conf cql2.Conformance
		src  string
	}{
		{
			name: "literal LHS comparison",
			conf: cql2.ConfBasic,
			src:  `'København'=name`,
		},
		{
			name: "literal LHS spatial",
			conf: cql2.ConfBasic | cql2.ConfBasicSpatial,
			src:  `s_intersects(POINT(1 2), geom)`,
		},
		{
			name: "property RHS spatial",
			conf: cql2.ConfBasic | cql2.ConfBasicSpatial | cql2.ConfSpatial,
			src:  `s_within(geom, other_geom)`,
		},
		{
			name: "property RHS temporal",
			conf: cql2.ConfBasic | cql2.ConfTemporal,
			src:  `t_after(start, finish)`,
		},
		{
			name: "property RHS array",
			conf: cql2.ConfBasic | cql2.ConfArray,
			src:  `a_contains(layers, allowed)`,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, err := cql2.Parse([]byte(tc.src), cql2.WithConformance(tc.conf))
			ce := confErr(t, err)
			if !strings.Contains(ce.Feature, "property") {
				t.Fatalf("Feature=%q, want property-related", ce.Feature)
			}
		})
	}
}

// TestConformance_BasicShapesStillAccepted ensures the expanded check
// does not over-trigger for legitimate Basic-CQL2 forms.
func TestConformance_BasicShapesStillAccepted(t *testing.T) {
	cases := []struct {
		conf cql2.Conformance
		src  string
	}{
		{cql2.ConfBasic, `name = 'København'`},
		{cql2.ConfBasic | cql2.ConfBasicSpatial, `s_intersects(geom, POINT(1 2))`},
		{cql2.ConfBasic | cql2.ConfTemporal, `t_after(start, TIMESTAMP('2024-01-01T00:00:00Z'))`},
		{cql2.ConfBasic | cql2.ConfArray, `a_contains(layers, ('a','b'))`},
		// Property-on-LHS, arithmetic-on-RHS does not require PP — only
		// Arithmetic class (which is already in cfg here).
		{cql2.ConfBasic | cql2.ConfArithmetic, `vehicle_height > bridge_clearance - 1`},
	}
	for _, tc := range cases {
		if _, err := cql2.Parse([]byte(tc.src), cql2.WithConformance(tc.conf)); err != nil {
			t.Errorf("Parse(%q) with %v: unexpected error: %v", tc.src, tc.conf, err)
		}
	}
}

// TestConformance_BasicSpatialPlus enforces the OGC granularity split:
// basic-spatial-functions only allows S_INTERSECTS with Point/BBox
// literals; using LineString, Polygon, MultiPoint, etc. requires the
// basic-spatial-functions-plus class (or the full ConfSpatial).
func TestConformance_BasicSpatialPlus(t *testing.T) {
	basic := cql2.ConfBasic | cql2.ConfBasicSpatial

	// Point and BBox are accepted under basic alone.
	for _, src := range []string{
		`s_intersects(geom, POINT(1 2))`,
		`s_intersects(geom, BBOX(0,0,10,10))`,
	} {
		if _, err := cql2.Parse([]byte(src), cql2.WithConformance(basic)); err != nil {
			t.Errorf("Parse(%q) under basic-spatial: unexpected error %v", src, err)
		}
	}

	// Non-Point geometry literals require the plus class.
	rejected := []string{
		`s_intersects(geom, LINESTRING(0 0, 1 1))`,
		`s_intersects(geom, POLYGON((0 0,1 0,1 1,0 1,0 0)))`,
		`s_intersects(geom, MULTIPOINT(0 0, 1 1))`,
	}
	for _, src := range rejected {
		_, err := cql2.Parse([]byte(src), cql2.WithConformance(basic))
		ce := confErr(t, err)
		if !strings.Contains(ce.Feature, "S_INTERSECTS") {
			t.Errorf("Parse(%q): Feature=%q, want S_INTERSECTS-related", src, ce.Feature)
		}
	}

	// Adding the plus class accepts them.
	plus := basic | cql2.ConfBasicSpatialPlus
	for _, src := range rejected {
		if _, err := cql2.Parse([]byte(src), cql2.WithConformance(plus)); err != nil {
			t.Errorf("Parse(%q) under basic+plus: unexpected error %v", src, err)
		}
	}

	// ConfSpatial alone (with basic) also covers it.
	full := basic | cql2.ConfSpatial
	for _, src := range rejected {
		if _, err := cql2.Parse([]byte(src), cql2.WithConformance(full)); err != nil {
			t.Errorf("Parse(%q) under full spatial: unexpected error %v", src, err)
		}
	}
}

func TestConformance_AllAcceptsEverything(t *testing.T) {
	src := `name LIKE 'A%' AND a BETWEEN 1 AND 2 AND s_intersects(g, POINT(1 2)) AND foo(1)`
	if _, err := cql2.Parse([]byte(src), cql2.WithConformance(cql2.ConfAll)); err != nil {
		t.Fatalf("ConfAll rejected valid input: %v", err)
	}
}

func TestConformance_FunctionRejected(t *testing.T) {
	_, err := cql2.Parse([]byte(`my_fn(1, 2)`),
		cql2.WithConformance(cql2.ConfBasic))
	ce := confErr(t, err)
	if ce.Feature != "function call" {
		t.Fatalf("Feature=%q, want \"function call\"", ce.Feature)
	}
}

// --- WithPositions -------------------------------------------------------

// TestWithPositions_TextCoverage asserts that every node in a richly-shaped
// text input has a position recorded. This is the v1 invariant downstream
// consumers rely on for source-mapped error reporting.
func TestWithPositions_TextCoverage(t *testing.T) {
	src := `(a = 1 OR b <> 2) AND NOT c BETWEEN 0 AND 10 ` +
		`OR d IN (1, 2) ` +
		`OR e IS NULL ` +
		`OR f LIKE 'A%' ` +
		`OR g + h - i * j / k % l div m ^ n > 0 ` +
		`OR S_INTERSECTS(geom, POINT(1 2)) ` +
		`OR T_AFTER(start, INTERVAL('2024-01-01', '..')) ` +
		`OR S_INTERSECTS(geom, BBOX(0,0,10,10)) ` +
		`OR foo(x) > 0 ` +
		`OR active = TRUE ` +
		`OR active = FALSE ` +
		`OR x = DATE('2024-01-01') ` +
		`OR x = TIMESTAMP('2024-01-01T00:00:00Z')`

	var pm *cql2.PositionMap
	n, err := cql2.Parse([]byte(src), cql2.WithPositions(&pm))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	var missing []string
	cql2.Inspect(n, func(node cql2.Node) bool {
		if node == nil {
			return true
		}
		// Every visited node, including IntervalLit endpoints (which are
		// ordinary Nodes), should have a recorded position.
		if _, ok := pm.Get(node); !ok {
			missing = append(missing, fmt.Sprintf("%T", node))
		}
		return true
	})
	if len(missing) > 0 {
		t.Fatalf("nodes without recorded position: %v", missing)
	}
}

func TestWithPositions_Populated(t *testing.T) {
	var pm *cql2.PositionMap
	n, err := cql2.Parse([]byte(`a = 1`), cql2.WithPositions(&pm))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if pm == nil {
		t.Fatalf("PositionMap not allocated")
	}
	// At minimum, recursive children should have positions even if the
	// root Op does not. Walk and require >=1 node has a recorded position.
	found := false
	cql2.Inspect(n, func(node cql2.Node) bool {
		if node == nil {
			return true
		}
		if _, ok := pm.Get(node); ok {
			found = true
		}
		return true
	})
	if !found {
		t.Fatalf("PositionMap empty after parse")
	}
}

// TestWithPositions_JSONCoverage parallels the text-side coverage test
// for JSON. Every parsed node carries a JSONPath in its recorded Pos.
func TestWithPositions_JSONCoverage(t *testing.T) {
	src := `{"op":"and","args":[
	  {"op":"=","args":[{"property":"a"},1]},
	  {"op":"<>","args":[{"property":"b"},"x"]},
	  {"op":"between","args":[{"property":"c"},0,10]},
	  {"op":"in","args":[{"property":"d"},[1,2]]},
	  {"op":"isNull","args":[{"property":"e"}]},
	  {"op":"like","args":[{"property":"f"},"A%"]},
	  {"op":"+","args":[{"property":"g"},{"property":"h"}]},
	  {"op":"s_intersects","args":[{"property":"geom"},{"type":"Point","coordinates":[1,2]}]},
	  {"op":"s_intersects","args":[{"property":"geom"},{"bbox":[0,0,10,10]}]},
	  {"op":"t_after","args":[{"property":"start"},{"interval":["2024-01-01",".."]}]},
	  {"op":"=","args":[{"property":"x"},{"date":"2024-01-01"}]},
	  {"op":"=","args":[{"property":"x"},{"timestamp":"2024-01-01T00:00:00Z"}]},
	  {"op":"=","args":[{"property":"x"},true]},
	  {"function":{"name":"foo","args":[1]}}
	]}`
	var pm *cql2.PositionMap
	n, err := cql2.Parse([]byte(src), cql2.WithPositions(&pm))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	var missing []string
	cql2.Inspect(n, func(node cql2.Node) bool {
		if node == nil {
			return true
		}
		if _, ok := pm.Get(node); !ok {
			missing = append(missing, fmt.Sprintf("%T", node))
		}
		return true
	})
	if len(missing) > 0 {
		t.Fatalf("nodes without recorded position: %v", missing)
	}
}

func TestWithPositions_JSON(t *testing.T) {
	var pm *cql2.PositionMap
	n, err := cql2.Parse([]byte(`{"op":"=","args":[{"property":"a"},1]}`),
		cql2.WithPositions(&pm))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if pm == nil {
		t.Fatalf("PositionMap not allocated")
	}
	if p, ok := pm.Get(n); !ok || p.JSONPath != "" {
		t.Fatalf("root node position missing or wrong: %+v ok=%v", p, ok)
	}
}

// --- WithDateTimezone ----------------------------------------------------

// TestWithDateTimezone_Text pins that bare DATE('YYYY-MM-DD') literals in
// CQL2-Text are parsed in the configured location, not silently in UTC.
// Default behavior remains UTC when the option is not supplied.
func TestWithDateTimezone_Text(t *testing.T) {
	loc := time.FixedZone("X", 3600)
	n, err := cql2.Parse([]byte(`DATE('2024-01-01')`), cql2.WithDateTimezone(loc))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	d, ok := n.(*cql2.DateLit)
	if !ok {
		t.Fatalf("got %T, want *DateLit", n)
	}
	if d.Value.Location().String() != "X" {
		t.Fatalf("location=%q, want X", d.Value.Location())
	}
	// Default (no option): UTC.
	n2, err := cql2.Parse([]byte(`DATE('2024-01-01')`))
	if err != nil {
		t.Fatalf("parse default: %v", err)
	}
	if loc2 := n2.(*cql2.DateLit).Value.Location(); loc2 != time.UTC {
		t.Fatalf("default location=%v, want UTC", loc2)
	}
}

// TestWithDateTimezone_JSON pins the equivalent behavior for CQL2-JSON.
func TestWithDateTimezone_JSON(t *testing.T) {
	loc := time.FixedZone("X", 3600)
	n, err := cql2.Parse([]byte(`{"date":"2024-01-01"}`), cql2.WithDateTimezone(loc))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if n.(*cql2.DateLit).Value.Location().String() != "X" {
		t.Fatalf("location=%q, want X", n.(*cql2.DateLit).Value.Location())
	}
}

// TestWithDateTimezone_Interval pins that DATE-shaped INTERVAL endpoints
// in both encodings honor WithDateTimezone.
func TestWithDateTimezone_Interval(t *testing.T) {
	loc := time.FixedZone("X", 3600)

	nT, err := cql2.Parse([]byte(`INTERVAL('2024-01-01','2024-12-31')`),
		cql2.WithDateTimezone(loc))
	if err != nil {
		t.Fatalf("parse text: %v", err)
	}
	iv := nT.(*cql2.IntervalLit)
	if iv.Start.(*cql2.DateLit).Value.Location().String() != "X" {
		t.Fatalf("text start location wrong")
	}
	if iv.End.(*cql2.DateLit).Value.Location().String() != "X" {
		t.Fatalf("text end location wrong")
	}

	nJ, err := cql2.Parse([]byte(`{"interval":["2024-01-01","2024-12-31"]}`),
		cql2.WithDateTimezone(loc))
	if err != nil {
		t.Fatalf("parse json: %v", err)
	}
	ivJ := nJ.(*cql2.IntervalLit)
	if ivJ.Start.(*cql2.DateLit).Value.Location().String() != "X" {
		t.Fatalf("json start location wrong")
	}
}

// --- WithTextStyle -------------------------------------------------------

func TestWithTextStyle_Verbose(t *testing.T) {
	n := cql2.MustParse([]byte(`a = 1 AND b = 2`))
	normal, err := cql2.Encode(n, cql2.EncodingText)
	if err != nil {
		t.Fatalf("encode normal: %v", err)
	}
	verbose, err := cql2.Encode(n, cql2.EncodingText, cql2.WithTextStyle(cql2.StyleVerbose))
	if err != nil {
		t.Fatalf("encode verbose: %v", err)
	}
	if !strings.Contains(string(verbose), "(") || strings.Count(string(verbose), "(") <= strings.Count(string(normal), "(") {
		t.Fatalf("verbose did not add extra parens:\nnormal=%s\nverbose=%s", normal, verbose)
	}
	// Re-parsing the verbose form must produce an Equal AST.
	n2, err := cql2.Parse(verbose)
	if err != nil {
		t.Fatalf("re-parse verbose: %v", err)
	}
	if !cql2.Equal(n, n2) {
		t.Fatalf("verbose round-trip changed AST")
	}
}
