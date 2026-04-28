package cql2_test

import (
	"errors"
	"strings"
	"testing"

	cql2 "github.com/example/go-cql2"
	_ "github.com/example/go-cql2/codecs"
	cqljson "github.com/example/go-cql2/json"
	cqltext "github.com/example/go-cql2/text"
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
	n := cql2.MustParse(`a = 1`)
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
	_ = cql2.MustParse(`@@@`)
}

// --- Encode dispatch -----------------------------------------------------

func TestEncodeDispatch(t *testing.T) {
	n := cql2.MustParse(`a = 1`)

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
	n1 := cql2.MustParse(src)

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
	n1 := cql2.MustParse(src)

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
		if _, ok := pm.Of(node); ok {
			found = true
		}
		return true
	})
	if !found {
		t.Fatalf("PositionMap empty after parse")
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
	if p, ok := pm.Of(n); !ok || p.JSONPath != "" {
		t.Fatalf("root node position missing or wrong: %+v ok=%v", p, ok)
	}
}

// --- WithTextStyle -------------------------------------------------------

func TestWithTextStyle_Verbose(t *testing.T) {
	n := cql2.MustParse(`a = 1 AND b = 2`)
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
