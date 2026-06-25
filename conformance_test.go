package cql2_test

// TestOGCCorpus exercises a vendored snapshot of the OGC CQL2 example
// corpus. For every paired (.text, .json) example under
// testdata/ogc/ the test verifies:
//
//  1. Parse(text)  succeeds → AST_text
//  2. Parse(json)  succeeds → AST_json
//  3. AST_text and AST_json are reflect.DeepEqual (modulo entries in
//     testdata/ogc/diffs.md — currently none).
//  4. Encode(AST_json, EncodingJSON) re-marshals to JSON that is
//     structurally equivalent to the on-disk source (comparison is
//     done via encoding/json round-tripping to canonicalize
//     whitespace, HTML escaping of < / >, and integer-valued floats
//     such as 180.0 ↔ 180; see diffs.md).
//  5. Encode(AST_text, EncodingText) re-parses to an AST that is
//     reflect.DeepEqual to AST_text — the text fixed-point property.
//
// Failures are reported per-example with t.Errorf so a single broken
// pair does not mask others.

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"testing"

	cql2 "github.com/exergy-dev/go-cql2"
	_ "github.com/exergy-dev/go-cql2/codecs"
)

const ogcCorpusDir = "testdata/ogc"

// relaxedJSONByteEquality lists examples whose JSON re-encode is intentionally
// non-byte-equal to the on-disk source. For these, step 4 is satisfied by a
// semantic check (Parse(encoded) == astJSON via cql2.Equal) instead of a
// canonicalized byte comparison. See testdata/ogc/diffs.md for the rationale
// per example.
var relaxedJSONByteEquality = map[string]string{
	"31_func_avg_inline_op":      "inline-op form normalized to function-form on encode: {\"op\":\"avg\",\"args\":...} → {\"function\":{\"name\":\"avg\",\"args\":...}}. Unknown op names parse to *FunctionCall, which always emits the function-form on encode.",
	"32_func_avg_inline_compare": "inline-op form normalized to function-form on encode (nested case)",
	"33_func_buffer_inline_op":   "inline-op form normalized to function-form on encode",
}

// canonicalizeJSON parses b as JSON and returns a normalized representation
// suitable for structural comparison. Numbers are decoded into float64,
// which collapses integer-valued floats (e.g. 180.0 / 180) into a single
// value. Object key order, whitespace and HTML escaping all become
// irrelevant.
func canonicalizeJSON(t *testing.T, name, label string, b []byte) interface{} {
	t.Helper()
	// Compact first so we surface obviously-malformed JSON early.
	var compact bytes.Buffer
	if err := json.Compact(&compact, b); err != nil {
		t.Errorf("%s: %s: json.Compact: %v", name, label, err)
		return nil
	}
	var v interface{}
	if err := json.Unmarshal(compact.Bytes(), &v); err != nil {
		t.Errorf("%s: %s: json.Unmarshal: %v", name, label, err)
		return nil
	}
	return v
}

func TestOGCCorpus(t *testing.T) {
	entries, err := os.ReadDir(ogcCorpusDir)
	if err != nil {
		t.Fatalf("read corpus dir %q: %v", ogcCorpusDir, err)
	}
	var stems []string
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		name := e.Name()
		if !strings.HasSuffix(name, ".text") {
			continue
		}
		stems = append(stems, strings.TrimSuffix(name, ".text"))
	}
	sort.Strings(stems)
	if len(stems) == 0 {
		t.Fatalf("no .text examples found in %q", ogcCorpusDir)
	}
	t.Logf("OGC corpus: %d paired examples", len(stems))

	for _, stem := range stems {
		stem := stem
		t.Run(stem, func(t *testing.T) {
			textPath := filepath.Join(ogcCorpusDir, stem+".text")
			jsonPath := filepath.Join(ogcCorpusDir, stem+".json")

			textBytes, err := os.ReadFile(textPath)
			if err != nil {
				t.Fatalf("%s: read text: %v", stem, err)
			}
			jsonBytes, err := os.ReadFile(jsonPath)
			if err != nil {
				t.Fatalf("%s: read json: %v", stem, err)
			}

			// Step 1: parse text.
			astText, err := cql2.Parse(textBytes)
			if err != nil {
				t.Errorf("%s: Parse(text) failed: %v\ninput: %q", stem, err, textBytes)
				return
			}

			// Step 2: parse json.
			astJSON, err := cql2.Parse(jsonBytes)
			if err != nil {
				t.Errorf("%s: Parse(json) failed: %v\ninput: %s", stem, err, jsonBytes)
				return
			}

			// Step 3: cross-encoding AST equality. Use cql2.Equal instead
			// of reflect.DeepEqual: gts geometries embed an atomic-pointer
			// envelope cache that diverges across parsers, and Equal routes
			// geometry comparison through a stride-aware structural check.
			if !cql2.Equal(astText, astJSON) {
				t.Errorf("%s: AST mismatch between text and JSON parse\n"+
					"  text AST: %#v\n"+
					"  json AST: %#v",
					stem, astText, astJSON)
			}

			// Step 4: JSON re-encode round-trip.
			// Strict (default): canonicalize source vs encoded and require equality.
			// Relaxed (listed in relaxedJSONByteEquality): require only semantic
			// round-trip — Parse(encoded) must be Equal to astJSON.
			encJSON, err := cql2.Encode(astJSON, cql2.EncodingJSON)
			if err != nil {
				t.Errorf("%s: Encode(astJSON, EncodingJSON) failed: %v", stem, err)
			} else if reason, relaxed := relaxedJSONByteEquality[stem]; relaxed {
				ast2, err := cql2.Parse(encJSON)
				if err != nil {
					t.Errorf("%s: re-Parse(encoded JSON) failed: %v\nencoded: %s\nrelaxed reason: %s",
						stem, err, encJSON, reason)
				} else if !cql2.Equal(astJSON, ast2) {
					t.Errorf("%s: JSON semantic round-trip failed\n"+
						"  encoded: %s\n"+
						"  relaxed reason: %s",
						stem, encJSON, reason)
				}
			} else {
				wantCanon := canonicalizeJSON(t, stem, "source", jsonBytes)
				gotCanon := canonicalizeJSON(t, stem, "encoded", encJSON)
				if wantCanon != nil && gotCanon != nil && !reflect.DeepEqual(wantCanon, gotCanon) {
					t.Errorf("%s: JSON re-encode does not match source (after canonicalization)\n"+
						"  source : %s\n"+
						"  encoded: %s",
						stem, jsonBytes, encJSON)
				}
			}

			// Step 5: text fixed-point.
			encText, err := cql2.Encode(astText, cql2.EncodingText)
			if err != nil {
				t.Errorf("%s: Encode(astText, EncodingText) failed: %v", stem, err)
				return
			}
			astText2, err := cql2.Parse(encText)
			if err != nil {
				t.Errorf("%s: re-Parse(encoded text) failed: %v\nencoded: %q",
					stem, err, encText)
				return
			}
			if !reflect.DeepEqual(astText, astText2) {
				t.Errorf("%s: text fixed-point failed\n"+
					"  original text: %q\n"+
					"  encoded text : %q\n"+
					"  original AST : %#v\n"+
					"  re-parsed AST: %#v",
					stem, textBytes, encText, astText, astText2)
			}
		})
	}
}
