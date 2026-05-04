package cql2_test

// Benchmarks for Parse and Encode across both encodings, using a fixed
// set of OGC corpus inputs as representative workloads. These pin a v1
// performance baseline; regressions show up as ns/op deltas in CI or
// `go test -bench` runs.

import (
	"os"
	"path/filepath"
	"testing"

	cql2 "github.com/exergy-dev/go-cql2"
	_ "github.com/exergy-dev/go-cql2/codecs"
)

// benchStems are corpus examples spanning the operator/literal surface
// without re-running the full corpus (which would inflate benchmark
// runtime without adding signal).
var benchStems = []string{
	"01_basic_eq",                   // simplest comparison
	"04_and",                        // logical chain
	"08_in_list",                    // IN with literals
	"14_arith_compare",              // arithmetic
	"15_function_avg_compare",       // function call
	"17_spatial_intersects_polygon", // spatial + polygon literal
	"21_temporal_equals_date",       // temporal + date literal
	"22_casei_like",                 // CASEI + LIKE
	"28_3d_polygon_within",          // 3D polygon
	"29_t_intersects_interval",      // interval literal
}

type benchInput struct {
	stem    string
	textSrc []byte
	jsonSrc []byte
	textAST cql2.Node
	jsonAST cql2.Node
}

func loadBenchInputs(b *testing.B) []benchInput {
	b.Helper()
	out := make([]benchInput, 0, len(benchStems))
	for _, stem := range benchStems {
		t, err := os.ReadFile(filepath.Join("testdata", "ogc", stem+".text"))
		if err != nil {
			b.Fatalf("read %s.text: %v", stem, err)
		}
		j, err := os.ReadFile(filepath.Join("testdata", "ogc", stem+".json"))
		if err != nil {
			b.Fatalf("read %s.json: %v", stem, err)
		}
		tAST, err := cql2.Parse(t)
		if err != nil {
			b.Fatalf("parse %s.text: %v", stem, err)
		}
		jAST, err := cql2.Parse(j)
		if err != nil {
			b.Fatalf("parse %s.json: %v", stem, err)
		}
		out = append(out, benchInput{stem, t, j, tAST, jAST})
	}
	return out
}

func BenchmarkParseText(b *testing.B) {
	inputs := loadBenchInputs(b)
	for _, in := range inputs {
		in := in
		b.Run(in.stem, func(b *testing.B) {
			b.SetBytes(int64(len(in.textSrc)))
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				if _, err := cql2.Parse(in.textSrc); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}

func BenchmarkParseJSON(b *testing.B) {
	inputs := loadBenchInputs(b)
	for _, in := range inputs {
		in := in
		b.Run(in.stem, func(b *testing.B) {
			b.SetBytes(int64(len(in.jsonSrc)))
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				if _, err := cql2.Parse(in.jsonSrc); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}

func BenchmarkEncodeText(b *testing.B) {
	inputs := loadBenchInputs(b)
	for _, in := range inputs {
		in := in
		b.Run(in.stem, func(b *testing.B) {
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				if _, err := cql2.Encode(in.textAST, cql2.EncodingText); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}

func BenchmarkEncodeJSON(b *testing.B) {
	inputs := loadBenchInputs(b)
	for _, in := range inputs {
		in := in
		b.Run(in.stem, func(b *testing.B) {
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				if _, err := cql2.Encode(in.jsonAST, cql2.EncodingJSON); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}
