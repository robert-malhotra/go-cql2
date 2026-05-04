package json

import (
	"os"
	"path/filepath"
	"testing"

	cql2 "github.com/exergy-dev/go-cql2"
)

func seedFromCorpusJSON(f *testing.F, ext string) {
	f.Helper()
	matches, _ := filepath.Glob(filepath.Join("..", "testdata", "ogc", "*."+ext))
	for _, p := range matches {
		b, err := os.ReadFile(p)
		if err != nil {
			continue
		}
		f.Add(b)
	}
}

func seedSyntheticJSON(f *testing.F) {
	f.Helper()
	seeds := [][]byte{
		[]byte(`true`),
		[]byte(`{"op":"=","args":[{"property":"a"},1]}`),
		[]byte(`{"op":"and","args":[{"op":"=","args":[{"property":"a"},1]},{"op":"=","args":[{"property":"b"},2]}]}`),
		[]byte(`{"op":"like","args":[{"property":"name"},"foo%"]}`),
		[]byte(`{"op":"isNull","args":[{"property":"x"}]}`),
		[]byte(`{"timestamp":"2024-01-01T00:00:00Z"}`),
		[]byte(`{"date":"2024-01-01"}`),
		[]byte(`{"interval":["..","2024-01-01"]}`),
		[]byte(`{"bbox":[-180,-90,180,90]}`),
		[]byte(`{"property":"a"}`),
		[]byte(`{"function":{"name":"abs","args":[{"property":"x"}]}}`),
		[]byte(`{"op":"s_intersects","args":[{"property":"geom"},{"type":"Point","coordinates":[1,2]}]}`),
	}
	for _, s := range seeds {
		f.Add(s)
	}
}

func FuzzParse(f *testing.F) {
	seedFromCorpusJSON(f, "json")
	seedSyntheticJSON(f)
	f.Fuzz(func(t *testing.T, input []byte) {
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("panic on input %q: %v", input, r)
			}
		}()
		_, _ = Parse(input)
	})
}

func FuzzRoundTrip(f *testing.F) {
	seedFromCorpusJSON(f, "json")
	seedSyntheticJSON(f)
	f.Fuzz(func(t *testing.T, input []byte) {
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("panic on input %q: %v", input, r)
			}
		}()
		n1, err := Parse(input)
		if err != nil {
			return
		}
		out, err := Encode(n1)
		if err != nil {
			t.Fatalf("encode failed for parseable input %q -> ast: %v", input, err)
		}
		n2, err := Parse(out)
		if err != nil {
			t.Fatalf("re-parse failed: input=%q encoded=%q err=%v", input, out, err)
		}
		if !cql2.Equal(n1, n2) {
			t.Fatalf("round-trip mismatch:\n  input:    %q\n  encoded:  %q\n", input, out)
		}
	})
}
