package text

import (
	"os"
	"path/filepath"
	"testing"

	cql2 "github.com/example/go-cql2"
)

func seedFromCorpusText(f *testing.F, ext string) {
	f.Helper()
	matches, _ := filepath.Glob(filepath.Join("..", "testdata", "ogc", "*."+ext))
	for _, p := range matches {
		b, err := os.ReadFile(p)
		if err != nil {
			continue
		}
		f.Add(string(b))
	}
}

func seedSyntheticText(f *testing.F) {
	f.Helper()
	seeds := []string{
		"a = 1",
		"TRUE",
		"a AND b",
		"a OR (b AND c)",
		"x BETWEEN 1 AND 10",
		"name LIKE 'foo%'",
		"x IN (1, 2, 3)",
		"x IS NULL",
		"x IS NOT NULL",
		"S_INTERSECTS(geom, POINT(1 2))",
		"T_AFTER(t, TIMESTAMP('2024-01-01T00:00:00Z'))",
		"a + b * c = 10",
		"CASEI(a) = CASEI('foo')",
		"fn(a, b, 'c')",
	}
	for _, s := range seeds {
		f.Add(s)
	}
}

func FuzzParse(f *testing.F) {
	seedFromCorpusText(f, "text")
	seedSyntheticText(f)
	f.Fuzz(func(t *testing.T, input string) {
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("panic on input %q: %v", input, r)
			}
		}()
		_, _ = Parse(input)
	})
}

func FuzzRoundTrip(f *testing.F) {
	seedFromCorpusText(f, "text")
	seedSyntheticText(f)
	f.Fuzz(func(t *testing.T, input string) {
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
