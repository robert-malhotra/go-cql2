# Documented round-trip differences

This file records the *expected* differences observed when running the
OGC CQL2 corpus through this implementation. The corpus test
(`TestOGCCorpus`) is aware of each item below and tolerates it via the
described normalization.

## Across-encoding AST equality (text vs JSON)

After Wave 4a's curation, every pair in `testdata/ogc/` produces
`reflect.DeepEqual` ASTs from text and from JSON, with **no per-example
exceptions**. The corpus was deliberately filtered to that subset; pairs
that exposed an unavoidable structural difference (e.g. unary-minus,
mixed-case operator names, function-call-as-operator in JSON) were
excluded from the vendored set and are noted in the upstream survey
within `README.md`.

If a future Wave reintroduces such an example, document the expected
diff here in the form:

> **`<example_name>`**: text encoding parses to `Op{...}` whereas JSON
> parses to `Op{...}` because <reason>. The test normalizes by
> <strategy>.

## JSON re-encoding canonicalization

When comparing the encoder's JSON output to the on-disk source JSON, the
test does **not** require byte-equality. Instead both sides are decoded
through `encoding/json` to `interface{}` and compared with
`reflect.DeepEqual`. This canonicalizes the following expected
differences:

1. **Whitespace and indentation.** On-disk JSON files are formatted with
   newlines and indentation (the OGC corpus ships them human-readable);
   the encoder emits compact JSON.

2. **HTML escaping of `<` and `>`.** Go's `encoding/json` escapes the
   bytes `<` and `>` to `<` and `>` by default. The library's
   encoder does the same since it uses `json.Marshal` for string-valued
   pieces. Source files contain literal `<`/`>` inside operator names
   like `"op":"<="`.

3. **Integer-valued floats.** Coordinates inside `BBoxLit.Coords
   ([]float64)` and `geojson.Geometry` are stored as `float64`. A
   coordinate written as `180.0` in the source JSON is therefore
   re-emitted as `180`, and `0.0` as `0`. This affects:
     - `19_spatial_bbox.json` (`180.0` → `180` in bbox coords)
     - `20_spatial_disjoint_multipolygon.json` (`180.0`, `0.0` in coords)

   `NumLit` literals (used everywhere outside geometries) preserve the
   source spelling verbatim because the AST keeps the raw
   `json.Number`, so `150.0 > 0` from `16_function_count.json` still
   round-trips byte-for-byte.

## Text fixed-point round-trip

Every example in this corpus satisfies the property
`Parse(Encode(Parse(text))) == Parse(text)`. No exceptions are needed.
