# Documented round-trip differences

This file records the *expected* differences observed when running the
OGC CQL2 corpus through this implementation. The corpus test
(`TestOGCCorpus`) is aware of each item below and tolerates it via the
described normalization.

## Across-encoding AST equality (text vs JSON)

Every pair in `testdata/ogc/` produces `reflect.DeepEqual` ASTs from
text and from JSON. The v0.6 expansion (examples 24–33) introduced
input forms that had previously been excluded — they now parse cleanly
because of the v0.6 fixes:

- **24–27** (`t_finishedBy`/`t_metBy`/`t_overlappedBy`/`t_startedBy`):
  JSON parser accepts mixed-case temporal-op names (#2). Text parser
  was already case-insensitive on op names.
- **24, 25, 27, 30** (`INTERVAL(starts_at, ends_at)` etc.): both
  parsers and both encoders now route property references through
  `IntervalLit.Start` / `IntervalLit.End` via the extended
  `IntervalEndpoint` interface (#4).
- **28** (`POLYGON Z (...)`): WKT parser accepts the explicit `Z`
  dimension tag (#5); text parser routes geometry literals before the
  bare-property fallback so `POLYGON` followed by `Z` is recognized.
- **31–33** (`{"op":"avg",...}`, `{"op":"Buffer",...}`): JSON parser
  routes unknown op strings to `*FunctionCall` instead of erroring (#3).

## JSON re-encoding canonicalization

When comparing the encoder's JSON output to the on-disk source JSON, the
test does **not** require byte-equality. Instead both sides are decoded
through `encoding/json` to `interface{}` and compared with
`reflect.DeepEqual`. For a small allowlist of examples (see
`relaxedJSONByteEquality` in `conformance_test.go`), even structural
canonicalization is too strict because of legitimate normalizations the
v0.6 codec introduces; for those, the test instead requires only that
`Parse(encoded)` is `cql2.Equal` to the original AST (semantic
round-trip). The allowlist:

- **24–27** — operator names canonicalized: `t_finishedBy` →
  `t_finishedby`. The CQL2 spec says ops are case-insensitive; the AST
  stores the canonical lowercased form for these temporal ops, so the
  encoder emits lowercase.
- **31–33** — inline-op form normalized to function-form on encode:
  `{"op":"avg","args":...}` → `{"function":{"name":"avg","args":...}}`.
  Unknown op names parse to `*FunctionCall`, which always emits the
  function-form on encode. Both forms parse to the same AST.

For the rest of the corpus, the canonicalization handles:

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
