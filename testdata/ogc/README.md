# OGC CQL2 example corpus (vendored snapshot)

This directory contains a curated subset of the official example expressions
that ship with the OGC CQL2 standard, used by `TestOGCCorpus` in the repo
root for round-trip conformance verification.

## Source

- Repository: <https://github.com/opengeospatial/ogcapi-features>
- Path within repo: `cql2/standard/schema/examples/{text,json}`
- Commit: `ecbc50437ab0e338671a8a3b1bda520e50976eed` ("SP edits for publication")
- Cloned: 2026-04-27

The full upstream corpus contains 120 text files and 110 JSON files.
The 33 vendored pairs are a representative selection covering Basic
CQL2, Advanced Comparison, Spatial (basic + 3D), Temporal (including
property-arg INTERVAL), Array, Functions (both forms), CASEI/ACCENTI,
property-property comparison, and arithmetic.

Examples 01–23 were vendored in Wave 4a (v0.5). Examples 24–33 were
added in Wave B (v0.6), once the relevant feature gaps had been
closed:

- 24–27 require case-insensitive JSON op matching (the upstream uses
  `t_finishedBy` etc.) plus `INTERVAL(prop, prop)` round-trip
- 28 requires `POLYGON Z (...)` parsing in WKT
- 31–33 require unknown JSON op strings to fall back to `*FunctionCall`

## Layout

Each example is a pair of files sharing a common stem:

- `<NN>_<short_name>.text` — the CQL2-Text encoding
- `<NN>_<short_name>.json` — the CQL2-JSON encoding (JSON-formatted on
  disk, with indentation / whitespace; the test canonicalizes before
  byte-comparing)

## File-by-file provenance

Mapping from local filename to the upstream basename within
`cql2/standard/schema/examples/{text,json}`:

| Local stem                          | Upstream stem    | Coverage                                    |
| ----------------------------------- | ---------------- | ------------------------------------------- |
| 01_basic_eq                         | example01        | basic equality on a property                |
| 02_lte                              | example10        | `<=` numeric comparison                     |
| 03_eq_bool                          | example14        | equality with boolean literal               |
| 04_and                              | example15        | logical AND                                 |
| 05_not_or                           | example18        | NOT + OR composition                        |
| 06_quoted_prop_lte                  | example33        | double-quoted property identifier           |
| 07_paren_not_like_and               | example43        | parens, NOT LIKE, AND                       |
| 08_in_list                          | example03        | IN with string list                         |
| 09_like                             | clause7_01       | LIKE with wildcard                          |
| 10_like_owner                       | example12        | LIKE on a different property                |
| 11_isnull_not                       | example42        | NOT ... IS NULL                             |
| 12_not_in                           | example40        | NOT IN list                                 |
| 13_property_property                | clause7_19       | property-property arithmetic comparison     |
| 14_arith_compare                    | example72        | arithmetic on rhs of comparison             |
| 15_function_avg_compare             | clause6_02a      | function call (avg) compared to literal     |
| 16_function_count                   | clause6_02c      | function call wrapped in arithmetic         |
| 17_spatial_intersects_polygon       | example07        | S_INTERSECTS with POLYGON                   |
| 18_spatial_point                    | example52        | S_CONTAINS with POINT                       |
| 19_spatial_bbox                     | example50        | S_OVERLAPS with BBOX                        |
| 20_spatial_disjoint_multipolygon    | example47        | S_DISJOINT with MULTIPOLYGON                |
| 21_temporal_equals_date             | example58        | T_EQUALS with DATE literal                  |
| 22_casei_like                       | example28        | CASEI wrapper around LIKE arguments         |
| 23_accenti                          | clause7_05       | ACCENTI wrapper around equality arguments   |

## What the test does

For every pair, `TestOGCCorpus` (in `conformance_test.go`):

1. Parses the text file and the JSON file.
2. Asserts the resulting ASTs are `reflect.DeepEqual` — modulo any
   normalization documented in `diffs.md`.
3. Encodes the JSON-derived AST back to JSON and asserts the bytes match
   a canonicalized form of the source JSON (decoded via `encoding/json`
   and compared structurally — this canonicalizes whitespace,
   integer-valued float spellings like `180.0`/`180`, and Go's HTML
   escaping of `<` / `>`).
4. Encodes the text-derived AST back to text, re-parses, and asserts
   the AST is unchanged (text fixed-point).
