# go-cql2

A Go library for parsing, encoding, and manipulating
[OGC CQL2](https://docs.ogc.org/is/21-065r2/21-065r2.html) filter
expressions in both **CQL2-Text** and **CQL2-JSON** encodings, sharing a
single AST. Suitable for STAC API and OGC API – Features clients and
servers.

This is a parse / encode / AST library — there is no built-in evaluator.

## Install

```sh
go get github.com/exergy-dev/go-cql2
```

Requires Go 1.23+.

## Quickstart

### Parse text, encode to both forms

```go
package main

import (
    "fmt"

    cql2 "github.com/exergy-dev/go-cql2"
    _ "github.com/exergy-dev/go-cql2/codecs" // register Text + JSON codecs
)

func main() {
    n, err := cql2.Parse([]byte(`(floors > 5 AND material = 'brick') OR swimming_pool = true`))
    if err != nil {
        panic(err)
    }
    asJSON, _ := cql2.Encode(n, cql2.EncodingJSON)
    asText, _ := cql2.Encode(n, cql2.EncodingText)
    fmt.Println(string(asJSON))
    fmt.Println(string(asText))
}
```

`cql2.Parse` auto-detects Text vs. JSON by the first non-whitespace byte;
ambiguous inputs (`true`, `null`, numbers, strings) are tried as JSON
first.

### Walk an AST to extract referenced properties

```go
seen := map[string]struct{}{}
cql2.Inspect(n, func(node cql2.Node) bool {
    if p, ok := node.(*cql2.PropertyRef); ok {
        seen[p.Name] = struct{}{}
    }
    return true
})
```

### Build a filter programmatically

```go
expr := cql2.And(
    cql2.Or(
        cql2.Eq("category", "research"),
        cql2.In("category", "science", "engineering"),
    ),
    cql2.Gte("cloud_cover", 0.0),
    cql2.Lt("cloud_cover", 0.2),
    cql2.TAfter("datetime", time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)),
    cql2.IsNotNull("thumbnail_url"),
)
text, _ := cql2.Encode(expr.Node(), cql2.EncodingText)
```

See the runnable programs under [`examples/`](./examples) — `parse`,
`walk`, `builder`, `spatial`.

## Feature support

| Area | Operators / forms | Status |
|---|---|---|
| Logical | `AND`, `OR`, `NOT` (n-ary, flattened) | ✅ |
| Comparison | `=`, `<>`, `<`, `<=`, `>`, `>=` | ✅ |
| Advanced comparison | `LIKE`, `BETWEEN`, `IN`, `IS [NOT] NULL` | ✅ |
| Case / accent | `CASEI`, `ACCENTI` | ✅ |
| Arithmetic | `+`, `-`, `*`, `/`, `%`, `^`, `div` (precedence + unary `-`) | ✅ |
| Spatial | `S_INTERSECTS`, `S_EQUALS`, `S_DISJOINT`, `S_TOUCHES`, `S_WITHIN`, `S_OVERLAPS`, `S_CROSSES`, `S_CONTAINS` | ✅ |
| Temporal | All 13 Allen-relation ops + `T_AFTER`, `T_BEFORE`, `T_INTERSECTS`, `T_DISJOINT`, `T_EQUALS` | ✅ |
| Array | `A_CONTAINS`, `A_CONTAINEDBY`, `A_EQUALS`, `A_OVERLAPS` | ✅ |
| Property refs | bare (with `:` and `.`) and `"quoted"` | ✅ |
| Literals | bool, null, number (`json.Number` fidelity), single-quoted strings, `TIMESTAMP`, `DATE`, `INTERVAL` (with property-ref endpoints), `BBOX`, geometry | ✅ |
| Geometries (Text) | WKT for all 7 types, optional `Z` dim-tag, `EMPTY` for all multi types | ✅ |
| Geometries (JSON) | RFC 7946 GeoJSON for all 7 types | ✅ |
| Functions | `FunctionCall` + spec-defined op desugaring | ✅ |
| Conformance | `Conformance` bitset; per-class gating in both codecs | ✅ |
| Position tracking | line/col (text), JSON-Pointer (json) for literal/identifier nodes | ✅ |
| Evaluator | Not provided | ❌ |

### Conformance classes

`cql2.Conformance` bitset matches OGC Filter conformance classes:
`ConfBasic`, `ConfAdvancedComparison`, `ConfCaseInsensitive`,
`ConfAccentInsensitive`, `ConfBasicSpatial`, `ConfSpatial`, `ConfTemporal`,
`ConfArray`, `ConfPropertyProperty`, `ConfFunctions`, `ConfArithmetic`,
`ConfAll`. Use `WithConformance(...)` to gate input.

### Options

| Option | Purpose |
|---|---|
| `WithConformance(c)` | Reject features outside the given conformance set |
| `WithPositions(&pm)` | Track source positions in a `*PositionMap` |
| `WithCustomOperators(...)` | Accept extra operator names |
| `WithDateTimezone(loc)` | Interpret bare `DATE('YYYY-MM-DD')` literals in a non-UTC location (default UTC) |
| `WithTextStyle(s)` | `StyleNormal` (default) or `StyleVerbose` text output |

### Documented normalizations

The library makes a few deliberate normalizations on encode. See
[`testdata/ogc/diffs.md`](./testdata/ogc/diffs.md) for the full list and
rationale. The most user-visible:

- Operator names in JSON canonicalize to lowercase (e.g. spec
  `t_finishedBy` parses fine but re-emits as `t_finishedby`).
- Unknown JSON `op` strings parse as `*FunctionCall` and re-emit in
  function-call form.
- `BBoxLit` and geometry coordinates are stored as `float64`, so `180.0`
  re-encodes as `180`.

## Errors

- `*cql2.SyntaxError` — parse errors with line/column (text) or
  RFC 6901 JSON-Pointer (json) location.
- `*cql2.ConformanceError` — input requires a conformance class not in
  the active configuration.
- `*cql2.GeometryError` — malformed WKT or GeoJSON.
- `*cql2.UnliftableError` — builder couldn't lift a Go value to a `Node`.

## Sub-packages

- [`text/`](./text) — CQL2-Text parser + encoder
- [`json/`](./json) — CQL2-JSON parser + encoder
- [`wkt/`](./wkt) — standalone WKT geometry parser/encoder
- [`geojson/`](./geojson) — standalone GeoJSON geometry parser/encoder
- [`codecs/`](./codecs) — convenience: blank-import to register both Text
  and JSON codecs with the root package

The root `cql2` package owns the AST, walk/transform helpers, builder
DSL, options, and conformance gating. The `Parse` and `Encode` entry
points dispatch to whichever codecs have registered themselves; importing
`codecs` (or either of `text` / `json`) wires them up.

## License

See `LICENSE` (when present).
