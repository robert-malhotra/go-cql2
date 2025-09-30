# go-cql-text

[![Go Report Card](https://goreportcard.com/badge/github.com/robert-malhotra/go-cql-text)](https://goreportcard.com/report/github.com/robert-malhotra/go-cql-text)
[![GoDoc](https://godoc.org/github.com/robert-malhotra/go-cql-text/text?status.svg)](https://godoc.org/github.com/robert-malhotra/go-cql-text/text)

`go-cql-text` is a Go library that parses [CQL2 (Common Query Language) text expressions](https://www.ogc.org/standards/cql2/) into the same Abstract Syntax Tree (AST) used by [`go-ogc`](https://github.com/planetlabs/go-ogc).  
This allows seamless handling of both CQL2 JSON and CQL2 text in Go.

## Features

- Parse CQL2 **text** into `*filter.Filter` (from `go-ogc`).
- Supports comparison operators: `=`, `<>`, `<`, `<=`, `>`, `>=`.
- Logical operators: `AND`, `OR`, `NOT`, with correct precedence and parentheses.
- Predicates:
  - `LIKE`, `BETWEEN`, `IN`, `IS NULL`
  - Spatial: `S_INTERSECTS`, `S_CONTAINS`, `S_WITHIN`, etc.
  - Temporal: `T_AFTER`, `T_DURING`, `T_INTERSECTS`, etc.
  - Array: `A_CONTAINS`, `A_EQUALS`, `A_OVERLAPS`, etc.
- Function calls: e.g., `Buffer(geom, 10)`, `distance(p1, p2)`.
- Modifiers: `CASEI(...)` (case-insensitive), `ACCENTI(...)` (accent-insensitive).
- Literals: strings, numbers, booleans, arrays, dates, timestamps, intervals.
- Case-insensitive handling of keywords and operators.

## Builder Helpers

The `builder` subpackage (`github.com/robert-malhotra/go-cql-text/builder`) offers
generics-based constructors that make it easy to assemble the
[`go-ogc/filter`](https://github.com/planetlabs/go-ogc/tree/main/filter) AST without
manually instantiating nodes or performing type assertions.

- Typed helpers such as `Property`, `Lit`, `Between`, `In`, `Array`, `Spatial`,
  `Temporal`, and `ArrayCompare` produce fully-formed filter expressions.
- Operation enums (`builder.SpatialContains`, `builder.TemporalDuring`, etc.)
  replace dozens of tiny wrappers while keeping intent clear.
- All helpers return concrete `filter.*` structs, so they compose directly with
  the text parser output and `go-ogc` utilities.

```go
package main

import (
	"time"

	"github.com/planetlabs/go-ogc/filter"
	cql "github.com/robert-malhotra/go-cql-text/builder"
)

func buildFilter() *filter.Filter {
	updated := cql.Temporal(
		cql.TemporalAfter,
		cql.Property("updated_at"),
		cql.Timestamp(time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)),
	)

	region := cql.Spatial(
		cql.SpatialContains,
		cql.Property("geometry"),
		cql.Geometry(map[string]any{"type": "Point", "coordinates": []float64{-122.4, 37.8}}),
	)

	expr := cql.And(updated, region)

	return &filter.Filter{Expression: expr}
}
```

See `builder/builder.go` and `builder/builder_test.go` for a full catalog of
available helpers and usage patterns.
