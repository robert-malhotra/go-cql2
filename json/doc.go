// Package json implements the OGC CQL2 JSON encoding (21-065r2).
//
// It exposes Parse and Encode that translate between byte slices and the
// shared cql2 AST. GeoJSON geometry handling is delegated to the
// github.com/exergy-dev/go-cql2/geojson package.
package json
