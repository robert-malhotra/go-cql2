// Package text implements a parser and encoder for the CQL2 Text encoding
// (OGC 21-065r2 §7).
//
// Parse converts a CQL2 Text source string into an AST rooted at a
// cql2.Node. Encode converts an AST back to canonical CQL2 Text.
//
// The parser is a hand-written recursive-descent parser. It honours the
// following cql2.Option values: WithConformance, WithPositions,
// WithCustomOperators, WithDateTimezone. The encoder honours WithTextStyle.
package text
