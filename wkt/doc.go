// Package wkt implements a Well-Known Text (WKT) codec for the geometry
// types defined in the parent cql2 package. It supports the seven OGC
// Simple Features geometry kinds in 2D and 3D, and is permissive about
// whitespace and case, accepting both the canonical and the pragmatic
// MULTIPOINT forms on input while always emitting the canonical form.
package wkt
