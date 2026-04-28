package cql2

// Conformance is a bitset of OGC API - Filter conformance classes.
type Conformance uint32

const (
	ConfBasic Conformance = 1 << iota
	ConfAdvancedComparison
	ConfCaseInsensitive
	ConfAccentInsensitive
	ConfBasicSpatial
	ConfSpatial
	ConfTemporal
	ConfArray
	ConfPropertyProperty
	ConfFunctions
	ConfArithmetic

	// ConfAll enables every conformance class, including future bits.
	ConfAll = ^Conformance(0)
)

// Has reports whether c includes every flag in want.
func (c Conformance) Has(want Conformance) bool {
	return c&want == want
}
