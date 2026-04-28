package cql2

import (
	"fmt"
	"strings"
)

// SyntaxError is returned when input cannot be parsed.
type SyntaxError struct {
	Encoding Encoding
	At       Pos
	Snippet  string
	Msg      string
	Got      string
	Expected []string
}

func (e *SyntaxError) Error() string {
	var b strings.Builder
	switch e.Encoding {
	case EncodingJSON:
		b.WriteString("json")
	default:
		b.WriteString("text")
	}
	if e.At != (Pos{}) {
		fmt.Fprintf(&b, ":%s", e.At.String())
	}
	b.WriteString(": ")
	if e.Msg != "" {
		b.WriteString(e.Msg)
	} else {
		b.WriteString("syntax error")
	}
	if e.Got != "" {
		fmt.Fprintf(&b, " (got %q", e.Got)
		if len(e.Expected) > 0 {
			fmt.Fprintf(&b, ", expected one of: %s", strings.Join(e.Expected, ", "))
		}
		b.WriteByte(')')
	} else if len(e.Expected) > 0 {
		fmt.Fprintf(&b, " (expected one of: %s)", strings.Join(e.Expected, ", "))
	}
	if e.Snippet != "" {
		fmt.Fprintf(&b, " near %q", e.Snippet)
	}
	return b.String()
}

// ConformanceError is returned when input requires a conformance class
// that is not enabled in the active configuration.
type ConformanceError struct {
	Required Conformance
	Active   Conformance
	Feature  string
	At       Pos
}

func (e *ConformanceError) Error() string {
	var b strings.Builder
	b.WriteString("conformance error")
	if e.At != (Pos{}) {
		fmt.Fprintf(&b, " at %s", e.At.String())
	}
	if e.Feature != "" {
		fmt.Fprintf(&b, ": feature %q", e.Feature)
	}
	fmt.Fprintf(&b, ": required=0x%x active=0x%x", uint32(e.Required), uint32(e.Active))
	return b.String()
}

// GeometryError is returned for malformed WKT or GeoJSON geometry.
type GeometryError struct {
	Encoding Encoding
	At       Pos
	Msg      string
}

func (e *GeometryError) Error() string {
	var b strings.Builder
	switch e.Encoding {
	case EncodingJSON:
		b.WriteString("geojson")
	default:
		b.WriteString("wkt")
	}
	if e.At != (Pos{}) {
		fmt.Fprintf(&b, ":%s", e.At.String())
	}
	b.WriteString(": ")
	if e.Msg != "" {
		b.WriteString(e.Msg)
	} else {
		b.WriteString("invalid geometry")
	}
	return b.String()
}

// UnliftableError is returned when a Go value cannot be lifted to a Node.
type UnliftableError struct {
	Value any
	Msg   string
}

func (e *UnliftableError) Error() string {
	if e.Msg != "" {
		return fmt.Sprintf("cannot lift %T: %s", e.Value, e.Msg)
	}
	return fmt.Sprintf("cannot lift %T", e.Value)
}
