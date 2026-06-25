package cql2

import (
	"fmt"
	"regexp"
	"strings"
)

// Validate walks n and returns the first arity / shape violation it finds,
// or nil if the AST is structurally valid per the CQL2 specification.
//
// Validate is invoked automatically at the end of text.Parse and json.Parse,
// so successfully-parsed inputs are already validated. Callers who construct
// ASTs by hand (e.g. via the builder) can invoke Validate directly to catch
// programmatic mistakes before encoding.
//
// Validate is structural only; it does not type-check operands (e.g. it does
// not verify that the RHS of S_INTERSECTS is geometry-shaped). Type checks
// are left to a future pass.
func Validate(n Node) error {
	if n == nil {
		return &ValidationError{Msg: "nil node", Node: nil}
	}
	switch x := n.(type) {
	case *Op:
		if err := validateOp(x); err != nil {
			return err
		}
		for _, a := range x.Args {
			if err := Validate(a); err != nil {
				return err
			}
		}
	case *FunctionCall:
		if x.Name == "" {
			return &ValidationError{Function: x.Name, Msg: "missing function name", Node: x}
		}
		if !isValidFunctionName(x.Name) {
			return &ValidationError{
				Function: x.Name,
				Msg:      "name is not a valid identifier ([A-Za-z_][A-Za-z0-9_]*)",
				Node:     x,
			}
		}
		if isReservedFunctionName(x.Name) {
			return &ValidationError{
				Function: x.Name,
				Msg:      "name collides with a reserved CQL2 keyword; use the equivalent *Op or typed literal node",
				Node:     x,
			}
		}
		for _, a := range x.Args {
			if err := Validate(a); err != nil {
				return err
			}
		}
	case *ArrayLit:
		for _, e := range x.Elements {
			if err := Validate(e); err != nil {
				return err
			}
		}
	case *IntervalLit:
		for _, ep := range [2]Node{x.Start, x.End} {
			if !isValidIntervalEndpoint(ep) {
				return &ValidationError{Msg: "interval endpoint must be a timestamp, date, property, function, or \"..\"", Node: x}
			}
			if err := Validate(ep); err != nil {
				return err
			}
		}
	}
	return nil
}

// isValidIntervalEndpoint reports whether n is an admissible IntervalLit
// endpoint per the CQL2 grammar. Used by both Validate and the builder.
func isValidIntervalEndpoint(n Node) bool {
	switch n.(type) {
	case *TimestampLit, *DateLit, *PropertyRef, *FunctionCall, *Unbounded:
		return true
	}
	return false
}

// validateOp checks the arity and shape of a single *Op node. Arity rules
// come from opTable; OpIn additionally requires a non-empty array literal.
func validateOp(x *Op) error {
	switch n := opTable[x.Op].arity; {
	case n == -1:
		if len(x.Args) < 2 {
			return arityErr(x, "requires at least 2 arguments")
		}
	case n > 0:
		if len(x.Args) != n {
			return arityErr(x, fmt.Sprintf("requires exactly %d argument%s", n, plural(n)))
		}
	}
	if x.Op == OpIn {
		arr, ok := x.Args[1].(*ArrayLit)
		if !ok {
			return &ValidationError{Op: x.Op, Msg: "second argument must be an array literal", Node: x}
		}
		if len(arr.Elements) == 0 {
			return &ValidationError{Op: x.Op, Msg: "list must contain at least one element", Node: x}
		}
	}
	return nil
}

func plural(n int) string {
	if n == 1 {
		return ""
	}
	return "s"
}

func arityErr(x *Op, msg string) error {
	return &ValidationError{
		Op:   x.Op,
		Msg:  msg,
		Node: x,
	}
}

// reservedFunctionNames is the set of identifiers that, if used as a
// FunctionCall.Name, would be canonicalised by the parser to a typed
// literal or *Op node — breaking round-trip for any builder-constructed
// AST. The set covers the operator-keyword functions, typed-literal
// constructors (DATE/TIMESTAMP/INTERVAL/BBOX), and the WKT geometry
// keywords. Comparison is case-insensitive because both the text and
// JSON parsers match these names case-insensitively.
var reservedFunctionNames = map[string]struct{}{
	"casei": {}, "accenti": {},
	"date": {}, "timestamp": {}, "interval": {}, "bbox": {},
	"point": {}, "linestring": {}, "polygon": {},
	"multipoint": {}, "multilinestring": {}, "multipolygon": {},
	"geometrycollection": {},
	"s_intersects":       {}, "s_equals": {}, "s_disjoint": {},
	"s_touches": {}, "s_within": {}, "s_overlaps": {},
	"s_crosses": {}, "s_contains": {},
	"t_after": {}, "t_before": {}, "t_contains": {}, "t_disjoint": {},
	"t_during": {}, "t_equals": {}, "t_finishedby": {}, "t_finishes": {},
	"t_intersects": {}, "t_meets": {}, "t_metby": {}, "t_overlappedby": {},
	"t_overlaps": {}, "t_startedby": {}, "t_starts": {},
	"a_contains": {}, "a_containedby": {}, "a_equals": {}, "a_overlaps": {},
}

func isReservedFunctionName(s string) bool {
	_, ok := reservedFunctionNames[strings.ToLower(s)]
	return ok
}

// functionNameRe is the CQL2 identifier grammar for function names. Function
// names are emitted unquoted in the text encoding, so any deviation produces
// output the parser would later reject.
var functionNameRe = regexp.MustCompile(`^[A-Za-z_][A-Za-z0-9_]*$`)

// isValidFunctionName reports whether s matches [A-Za-z_][A-Za-z0-9_]*.
func isValidFunctionName(s string) bool {
	return functionNameRe.MatchString(s)
}
