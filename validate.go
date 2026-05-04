package cql2

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
		// Endpoints are non-Node sentinels (Unbounded) or Nodes; only the
		// latter need recursive validation.
		if sn := endpointAsNode(x.Start); sn != nil {
			if err := Validate(sn); err != nil {
				return err
			}
		}
		if en := endpointAsNode(x.End); en != nil {
			if err := Validate(en); err != nil {
				return err
			}
		}
	}
	return nil
}

// validateOp checks the arity and shape of a single *Op node.
func validateOp(x *Op) error {
	switch x.Op {
	case OpAnd, OpOr:
		if len(x.Args) < 2 {
			return arityErr(x, "requires at least 2 arguments")
		}
	case OpNot:
		if len(x.Args) != 1 {
			return arityErr(x, "requires exactly 1 argument")
		}
	case OpEq, OpNeq, OpLt, OpLte, OpGt, OpGte,
		OpAdd, OpSub, OpMul, OpDiv, OpMod, OpPow, OpIDiv,
		OpLike,
		OpSIntersects, OpSEquals, OpSDisjoint, OpSTouches,
		OpSWithin, OpSOverlaps, OpSCrosses, OpSContains,
		OpTAfter, OpTBefore, OpTContains, OpTDisjoint,
		OpTDuring, OpTEquals, OpTFinishedBy, OpTFinishes,
		OpTIntersects, OpTMeets, OpTMetBy, OpTOverlappedBy,
		OpTOverlaps, OpTStartedBy, OpTStarts,
		OpAContains, OpAContainedBy, OpAEquals, OpAOverlaps:
		if len(x.Args) != 2 {
			return arityErr(x, "requires exactly 2 arguments")
		}
	case OpBetween:
		if len(x.Args) != 3 {
			return arityErr(x, "requires exactly 3 arguments (value, lo, hi)")
		}
	case OpIn:
		if len(x.Args) != 2 {
			return arityErr(x, "requires exactly 2 arguments (value, list)")
		}
		arr, ok := x.Args[1].(*ArrayLit)
		if !ok {
			return &ValidationError{
				Op:   x.Op,
				Msg:  "second argument must be an array literal",
				Node: x,
			}
		}
		if len(arr.Elements) == 0 {
			return &ValidationError{
				Op:   x.Op,
				Msg:  "list must contain at least one element",
				Node: x,
			}
		}
	case OpIsNull:
		if len(x.Args) != 1 {
			return arityErr(x, "requires exactly 1 argument")
		}
	case OpCaseI, OpAccentI:
		if len(x.Args) != 1 {
			return arityErr(x, "requires exactly 1 argument")
		}
	}
	return nil
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
	lower := make([]byte, len(s))
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c >= 'A' && c <= 'Z' {
			c += 'a' - 'A'
		}
		lower[i] = c
	}
	_, ok := reservedFunctionNames[string(lower)]
	return ok
}

// isValidFunctionName reports whether s matches the CQL2 identifier
// grammar [A-Za-z_][A-Za-z0-9_]*. Function names are emitted unquoted
// in the text encoding, so any deviation from this grammar produces
// output the parser would later reject.
func isValidFunctionName(s string) bool {
	if s == "" {
		return false
	}
	for i := 0; i < len(s); i++ {
		ch := s[i]
		if i == 0 {
			if !((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_') {
				return false
			}
			continue
		}
		if !((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') ||
			(ch >= '0' && ch <= '9') || ch == '_') {
			return false
		}
	}
	return true
}
