package text

import (
	"fmt"
	"strconv"
	"strings"

	cql2 "github.com/exergy-dev/go-cql2"
	"github.com/exergy-dev/go-topology-suite/wkt"
)

// Encode emits an AST as a CQL2 Text string.
//
// Recognized options: WithTextStyle (StyleNormal, StyleVerbose).
func Encode(n cql2.Node, opts ...cql2.Option) (string, error) {
	cfg := cql2.ResolveOptions(opts...)
	if n == nil {
		return "", fmt.Errorf("text: cannot encode nil node")
	}
	var b strings.Builder
	enc := &encoder{cfg: cfg}
	if err := enc.writeNode(&b, n, precTop); err != nil {
		return "", err
	}
	return b.String(), nil
}

// encoder bundles per-call state for text encoding so that style settings
// reach inner helpers without threading a parameter through.
type encoder struct {
	cfg *cql2.Config
}

// Precedence levels (higher binds tighter). Used to decide parenthesisation.
const (
	precTop      = 0
	precOr       = 1
	precAnd      = 2
	precNot      = 3
	precCompare  = 4 // = <> < <= > >= LIKE BETWEEN IN IS NULL
	precAdditive = 5
	precMul      = 6
	precPower    = 7
	precUnary    = 8
	precAtom     = 9
)

func opPrec(op cql2.Operator) int {
	switch op {
	case cql2.OpOr:
		return precOr
	case cql2.OpAnd:
		return precAnd
	case cql2.OpNot:
		return precNot
	case cql2.OpEq, cql2.OpNeq, cql2.OpLt, cql2.OpLte, cql2.OpGt, cql2.OpGte,
		cql2.OpLike, cql2.OpBetween, cql2.OpIn, cql2.OpIsNull:
		return precCompare
	case cql2.OpAdd, cql2.OpSub:
		return precAdditive
	case cql2.OpMul, cql2.OpDiv, cql2.OpMod, cql2.OpIDiv:
		return precMul
	case cql2.OpPow:
		return precPower
	}
	// Spatial / temporal / array / casei / accenti are emitted as function calls,
	// which behave as atoms.
	return precAtom
}

func (e *encoder) writeNode(b *strings.Builder, n cql2.Node, parentPrec int) error {
	switch x := n.(type) {
	case *cql2.BoolLit:
		if x.Value {
			b.WriteString("TRUE")
		} else {
			b.WriteString("FALSE")
		}
		return nil
	case *cql2.NumLit:
		b.WriteString(string(x.Value))
		return nil
	case *cql2.StringLit:
		writeStringLit(b, x.Value)
		return nil
	case *cql2.NullLit:
		b.WriteString("NULL")
		return nil
	case *cql2.TimestampLit:
		b.WriteString("TIMESTAMP('")
		b.WriteString(cql2.FormatTimestamp(x.Value))
		b.WriteString("')")
		return nil
	case *cql2.DateLit:
		b.WriteString("DATE('")
		b.WriteString(x.Value.UTC().Format("2006-01-02"))
		b.WriteString("')")
		return nil
	case *cql2.IntervalLit:
		b.WriteString("INTERVAL(")
		if err := e.writeIntervalEndpoint(b, x.Start); err != nil {
			return err
		}
		b.WriteByte(',')
		if err := e.writeIntervalEndpoint(b, x.End); err != nil {
			return err
		}
		b.WriteByte(')')
		return nil
	case *cql2.GeomLit:
		s, err := wkt.Marshal(x.Geom)
		if err != nil {
			return err
		}
		b.WriteString(s)
		return nil
	case *cql2.BBoxLit:
		b.WriteString("BBOX(")
		for i, c := range x.Coords {
			if i > 0 {
				b.WriteByte(',')
			}
			b.WriteString(formatFloat(c))
		}
		b.WriteByte(')')
		return nil
	case *cql2.ArrayLit:
		b.WriteByte('(')
		for i, el := range x.Elements {
			if i > 0 {
				b.WriteString(", ")
			}
			if err := e.writeNode(b, el, precTop); err != nil {
				return err
			}
		}
		b.WriteByte(')')
		return nil
	case *cql2.PropertyRef:
		writePropertyRef(b, x.Name)
		return nil
	case *cql2.FunctionCall:
		b.WriteString(x.Name)
		b.WriteByte('(')
		for i, a := range x.Args {
			if i > 0 {
				b.WriteString(", ")
			}
			if err := e.writeNode(b, a, precTop); err != nil {
				return err
			}
		}
		b.WriteByte(')')
		return nil
	case *cql2.Op:
		return e.writeOp(b, x, parentPrec)
	}
	return fmt.Errorf("text: cannot encode node of type %T", n)
}

func (e *encoder) writeOp(b *strings.Builder, x *cql2.Op, parentPrec int) error {
	myPrec := opPrec(x.Op)
	wrap := myPrec < parentPrec && myPrec >= precOr
	// Comparison operators are non-associative in CQL2 text (`a < b < c`
	// is not valid), so a comparison directly inside another comparison
	// must be parenthesized even at equal precedence.
	if !wrap && myPrec == parentPrec && myPrec == precCompare {
		wrap = true
	}
	// StyleVerbose: always wrap any binary/logical operator in parens, even
	// when not strictly required for precedence. Function-style ops (spatial,
	// temporal, array, casei, accenti) emit as atoms and aren't wrapped.
	if e != nil && e.cfg != nil && e.cfg.TextStyle == cql2.StyleVerbose {
		if myPrec >= precOr && myPrec <= precPower {
			wrap = true
		}
	}
	if wrap {
		b.WriteByte('(')
	}
	if err := e.writeOpInner(b, x); err != nil {
		return err
	}
	if wrap {
		b.WriteByte(')')
	}
	return nil
}

func (e *encoder) writeOpInner(b *strings.Builder, x *cql2.Op) error {
	switch x.Op {
	case cql2.OpAnd, cql2.OpOr:
		sep := " AND "
		if x.Op == cql2.OpOr {
			sep = " OR "
		}
		myPrec := opPrec(x.Op)
		for i, a := range x.Args {
			if i > 0 {
				b.WriteString(sep)
			}
			if err := e.writeNode(b, a, myPrec+1); err != nil {
				return err
			}
		}
		return nil
	case cql2.OpNot:
		// Special un-desugaring cases for nice text.
		if len(x.Args) == 1 {
			if inner, ok := x.Args[0].(*cql2.Op); ok {
				switch inner.Op {
				case cql2.OpIsNull:
					if len(inner.Args) == 1 {
						if err := e.writeNode(b, inner.Args[0], precCompare); err != nil {
							return err
						}
						b.WriteString(" IS NOT NULL")
						return nil
					}
				case cql2.OpBetween:
					if len(inner.Args) == 3 {
						return e.writeBetween(b, inner.Args[0], inner.Args[1], inner.Args[2], true)
					}
				case cql2.OpIn:
					if len(inner.Args) == 2 {
						return e.writeIn(b, inner.Args[0], inner.Args[1], true)
					}
				case cql2.OpLike:
					if len(inner.Args) == 2 {
						return e.writeLike(b, inner.Args[0], inner.Args[1], true)
					}
				}
			}
		}
		b.WriteString("NOT ")
		if len(x.Args) == 1 {
			return e.writeNode(b, x.Args[0], precNot)
		}
		return fmt.Errorf("text: NOT requires exactly one argument")
	case cql2.OpEq, cql2.OpNeq, cql2.OpLt, cql2.OpLte, cql2.OpGt, cql2.OpGte,
		cql2.OpAdd, cql2.OpSub, cql2.OpMul, cql2.OpDiv, cql2.OpMod, cql2.OpPow:
		if len(x.Args) != 2 {
			return fmt.Errorf("text: operator %q requires 2 arguments, got %d", x.Op, len(x.Args))
		}
		myPrec := opPrec(x.Op)
		// Left-assoc for everything except power; for power we right-assoc.
		leftPrec := myPrec
		rightPrec := myPrec + 1
		if x.Op == cql2.OpPow {
			leftPrec = myPrec + 1
			rightPrec = myPrec
		}
		if err := e.writeNode(b, x.Args[0], leftPrec); err != nil {
			return err
		}
		b.WriteString(" ")
		b.WriteString(string(x.Op))
		b.WriteString(" ")
		return e.writeNode(b, x.Args[1], rightPrec)
	case cql2.OpIDiv:
		if len(x.Args) != 2 {
			return fmt.Errorf("text: div requires 2 arguments")
		}
		myPrec := opPrec(x.Op)
		if err := e.writeNode(b, x.Args[0], myPrec); err != nil {
			return err
		}
		b.WriteString(" div ")
		return e.writeNode(b, x.Args[1], myPrec+1)
	case cql2.OpBetween:
		if len(x.Args) != 3 {
			return fmt.Errorf("text: BETWEEN requires 3 arguments")
		}
		return e.writeBetween(b, x.Args[0], x.Args[1], x.Args[2], false)
	case cql2.OpIn:
		if len(x.Args) != 2 {
			return fmt.Errorf("text: IN requires 2 arguments")
		}
		return e.writeIn(b, x.Args[0], x.Args[1], false)
	case cql2.OpIsNull:
		if len(x.Args) != 1 {
			return fmt.Errorf("text: IS NULL requires 1 argument")
		}
		if err := e.writeNode(b, x.Args[0], precCompare); err != nil {
			return err
		}
		b.WriteString(" IS NULL")
		return nil
	case cql2.OpLike:
		if len(x.Args) != 2 {
			return fmt.Errorf("text: LIKE requires 2 arguments")
		}
		return e.writeLike(b, x.Args[0], x.Args[1], false)
	}
	// Default for spatial / temporal / array / casei / accenti operators:
	// function-call form (e.g. S_INTERSECTS(...), CASEI(...)).
	return e.writeOpAsFunction(b, x)
}

func (e *encoder) writeOpAsFunction(b *strings.Builder, x *cql2.Op) error {
	b.WriteString(strings.ToUpper(string(x.Op)))
	b.WriteByte('(')
	for i, a := range x.Args {
		if i > 0 {
			b.WriteString(", ")
		}
		if err := e.writeNode(b, a, precTop); err != nil {
			return err
		}
	}
	b.WriteByte(')')
	return nil
}

func (e *encoder) writeBetween(b *strings.Builder, lhs, lo, hi cql2.Node, negate bool) error {
	if err := e.writeNode(b, lhs, precCompare); err != nil {
		return err
	}
	if negate {
		b.WriteString(" NOT BETWEEN ")
	} else {
		b.WriteString(" BETWEEN ")
	}
	if err := e.writeNode(b, lo, precCompare+1); err != nil {
		return err
	}
	b.WriteString(" AND ")
	return e.writeNode(b, hi, precCompare+1)
}

func (e *encoder) writeIn(b *strings.Builder, lhs, list cql2.Node, negate bool) error {
	if err := e.writeNode(b, lhs, precCompare); err != nil {
		return err
	}
	if negate {
		b.WriteString(" NOT IN ")
	} else {
		b.WriteString(" IN ")
	}
	arr, ok := list.(*cql2.ArrayLit)
	if !ok {
		// Fallback for tolerance: emit whatever the RHS is in parens.
		b.WriteByte('(')
		if err := e.writeNode(b, list, precTop); err != nil {
			return err
		}
		b.WriteByte(')')
		return nil
	}
	b.WriteByte('(')
	for i, el := range arr.Elements {
		if i > 0 {
			b.WriteString(", ")
		}
		if err := e.writeNode(b, el, precTop); err != nil {
			return err
		}
	}
	b.WriteByte(')')
	return nil
}

func (e *encoder) writeLike(b *strings.Builder, lhs, pat cql2.Node, negate bool) error {
	if err := e.writeNode(b, lhs, precCompare); err != nil {
		return err
	}
	if negate {
		b.WriteString(" NOT LIKE ")
	} else {
		b.WriteString(" LIKE ")
	}
	return e.writeNode(b, pat, precCompare+1)
}

// --- formatting helpers ------------------------------------------------

func writeStringLit(b *strings.Builder, s string) {
	b.WriteByte('\'')
	for i := 0; i < len(s); i++ {
		ch := s[i]
		switch ch {
		case '\'':
			b.WriteString("''")
		case '\\':
			b.WriteString(`\\`)
		case '\a':
			b.WriteString(`\a`)
		case '\b':
			b.WriteString(`\b`)
		case '\t':
			b.WriteString(`\t`)
		case '\n':
			b.WriteString(`\n`)
		case '\v':
			b.WriteString(`\v`)
		case '\f':
			b.WriteString(`\f`)
		case '\r':
			b.WriteString(`\r`)
		default:
			b.WriteByte(ch)
		}
	}
	b.WriteByte('\'')
}

// isBareIdent reports whether name can be emitted as a bare identifier
// (per Annex B's propertyName grammar). Mirrors text/parse.go's
// isIdentStartRune / isIdentContRune.
func isBareIdent(name string) bool {
	if name == "" {
		return false
	}
	first := true
	for _, r := range name {
		if first {
			if !isIdentStartRune(r) {
				return false
			}
			first = false
			continue
		}
		if !isIdentContRune(r) {
			return false
		}
	}
	// A bare identifier may not collide with a reserved keyword; the parser
	// would re-interpret it. reservedKeywords (parse.go) is the shared list.
	return !reservedKeywords[strings.ToLower(name)]
}

func writePropertyRef(b *strings.Builder, name string) {
	if isBareIdent(name) {
		b.WriteString(name)
		return
	}
	b.WriteByte('"')
	for i := 0; i < len(name); i++ {
		ch := name[i]
		if ch == '"' {
			b.WriteByte('"')
			b.WriteByte('"')
			continue
		}
		b.WriteByte(ch)
	}
	b.WriteByte('"')
}

func formatFloat(f float64) string {
	// Use the shortest round-tripping representation.
	return strconv.FormatFloat(f, 'g', -1, 64)
}

func formatEndpoint(ep cql2.Node) string {
	switch v := ep.(type) {
	case *cql2.Unbounded, nil:
		return ".."
	case *cql2.TimestampLit:
		return cql2.FormatTimestamp(v.Value)
	case *cql2.DateLit:
		return v.Value.UTC().Format("2006-01-02")
	}
	return ".."
}

// writeIntervalEndpoint emits an INTERVAL endpoint. String forms (timestamp,
// date, unbounded) are wrapped in single quotes; property references are
// emitted unquoted using the standard property-ref formatting; function
// calls are recursively encoded.
func (e *encoder) writeIntervalEndpoint(b *strings.Builder, ep cql2.Node) error {
	switch v := ep.(type) {
	case *cql2.PropertyRef:
		writePropertyRef(b, v.Name)
		return nil
	case *cql2.FunctionCall:
		return e.writeNode(b, v, precTop)
	}
	b.WriteByte('\'')
	b.WriteString(formatEndpoint(ep))
	b.WriteByte('\'')
	return nil
}
