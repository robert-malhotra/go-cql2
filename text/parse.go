package text

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"

	cql2 "github.com/example/go-cql2"
	"github.com/example/go-cql2/wkt"
)

// Parse parses CQL2 Text input into an AST.
//
// TODO(wave-3): consume opts (positions, conformance, custom ops, text style).
func Parse(input string, opts ...cql2.Option) (cql2.Node, error) {
	_ = opts
	p := newParser(input)
	n, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	p.skipWS()
	if p.pos < len(p.src) {
		return nil, p.syntaxErrorAt(p.curPos(), "unexpected trailing input", p.peekRune())
	}
	return n, nil
}

// ParseBytes parses CQL2 Text from a byte slice.
//
// TODO(wave-3): consume opts (positions, conformance, custom ops, text style).
func ParseBytes(input []byte, opts ...cql2.Option) (cql2.Node, error) {
	return Parse(string(input), opts...)
}

// --- token kinds --------------------------------------------------------

type tokKind int

const (
	tokEOF tokKind = iota
	tokIdent
	tokQuotedIdent // "..." double-quoted identifier
	tokNumber
	tokString // single-quoted string
	tokLParen
	tokRParen
	tokComma
	tokPlus
	tokMinus
	tokStar
	tokSlash
	tokPercent
	tokCaret
	tokEq
	tokNeq
	tokLt
	tokLte
	tokGt
	tokGte
)

type token struct {
	kind tokKind
	text string // raw lexeme (for ident: original casing; for string: unquoted+unescaped; for number: source)
	pos  cql2.Pos
}

// --- parser -------------------------------------------------------------

type parser struct {
	src  string
	pos  int
	line int
	col  int

	// One-token lookahead.
	hasPeek bool
	peeked  token
}

func newParser(src string) *parser {
	return &parser{src: src, pos: 0, line: 1, col: 1}
}

func (p *parser) curPos() cql2.Pos {
	return cql2.Pos{Line: p.line, Column: p.col, Offset: p.pos}
}

func (p *parser) peekRune() string {
	if p.pos >= len(p.src) {
		return ""
	}
	return string(p.src[p.pos])
}

func (p *parser) advance(n int) {
	for i := 0; i < n && p.pos < len(p.src); i++ {
		ch := p.src[p.pos]
		p.pos++
		if ch == '\n' {
			p.line++
			p.col = 1
		} else {
			p.col++
		}
	}
}

func (p *parser) skipWS() {
	for p.pos < len(p.src) {
		ch := p.src[p.pos]
		if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' {
			p.advance(1)
			continue
		}
		break
	}
}

func (p *parser) snippet(at int) string {
	start := at - 8
	if start < 0 {
		start = 0
	}
	end := at + 16
	if end > len(p.src) {
		end = len(p.src)
	}
	return p.src[start:end]
}

func (p *parser) syntaxErrorAt(at cql2.Pos, msg, got string, expected ...string) error {
	return &cql2.SyntaxError{
		Encoding: cql2.EncodingText,
		At:       at,
		Snippet:  p.snippet(at.Offset),
		Msg:      msg,
		Got:      got,
		Expected: expected,
	}
}

// --- lexer --------------------------------------------------------------

func (p *parser) peekToken() (token, error) {
	if p.hasPeek {
		return p.peeked, nil
	}
	t, err := p.nextToken()
	if err != nil {
		return token{}, err
	}
	p.peeked = t
	p.hasPeek = true
	return t, nil
}

func (p *parser) consumeToken() (token, error) {
	if p.hasPeek {
		t := p.peeked
		p.hasPeek = false
		return t, nil
	}
	return p.nextToken()
}

// putBack stuffs t back into the lookahead slot. Only safe when slot empty.
func (p *parser) putBack(t token) {
	if p.hasPeek {
		panic("text: putBack on full lookahead")
	}
	p.peeked = t
	p.hasPeek = true
}

func (p *parser) nextToken() (token, error) {
	p.skipWS()
	pos := p.curPos()
	if p.pos >= len(p.src) {
		return token{kind: tokEOF, pos: pos}, nil
	}
	ch := p.src[p.pos]
	switch ch {
	case '(':
		p.advance(1)
		return token{kind: tokLParen, text: "(", pos: pos}, nil
	case ')':
		p.advance(1)
		return token{kind: tokRParen, text: ")", pos: pos}, nil
	case ',':
		p.advance(1)
		return token{kind: tokComma, text: ",", pos: pos}, nil
	case '+':
		p.advance(1)
		return token{kind: tokPlus, text: "+", pos: pos}, nil
	case '-':
		p.advance(1)
		return token{kind: tokMinus, text: "-", pos: pos}, nil
	case '*':
		p.advance(1)
		return token{kind: tokStar, text: "*", pos: pos}, nil
	case '/':
		p.advance(1)
		return token{kind: tokSlash, text: "/", pos: pos}, nil
	case '%':
		p.advance(1)
		return token{kind: tokPercent, text: "%", pos: pos}, nil
	case '^':
		p.advance(1)
		return token{kind: tokCaret, text: "^", pos: pos}, nil
	case '=':
		p.advance(1)
		return token{kind: tokEq, text: "=", pos: pos}, nil
	case '<':
		p.advance(1)
		if p.pos < len(p.src) {
			switch p.src[p.pos] {
			case '=':
				p.advance(1)
				return token{kind: tokLte, text: "<=", pos: pos}, nil
			case '>':
				p.advance(1)
				return token{kind: tokNeq, text: "<>", pos: pos}, nil
			}
		}
		return token{kind: tokLt, text: "<", pos: pos}, nil
	case '>':
		p.advance(1)
		if p.pos < len(p.src) && p.src[p.pos] == '=' {
			p.advance(1)
			return token{kind: tokGte, text: ">=", pos: pos}, nil
		}
		return token{kind: tokGt, text: ">", pos: pos}, nil
	case '\'':
		return p.lexString(pos)
	case '"':
		return p.lexQuotedIdent(pos)
	}
	if ch >= '0' && ch <= '9' {
		return p.lexNumber(pos)
	}
	if ch == '.' {
		// possibly a number starting with '.'
		if p.pos+1 < len(p.src) && p.src[p.pos+1] >= '0' && p.src[p.pos+1] <= '9' {
			return p.lexNumber(pos)
		}
	}
	if isIdentStart(ch) {
		return p.lexIdent(pos)
	}
	return token{}, p.syntaxErrorAt(pos, "unexpected character", string(ch))
}

func isIdentStart(ch byte) bool {
	return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_'
}

func isIdentCont(ch byte) bool {
	return isIdentStart(ch) || (ch >= '0' && ch <= '9') || ch == ':' || ch == '.'
}

func (p *parser) lexIdent(pos cql2.Pos) (token, error) {
	start := p.pos
	p.advance(1)
	for p.pos < len(p.src) && isIdentCont(p.src[p.pos]) {
		p.advance(1)
	}
	return token{kind: tokIdent, text: p.src[start:p.pos], pos: pos}, nil
}

func (p *parser) lexQuotedIdent(pos cql2.Pos) (token, error) {
	p.advance(1) // opening "
	var b strings.Builder
	for p.pos < len(p.src) {
		ch := p.src[p.pos]
		if ch == '"' {
			// possibly escaped ""
			if p.pos+1 < len(p.src) && p.src[p.pos+1] == '"' {
				b.WriteByte('"')
				p.advance(2)
				continue
			}
			p.advance(1)
			return token{kind: tokQuotedIdent, text: b.String(), pos: pos}, nil
		}
		b.WriteByte(ch)
		p.advance(1)
	}
	return token{}, p.syntaxErrorAt(pos, "unterminated quoted identifier", "")
}

func (p *parser) lexString(pos cql2.Pos) (token, error) {
	p.advance(1) // opening '
	var b strings.Builder
	for p.pos < len(p.src) {
		ch := p.src[p.pos]
		if ch == '\'' {
			if p.pos+1 < len(p.src) && p.src[p.pos+1] == '\'' {
				b.WriteByte('\'')
				p.advance(2)
				continue
			}
			p.advance(1)
			return token{kind: tokString, text: b.String(), pos: pos}, nil
		}
		b.WriteByte(ch)
		p.advance(1)
	}
	return token{}, p.syntaxErrorAt(pos, "unterminated string literal", "")
}

func (p *parser) lexNumber(pos cql2.Pos) (token, error) {
	start := p.pos
	// integer part
	for p.pos < len(p.src) && p.src[p.pos] >= '0' && p.src[p.pos] <= '9' {
		p.advance(1)
	}
	// fraction
	if p.pos < len(p.src) && p.src[p.pos] == '.' {
		p.advance(1)
		for p.pos < len(p.src) && p.src[p.pos] >= '0' && p.src[p.pos] <= '9' {
			p.advance(1)
		}
	}
	// exponent
	if p.pos < len(p.src) && (p.src[p.pos] == 'e' || p.src[p.pos] == 'E') {
		p.advance(1)
		if p.pos < len(p.src) && (p.src[p.pos] == '+' || p.src[p.pos] == '-') {
			p.advance(1)
		}
		expStart := p.pos
		for p.pos < len(p.src) && p.src[p.pos] >= '0' && p.src[p.pos] <= '9' {
			p.advance(1)
		}
		if p.pos == expStart {
			return token{}, p.syntaxErrorAt(pos, "malformed exponent", p.src[start:p.pos])
		}
	}
	return token{kind: tokNumber, text: p.src[start:p.pos], pos: pos}, nil
}

// --- token helpers ------------------------------------------------------

// keywordEqual reports whether the ident token matches the given keyword (case-insensitive).
func keywordEqual(t token, kw string) bool {
	if t.kind != tokIdent {
		return false
	}
	return strings.EqualFold(t.text, kw)
}

// operatorFunctionNames maps lowercase function-style operator names to the AST Operator constant.
var operatorFunctionNames = map[string]cql2.Operator{
	"casei":   cql2.OpCaseI,
	"accenti": cql2.OpAccentI,

	"s_intersects": cql2.OpSIntersects,
	"s_equals":     cql2.OpSEquals,
	"s_disjoint":   cql2.OpSDisjoint,
	"s_touches":    cql2.OpSTouches,
	"s_within":     cql2.OpSWithin,
	"s_overlaps":   cql2.OpSOverlaps,
	"s_crosses":    cql2.OpSCrosses,
	"s_contains":   cql2.OpSContains,

	"t_after":        cql2.OpTAfter,
	"t_before":       cql2.OpTBefore,
	"t_contains":     cql2.OpTContains,
	"t_disjoint":     cql2.OpTDisjoint,
	"t_during":       cql2.OpTDuring,
	"t_equals":       cql2.OpTEquals,
	"t_finishedby":   cql2.OpTFinishedBy,
	"t_finishes":     cql2.OpTFinishes,
	"t_intersects":   cql2.OpTIntersects,
	"t_meets":        cql2.OpTMeets,
	"t_metby":        cql2.OpTMetBy,
	"t_overlappedby": cql2.OpTOverlappedBy,
	"t_overlaps":     cql2.OpTOverlaps,
	"t_startedby":    cql2.OpTStartedBy,
	"t_starts":       cql2.OpTStarts,

	"a_contains":    cql2.OpAContains,
	"a_containedby": cql2.OpAContainedBy,
	"a_equals":      cql2.OpAEquals,
	"a_overlaps":    cql2.OpAOverlaps,
}

var geometryKeywords = map[string]bool{
	"point": true, "linestring": true, "polygon": true,
	"multipoint": true, "multilinestring": true, "multipolygon": true,
	"geometrycollection": true,
}

// --- recursive-descent grammar -----------------------------------------

// expression = orExpr
// orExpr = andExpr ( "OR" andExpr )*
// andExpr = notExpr ( "AND" notExpr )*
// notExpr = "NOT"? predicate
// predicate = additive predTail?
// predTail = compareTail | "BETWEEN" ... | "IN" ... | "IS" ["NOT"] "NULL" | "LIKE" ...
//            | "NOT" "BETWEEN" ... | "NOT" "IN" ... | "NOT" "LIKE" ...

func (p *parser) parseExpression() (cql2.Node, error) {
	return p.parseOr()
}

func (p *parser) parseOr() (cql2.Node, error) {
	left, err := p.parseAnd()
	if err != nil {
		return nil, err
	}
	for {
		t, err := p.peekToken()
		if err != nil {
			return nil, err
		}
		if !keywordEqual(t, "OR") {
			return left, nil
		}
		_, _ = p.consumeToken()
		right, err := p.parseAnd()
		if err != nil {
			return nil, err
		}
		// Flatten left-assoc into a single OR with multiple args.
		if op, ok := left.(*cql2.Op); ok && op.Op == cql2.OpOr {
			op.Args = append(op.Args, right)
		} else {
			left = &cql2.Op{Op: cql2.OpOr, Args: []cql2.Node{left, right}}
		}
	}
}

func (p *parser) parseAnd() (cql2.Node, error) {
	left, err := p.parseNot()
	if err != nil {
		return nil, err
	}
	for {
		t, err := p.peekToken()
		if err != nil {
			return nil, err
		}
		if !keywordEqual(t, "AND") {
			return left, nil
		}
		_, _ = p.consumeToken()
		right, err := p.parseNot()
		if err != nil {
			return nil, err
		}
		if op, ok := left.(*cql2.Op); ok && op.Op == cql2.OpAnd {
			op.Args = append(op.Args, right)
		} else {
			left = &cql2.Op{Op: cql2.OpAnd, Args: []cql2.Node{left, right}}
		}
	}
}

func (p *parser) parseNot() (cql2.Node, error) {
	t, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	if keywordEqual(t, "NOT") {
		_, _ = p.consumeToken()
		inner, err := p.parsePredicate()
		if err != nil {
			return nil, err
		}
		return &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{inner}}, nil
	}
	return p.parsePredicate()
}

func (p *parser) parsePredicate() (cql2.Node, error) {
	lhs, err := p.parseAdditive()
	if err != nil {
		return nil, err
	}
	t, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	switch {
	case t.kind == tokEq:
		_, _ = p.consumeToken()
		rhs, err := p.parseAdditive()
		if err != nil {
			return nil, err
		}
		return &cql2.Op{Op: cql2.OpEq, Args: []cql2.Node{lhs, rhs}}, nil
	case t.kind == tokNeq:
		_, _ = p.consumeToken()
		rhs, err := p.parseAdditive()
		if err != nil {
			return nil, err
		}
		return &cql2.Op{Op: cql2.OpNeq, Args: []cql2.Node{lhs, rhs}}, nil
	case t.kind == tokLt:
		_, _ = p.consumeToken()
		rhs, err := p.parseAdditive()
		if err != nil {
			return nil, err
		}
		return &cql2.Op{Op: cql2.OpLt, Args: []cql2.Node{lhs, rhs}}, nil
	case t.kind == tokLte:
		_, _ = p.consumeToken()
		rhs, err := p.parseAdditive()
		if err != nil {
			return nil, err
		}
		return &cql2.Op{Op: cql2.OpLte, Args: []cql2.Node{lhs, rhs}}, nil
	case t.kind == tokGt:
		_, _ = p.consumeToken()
		rhs, err := p.parseAdditive()
		if err != nil {
			return nil, err
		}
		return &cql2.Op{Op: cql2.OpGt, Args: []cql2.Node{lhs, rhs}}, nil
	case t.kind == tokGte:
		_, _ = p.consumeToken()
		rhs, err := p.parseAdditive()
		if err != nil {
			return nil, err
		}
		return &cql2.Op{Op: cql2.OpGte, Args: []cql2.Node{lhs, rhs}}, nil
	case keywordEqual(t, "BETWEEN"):
		return p.parseBetweenTail(lhs, false)
	case keywordEqual(t, "IN"):
		return p.parseInTail(lhs, false)
	case keywordEqual(t, "IS"):
		return p.parseIsNullTail(lhs)
	case keywordEqual(t, "LIKE"):
		return p.parseLikeTail(lhs, false)
	case keywordEqual(t, "NOT"):
		// Lookahead for NOT BETWEEN / NOT IN / NOT LIKE.
		notTok, _ := p.consumeToken()
		t2, err := p.peekToken()
		if err != nil {
			return nil, err
		}
		switch {
		case keywordEqual(t2, "BETWEEN"):
			return p.parseBetweenTail(lhs, true)
		case keywordEqual(t2, "IN"):
			return p.parseInTail(lhs, true)
		case keywordEqual(t2, "LIKE"):
			return p.parseLikeTail(lhs, true)
		default:
			// Not a valid trailing predicate; put NOT back so caller (e.g. AND) can possibly consume.
			// But the AND / OR logic only consumes their own keyword, not NOT.
			// In practice, "<expr> NOT <expr>" without a recognised continuation is a syntax error.
			p.putBack(notTok)
			return nil, p.syntaxErrorAt(t2.pos, "expected BETWEEN, IN, or LIKE after NOT", t2.text)
		}
	}
	return lhs, nil
}

func (p *parser) parseBetweenTail(lhs cql2.Node, negate bool) (cql2.Node, error) {
	t, err := p.consumeToken()
	if err != nil {
		return nil, err
	}
	if !keywordEqual(t, "BETWEEN") {
		return nil, p.syntaxErrorAt(t.pos, "expected BETWEEN", t.text)
	}
	lo, err := p.parseAdditive()
	if err != nil {
		return nil, err
	}
	andTok, err := p.consumeToken()
	if err != nil {
		return nil, err
	}
	if !keywordEqual(andTok, "AND") {
		return nil, p.syntaxErrorAt(andTok.pos, "expected AND in BETWEEN", andTok.text)
	}
	hi, err := p.parseAdditive()
	if err != nil {
		return nil, err
	}
	node := cql2.Node(&cql2.Op{Op: cql2.OpBetween, Args: []cql2.Node{lhs, lo, hi}})
	if negate {
		node = &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{node}}
	}
	return node, nil
}

func (p *parser) parseInTail(lhs cql2.Node, negate bool) (cql2.Node, error) {
	t, err := p.consumeToken()
	if err != nil {
		return nil, err
	}
	if !keywordEqual(t, "IN") {
		return nil, p.syntaxErrorAt(t.pos, "expected IN", t.text)
	}
	lp, err := p.consumeToken()
	if err != nil {
		return nil, err
	}
	if lp.kind != tokLParen {
		return nil, p.syntaxErrorAt(lp.pos, "expected '(' after IN", lp.text)
	}
	var elems []cql2.Node
	// Empty list is invalid; require at least one element.
	t2, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	if t2.kind == tokRParen {
		return nil, p.syntaxErrorAt(t2.pos, "IN list must contain at least one value", t2.text)
	}
	for {
		v, err := p.parseAdditive()
		if err != nil {
			return nil, err
		}
		elems = append(elems, v)
		nxt, err := p.peekToken()
		if err != nil {
			return nil, err
		}
		if nxt.kind == tokComma {
			_, _ = p.consumeToken()
			continue
		}
		break
	}
	rp, err := p.consumeToken()
	if err != nil {
		return nil, err
	}
	if rp.kind != tokRParen {
		return nil, p.syntaxErrorAt(rp.pos, "expected ')' to close IN list", rp.text)
	}
	node := cql2.Node(&cql2.Op{Op: cql2.OpIn, Args: []cql2.Node{lhs, &cql2.ArrayLit{Elements: elems}}})
	if negate {
		node = &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{node}}
	}
	return node, nil
}

func (p *parser) parseIsNullTail(lhs cql2.Node) (cql2.Node, error) {
	t, err := p.consumeToken()
	if err != nil {
		return nil, err
	}
	if !keywordEqual(t, "IS") {
		return nil, p.syntaxErrorAt(t.pos, "expected IS", t.text)
	}
	negate := false
	t2, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	if keywordEqual(t2, "NOT") {
		negate = true
		_, _ = p.consumeToken()
	}
	t3, err := p.consumeToken()
	if err != nil {
		return nil, err
	}
	if !keywordEqual(t3, "NULL") {
		return nil, p.syntaxErrorAt(t3.pos, "expected NULL", t3.text)
	}
	node := cql2.Node(&cql2.Op{Op: cql2.OpIsNull, Args: []cql2.Node{lhs}})
	if negate {
		node = &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{node}}
	}
	return node, nil
}

func (p *parser) parseLikeTail(lhs cql2.Node, negate bool) (cql2.Node, error) {
	t, err := p.consumeToken()
	if err != nil {
		return nil, err
	}
	if !keywordEqual(t, "LIKE") {
		return nil, p.syntaxErrorAt(t.pos, "expected LIKE", t.text)
	}
	rhs, err := p.parseAdditive()
	if err != nil {
		return nil, err
	}
	node := cql2.Node(&cql2.Op{Op: cql2.OpLike, Args: []cql2.Node{lhs, rhs}})
	if negate {
		node = &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{node}}
	}
	return node, nil
}

// Arithmetic precedence: + - (additive) < * / % div (multiplicative) < ^ (power, right-assoc) < unary - < primary.
func (p *parser) parseAdditive() (cql2.Node, error) {
	left, err := p.parseMultiplicative()
	if err != nil {
		return nil, err
	}
	for {
		t, err := p.peekToken()
		if err != nil {
			return nil, err
		}
		var op cql2.Operator
		switch t.kind {
		case tokPlus:
			op = cql2.OpAdd
		case tokMinus:
			op = cql2.OpSub
		default:
			return left, nil
		}
		_, _ = p.consumeToken()
		right, err := p.parseMultiplicative()
		if err != nil {
			return nil, err
		}
		left = &cql2.Op{Op: op, Args: []cql2.Node{left, right}}
	}
}

func (p *parser) parseMultiplicative() (cql2.Node, error) {
	left, err := p.parsePower()
	if err != nil {
		return nil, err
	}
	for {
		t, err := p.peekToken()
		if err != nil {
			return nil, err
		}
		var op cql2.Operator
		switch {
		case t.kind == tokStar:
			op = cql2.OpMul
		case t.kind == tokSlash:
			op = cql2.OpDiv
		case t.kind == tokPercent:
			op = cql2.OpMod
		case keywordEqual(t, "div"):
			op = cql2.OpIDiv
		default:
			return left, nil
		}
		_, _ = p.consumeToken()
		right, err := p.parsePower()
		if err != nil {
			return nil, err
		}
		left = &cql2.Op{Op: op, Args: []cql2.Node{left, right}}
	}
}

func (p *parser) parsePower() (cql2.Node, error) {
	left, err := p.parseUnary()
	if err != nil {
		return nil, err
	}
	t, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	if t.kind != tokCaret {
		return left, nil
	}
	_, _ = p.consumeToken()
	// Right-assoc.
	right, err := p.parsePower()
	if err != nil {
		return nil, err
	}
	return &cql2.Op{Op: cql2.OpPow, Args: []cql2.Node{left, right}}, nil
}

func (p *parser) parseUnary() (cql2.Node, error) {
	t, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	if t.kind == tokMinus {
		_, _ = p.consumeToken()
		inner, err := p.parseUnary()
		if err != nil {
			return nil, err
		}
		// Represent unary minus as 0 - inner using a NumLit zero for a stable AST.
		// Alternative: a dedicated "neg" op. The spec doesn't mandate either.
		// Use OpSub with a zero literal so the AST stays in the existing operator set.
		zero := &cql2.NumLit{Value: json.Number("0")}
		return &cql2.Op{Op: cql2.OpSub, Args: []cql2.Node{zero, inner}}, nil
	}
	if t.kind == tokPlus {
		_, _ = p.consumeToken()
		return p.parseUnary()
	}
	return p.parsePrimary()
}

func (p *parser) parsePrimary() (cql2.Node, error) {
	t, err := p.consumeToken()
	if err != nil {
		return nil, err
	}
	switch t.kind {
	case tokLParen:
		// Parenthesised expression.
		inner, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		rp, err := p.consumeToken()
		if err != nil {
			return nil, err
		}
		if rp.kind != tokRParen {
			return nil, p.syntaxErrorAt(rp.pos, "expected ')' to close expression", rp.text)
		}
		return inner, nil
	case tokNumber:
		return &cql2.NumLit{Value: json.Number(t.text)}, nil
	case tokString:
		return &cql2.StringLit{Value: t.text}, nil
	case tokQuotedIdent:
		return &cql2.PropertyRef{Name: t.text}, nil
	case tokIdent:
		return p.parseIdentPrimary(t)
	}
	return nil, p.syntaxErrorAt(t.pos, "unexpected token", t.text)
}

// parseIdentPrimary handles identifiers, which can be:
// - reserved literals: TRUE, FALSE, NULL
// - typed-literal constructors: TIMESTAMP('...'), DATE('...'), INTERVAL(...)
// - geometry-type keyword followed by WKT expression
// - BBOX(...)
// - operator-keyword-style function call (S_INTERSECTS, T_AFTER, CASEI, ...)
// - user-defined function call: name(args...)
// - bare property reference
func (p *parser) parseIdentPrimary(t token) (cql2.Node, error) {
	lower := strings.ToLower(t.text)

	// Boolean / null literals.
	switch lower {
	case "true":
		return &cql2.BoolLit{Value: true}, nil
	case "false":
		return &cql2.BoolLit{Value: false}, nil
	case "null":
		return &cql2.NullLit{}, nil
	}

	// Lookahead: '(' means a function-style construct; otherwise this is a property reference.
	la, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	if la.kind != tokLParen {
		// Bare property reference.
		return &cql2.PropertyRef{Name: t.text}, nil
	}

	// Typed-literal constructors.
	switch lower {
	case "timestamp":
		return p.parseTimestampConstructor(t.pos)
	case "date":
		return p.parseDateConstructor(t.pos)
	case "interval":
		return p.parseIntervalConstructor(t.pos)
	case "bbox":
		return p.parseBBoxConstructor(t.pos)
	}

	// Geometry literals: capture the WKT span and delegate to wkt.Parse.
	if geometryKeywords[lower] {
		return p.parseGeometryLiteral(t)
	}

	// Otherwise: function-call (which may desugar into an Op).
	return p.parseFunctionCall(t)
}

func (p *parser) parseTimestampConstructor(at cql2.Pos) (cql2.Node, error) {
	if err := p.expectKind(tokLParen, "("); err != nil {
		return nil, err
	}
	s, err := p.expectStringLiteral()
	if err != nil {
		return nil, err
	}
	if err := p.expectKind(tokRParen, ")"); err != nil {
		return nil, err
	}
	tm, err := time.Parse(time.RFC3339Nano, s)
	if err != nil {
		return nil, p.syntaxErrorAt(at, fmt.Sprintf("malformed TIMESTAMP literal: %v", err), s)
	}
	return &cql2.TimestampLit{Value: tm}, nil
}

func (p *parser) parseDateConstructor(at cql2.Pos) (cql2.Node, error) {
	if err := p.expectKind(tokLParen, "("); err != nil {
		return nil, err
	}
	s, err := p.expectStringLiteral()
	if err != nil {
		return nil, err
	}
	if err := p.expectKind(tokRParen, ")"); err != nil {
		return nil, err
	}
	tm, err := time.ParseInLocation("2006-01-02", s, time.UTC)
	if err != nil {
		return nil, p.syntaxErrorAt(at, fmt.Sprintf("malformed DATE literal: %v", err), s)
	}
	return &cql2.DateLit{Value: tm}, nil
}

func (p *parser) parseIntervalConstructor(at cql2.Pos) (cql2.Node, error) {
	if err := p.expectKind(tokLParen, "("); err != nil {
		return nil, err
	}
	startS, err := p.expectStringLiteral()
	if err != nil {
		return nil, err
	}
	if err := p.expectKind(tokComma, ","); err != nil {
		return nil, err
	}
	endS, err := p.expectStringLiteral()
	if err != nil {
		return nil, err
	}
	if err := p.expectKind(tokRParen, ")"); err != nil {
		return nil, err
	}
	startEP, err := parseIntervalEndpoint(startS)
	if err != nil {
		return nil, p.syntaxErrorAt(at, fmt.Sprintf("malformed INTERVAL start: %v", err), startS)
	}
	endEP, err := parseIntervalEndpoint(endS)
	if err != nil {
		return nil, p.syntaxErrorAt(at, fmt.Sprintf("malformed INTERVAL end: %v", err), endS)
	}
	return &cql2.IntervalLit{Start: startEP, End: endEP}, nil
}

func parseIntervalEndpoint(s string) (cql2.IntervalEndpoint, error) {
	if s == ".." {
		return &cql2.Unbounded{}, nil
	}
	if tm, err := time.Parse(time.RFC3339Nano, s); err == nil {
		return &cql2.TimestampLit{Value: tm}, nil
	}
	if tm, err := time.ParseInLocation("2006-01-02", s, time.UTC); err == nil {
		return &cql2.DateLit{Value: tm}, nil
	}
	return nil, fmt.Errorf("not a recognised RFC3339 timestamp or date: %q", s)
}

func (p *parser) parseBBoxConstructor(at cql2.Pos) (cql2.Node, error) {
	_ = at
	if err := p.expectKind(tokLParen, "("); err != nil {
		return nil, err
	}
	var coords []float64
	for {
		// Allow leading +/- and consume an additive expression that must reduce to a numeric literal.
		neg := false
		t, err := p.peekToken()
		if err != nil {
			return nil, err
		}
		if t.kind == tokMinus {
			neg = true
			_, _ = p.consumeToken()
		} else if t.kind == tokPlus {
			_, _ = p.consumeToken()
		}
		num, err := p.consumeToken()
		if err != nil {
			return nil, err
		}
		if num.kind != tokNumber {
			return nil, p.syntaxErrorAt(num.pos, "expected number in BBOX", num.text)
		}
		s := num.text
		if neg {
			s = "-" + s
		}
		var f float64
		if _, err := fmt.Sscanf(s, "%g", &f); err != nil {
			return nil, p.syntaxErrorAt(num.pos, fmt.Sprintf("invalid number in BBOX: %v", err), s)
		}
		coords = append(coords, f)
		nxt, err := p.peekToken()
		if err != nil {
			return nil, err
		}
		if nxt.kind == tokComma {
			_, _ = p.consumeToken()
			continue
		}
		break
	}
	if err := p.expectKind(tokRParen, ")"); err != nil {
		return nil, err
	}
	if len(coords) != 4 && len(coords) != 6 {
		return nil, p.syntaxErrorAt(p.curPos(), fmt.Sprintf("BBOX requires 4 or 6 coordinates, got %d", len(coords)), "")
	}
	return &cql2.BBoxLit{Coords: coords}, nil
}

// parseGeometryLiteral captures the source span starting at the keyword token and
// running through the matched closing paren, then hands it to wkt.Parse.
//
// At entry, t is the geometry-type keyword that was already consumed via the
// token lexer. The next token in the lookahead slot is expected to be '(' (or
// the bare identifier "EMPTY" for empty geometries).
func (p *parser) parseGeometryLiteral(t token) (cql2.Node, error) {
	startOff := t.pos.Offset
	la, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	// POINT EMPTY style: reconstruct the span as "<KEYWORD> EMPTY" and hand to wkt.
	if keywordEqual(la, "EMPTY") {
		_, _ = p.consumeToken()
		endOff := p.pos
		g, err := wkt.Parse(p.src[startOff:endOff])
		if err != nil {
			return nil, p.syntaxErrorAt(t.pos, fmt.Sprintf("invalid geometry literal: %v", err), p.src[startOff:endOff])
		}
		return &cql2.GeomLit{Geom: g}, nil
	}
	if la.kind != tokLParen {
		return nil, p.syntaxErrorAt(la.pos, "expected '(' after geometry-type keyword", la.text)
	}
	// The opening '(' is in the peek slot. Use its offset as the scan start
	// so we don't depend on whether the lexer has advanced p.pos past it.
	openOff := la.pos.Offset
	depth := 0
	i := openOff
	for i < len(p.src) {
		ch := p.src[i]
		if ch == '(' {
			depth++
		} else if ch == ')' {
			depth--
			if depth == 0 {
				i++
				break
			}
		}
		i++
	}
	if depth != 0 {
		return nil, p.syntaxErrorAt(t.pos, "unbalanced parens in geometry literal", "")
	}
	span := p.src[startOff:i]
	// Drop the cached lookahead and rewind the lexer cursor to the open paren,
	// then advance to the end of the matched span.
	p.hasPeek = false
	// Roll p.pos / line / col forward from current position to i.
	// Since we may currently be past openOff, set pos directly and re-derive line/col.
	p.pos = i
	p.line, p.col = lineColAt(p.src, i)
	g, err := wkt.Parse(span)
	if err != nil {
		return nil, p.syntaxErrorAt(t.pos, fmt.Sprintf("invalid geometry literal: %v", err), span)
	}
	return &cql2.GeomLit{Geom: g}, nil
}

// lineColAt computes 1-based line/column for the given byte offset in src.
func lineColAt(src string, off int) (int, int) {
	line, col := 1, 1
	if off > len(src) {
		off = len(src)
	}
	for i := 0; i < off; i++ {
		if src[i] == '\n' {
			line++
			col = 1
		} else {
			col++
		}
	}
	return line, col
}

// parseFunctionCall parses argument list after the consumed name token.
func (p *parser) parseFunctionCall(name token) (cql2.Node, error) {
	if err := p.expectKind(tokLParen, "("); err != nil {
		return nil, err
	}
	var args []cql2.Node
	t, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	if t.kind != tokRParen {
		for {
			a, err := p.parseExpression()
			if err != nil {
				return nil, err
			}
			args = append(args, a)
			nxt, err := p.peekToken()
			if err != nil {
				return nil, err
			}
			if nxt.kind == tokComma {
				_, _ = p.consumeToken()
				continue
			}
			break
		}
	}
	if err := p.expectKind(tokRParen, ")"); err != nil {
		return nil, err
	}
	// Desugar known operator-keyword functions.
	if op, ok := operatorFunctionNames[strings.ToLower(name.text)]; ok {
		return &cql2.Op{Op: op, Args: args}, nil
	}
	return &cql2.FunctionCall{Name: name.text, Args: args}, nil
}

func (p *parser) expectKind(k tokKind, want string) error {
	t, err := p.consumeToken()
	if err != nil {
		return err
	}
	if t.kind != k {
		return p.syntaxErrorAt(t.pos, fmt.Sprintf("expected %q", want), t.text)
	}
	return nil
}

func (p *parser) expectStringLiteral() (string, error) {
	t, err := p.consumeToken()
	if err != nil {
		return "", err
	}
	if t.kind != tokString {
		return "", p.syntaxErrorAt(t.pos, "expected single-quoted string literal", t.text)
	}
	return t.text, nil
}

