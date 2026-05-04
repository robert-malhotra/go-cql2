package text

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"

	cql2 "github.com/exergy-dev/go-cql2"
	"github.com/exergy-dev/go-cql2/wkt"
)

// Parse parses CQL2 Text input into an AST.
//
// Recognized options: WithConformance, WithPositions, WithCustomOperators,
// WithDateTimezone.
func Parse(input string, opts ...cql2.Option) (cql2.Node, error) {
	cfg := cql2.ResolveOptions(opts...)
	p := newParser(input)
	p.cfg = cfg
	n, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	// Trailing input may live in the lookahead slot (already lexed) or in
	// the un-lexed source remainder. Check both so we don't accept a stray
	// trailing identifier just because the lexer happened to swallow it.
	if p.hasPeek && p.peeked.kind != tokEOF {
		t := p.peeked
		return nil, p.syntaxErrorAt(t.pos, "unexpected trailing input", t.text)
	}
	p.skipWS()
	if p.pos < len(p.src) {
		return nil, p.syntaxErrorAt(p.curPos(), "unexpected trailing input", p.peekRune())
	}
	if err := checkConformance(n, cfg); err != nil {
		return nil, err
	}
	if err := cql2.Validate(n); err != nil {
		return nil, err
	}
	return n, nil
}

// ParseBytes parses CQL2 Text from a byte slice.
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

	cfg *cql2.Config

	// One-token lookahead.
	hasPeek bool
	peeked  token

	// depth is the current expression nesting depth, bumped on entry to
	// parseExpression and decremented on exit; checked against cfg.MaxDepth.
	depth int
}

// enterDepth bumps the recursion counter and rejects inputs that exceed
// cfg.MaxDepth. Must be paired with a deferred call to leaveDepth.
func (p *parser) enterDepth(at cql2.Pos) error {
	p.depth++
	if p.cfg != nil && p.cfg.MaxDepth >= 0 {
		max := p.cfg.MaxDepth
		if max == 0 {
			max = cql2.DefaultMaxDepth
		}
		if p.depth > max {
			return p.syntaxErrorAt(at, fmt.Sprintf("expression nesting exceeds limit (max %d)", max), "")
		}
	}
	return nil
}

func (p *parser) leaveDepth() { p.depth-- }

// recordPos sets a position on the parser's PositionMap if active.
func (p *parser) recordPos(n cql2.Node, pos cql2.Pos) {
	if p == nil || p.cfg == nil || p.cfg.Positions == nil || n == nil {
		return
	}
	p.cfg.Positions.Set(n, pos)
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
	if ch < utf8.RuneSelf {
		if isIdentStartByte(ch) {
			return p.lexIdent(pos)
		}
		return token{}, p.syntaxErrorAt(pos, "unexpected character", string(ch))
	}
	// Non-ASCII: decode rune and check the Unicode identifier rules from
	// Annex B's propertyName grammar.
	r, _ := utf8.DecodeRuneInString(p.src[p.pos:])
	if isIdentStartRune(r) {
		return p.lexIdent(pos)
	}
	return token{}, p.syntaxErrorAt(pos, "unexpected character", string(r))
}

// isIdentStartByte is the ASCII fast-path for identifier-start chars
// permitted by Annex B (colon, underscore, A-Z, a-z).
func isIdentStartByte(ch byte) bool {
	return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_' || ch == ':'
}

// isIdentStartRune covers the full Annex B identifierStart range: the ASCII
// fast-path plus any Unicode letter (covers Latin-1 supplement, Greek,
// Cyrillic, CJK, and the rest of the spec's enumerated blocks).
func isIdentStartRune(r rune) bool {
	if r < utf8.RuneSelf {
		return isIdentStartByte(byte(r))
	}
	return unicode.IsLetter(r)
}

// isIdentContRune covers identifier continuation: all start chars, plus
// '.', digits, and combining/diacritical marks.
func isIdentContRune(r rune) bool {
	if r < utf8.RuneSelf {
		ch := byte(r)
		return isIdentStartByte(ch) || (ch >= '0' && ch <= '9') || ch == '.'
	}
	return unicode.IsLetter(r) || unicode.IsDigit(r) || unicode.IsMark(r)
}

func (p *parser) lexIdent(pos cql2.Pos) (token, error) {
	start := p.pos
	// First rune is already known to satisfy isIdentStartRune.
	_, sz := utf8.DecodeRuneInString(p.src[p.pos:])
	p.advance(sz)
	for p.pos < len(p.src) {
		r, rsz := utf8.DecodeRuneInString(p.src[p.pos:])
		if !isIdentContRune(r) {
			break
		}
		p.advance(rsz)
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
		// /req/cql2-text/escaping: backslash escape for embedded quote and
		// the seven C-style control characters.
		if ch == '\\' && p.pos+1 < len(p.src) {
			esc := p.src[p.pos+1]
			var out byte
			switch esc {
			case '\'':
				out = '\''
			case '\\':
				out = '\\'
			case 'a':
				out = '\a'
			case 'b':
				out = '\b'
			case 't':
				out = '\t'
			case 'n':
				out = '\n'
			case 'v':
				out = '\v'
			case 'f':
				out = '\f'
			case 'r':
				out = '\r'
			default:
				return token{}, p.syntaxErrorAt(pos,
					fmt.Sprintf("unknown escape sequence \\%c in string literal", esc), "")
			}
			b.WriteByte(out)
			p.advance(2)
			continue
		}
		b.WriteByte(ch)
		p.advance(1)
	}
	return token{}, p.syntaxErrorAt(pos, "unterminated string literal", "")
}

func (p *parser) lexNumber(pos cql2.Pos) (token, error) {
	start := p.pos
	// Integer part: required.
	for p.pos < len(p.src) && p.src[p.pos] >= '0' && p.src[p.pos] <= '9' {
		p.advance(1)
	}
	if p.pos == start {
		return token{}, p.syntaxErrorAt(pos, "malformed number", "")
	}
	// Optional fraction. JSON requires ≥1 digit after the decimal point;
	// matching that constraint keeps NumLit.Value (a json.Number) round-trippable.
	if p.pos < len(p.src) && p.src[p.pos] == '.' {
		p.advance(1)
		fracStart := p.pos
		for p.pos < len(p.src) && p.src[p.pos] >= '0' && p.src[p.pos] <= '9' {
			p.advance(1)
		}
		if p.pos == fracStart {
			return token{}, p.syntaxErrorAt(pos, "malformed number: missing fraction digits", p.src[start:p.pos])
		}
	}
	// Optional exponent.
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

// isWKTDimTag reports whether t is one of the WKT dimension-tag idents
// (Z / M / ZM) that may appear between a geometry-type keyword and `(`.
func isWKTDimTag(t token) bool {
	return keywordEqual(t, "Z") || keywordEqual(t, "M") || keywordEqual(t, "ZM")
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

// reservedKeywords lists identifiers that may not appear as bare property
// references or as user-defined function-call names. Operator-keyword
// functions (S_INTERSECTS, T_AFTER, CASEI, …) and typed-literal constructors
// (TIMESTAMP, DATE, INTERVAL, BBOX) are routed by their own switches before
// the reserved check fires, so they are still accepted in their proper
// positions. The set mirrors text/encode.go isBareIdent so an AST cannot
// be encoded as text the parser would later reject.
var reservedKeywords = map[string]bool{
	"and": true, "or": true, "not": true,
	"like": true, "between": true, "in": true, "is": true, "null": true,
	"true": true, "false": true,
	"date": true, "timestamp": true, "interval": true, "bbox": true,
	"point": true, "linestring": true, "polygon": true,
	"multipoint": true, "multilinestring": true, "multipolygon": true,
	"geometrycollection": true,
	"casei":              true, "accenti": true, "div": true,
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
	if err := p.enterDepth(p.curPos()); err != nil {
		return nil, err
	}
	defer p.leaveDepth()
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
		opTok, _ := p.consumeToken()
		right, err := p.parseAnd()
		if err != nil {
			return nil, err
		}
		// Flatten left-assoc into a single OR with multiple args.
		if op, ok := left.(*cql2.Op); ok && op.Op == cql2.OpOr {
			op.Args = append(op.Args, right)
		} else {
			n := &cql2.Op{Op: cql2.OpOr, Args: []cql2.Node{left, right}}
			p.recordPos(n, opTok.pos)
			left = n
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
		opTok, _ := p.consumeToken()
		right, err := p.parseNot()
		if err != nil {
			return nil, err
		}
		if op, ok := left.(*cql2.Op); ok && op.Op == cql2.OpAnd {
			op.Args = append(op.Args, right)
		} else {
			n := &cql2.Op{Op: cql2.OpAnd, Args: []cql2.Node{left, right}}
			p.recordPos(n, opTok.pos)
			left = n
		}
	}
}

func (p *parser) parseNot() (cql2.Node, error) {
	t, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	if keywordEqual(t, "NOT") {
		notTok, _ := p.consumeToken()
		// Recurse into parseNot, not parsePredicate, so consecutive
		// NOTs (e.g. NOT NOT x) are folded correctly. Each NOT adds a
		// frame, so bump the depth counter to protect against deep chains.
		if err := p.enterDepth(t.pos); err != nil {
			return nil, err
		}
		inner, err := p.parseNot()
		p.leaveDepth()
		if err != nil {
			return nil, err
		}
		n := &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{inner}}
		p.recordPos(n, notTok.pos)
		return n, nil
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
	cmp := func(op cql2.Operator) (cql2.Node, error) {
		opTok, _ := p.consumeToken()
		rhs, err := p.parseAdditive()
		if err != nil {
			return nil, err
		}
		n := &cql2.Op{Op: op, Args: []cql2.Node{lhs, rhs}}
		p.recordPos(n, opTok.pos)
		return n, nil
	}
	switch {
	case t.kind == tokEq:
		return cmp(cql2.OpEq)
	case t.kind == tokNeq:
		return cmp(cql2.OpNeq)
	case t.kind == tokLt:
		return cmp(cql2.OpLt)
	case t.kind == tokLte:
		return cmp(cql2.OpLte)
	case t.kind == tokGt:
		return cmp(cql2.OpGt)
	case t.kind == tokGte:
		return cmp(cql2.OpGte)
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
			// Not a valid trailing predicate. We can't put notTok back because the
			// lookahead slot is already populated by t2; emit a syntax error pinned
			// at the original NOT instead.
			return nil, p.syntaxErrorAt(notTok.pos, "expected BETWEEN, IN, or LIKE after NOT", t2.text)
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
	inner := &cql2.Op{Op: cql2.OpBetween, Args: []cql2.Node{lhs, lo, hi}}
	p.recordPos(inner, t.pos)
	if negate {
		outer := &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{inner}}
		p.recordPos(outer, t.pos)
		return outer, nil
	}
	return inner, nil
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
	arr := &cql2.ArrayLit{Elements: elems}
	p.recordPos(arr, lp.pos)
	inner := &cql2.Op{Op: cql2.OpIn, Args: []cql2.Node{lhs, arr}}
	p.recordPos(inner, t.pos)
	if negate {
		outer := &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{inner}}
		p.recordPos(outer, t.pos)
		return outer, nil
	}
	return inner, nil
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
	inner := &cql2.Op{Op: cql2.OpIsNull, Args: []cql2.Node{lhs}}
	p.recordPos(inner, t.pos)
	if negate {
		outer := &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{inner}}
		p.recordPos(outer, t.pos)
		return outer, nil
	}
	return inner, nil
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
	inner := &cql2.Op{Op: cql2.OpLike, Args: []cql2.Node{lhs, rhs}}
	p.recordPos(inner, t.pos)
	if negate {
		outer := &cql2.Op{Op: cql2.OpNot, Args: []cql2.Node{inner}}
		p.recordPos(outer, t.pos)
		return outer, nil
	}
	return inner, nil
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
		opTok, _ := p.consumeToken()
		right, err := p.parseMultiplicative()
		if err != nil {
			return nil, err
		}
		n := &cql2.Op{Op: op, Args: []cql2.Node{left, right}}
		p.recordPos(n, opTok.pos)
		left = n
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
		opTok, _ := p.consumeToken()
		right, err := p.parsePower()
		if err != nil {
			return nil, err
		}
		n := &cql2.Op{Op: op, Args: []cql2.Node{left, right}}
		p.recordPos(n, opTok.pos)
		left = n
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
	opTok, _ := p.consumeToken()
	// Right-associative; each `^` adds a stack frame.
	if err := p.enterDepth(t.pos); err != nil {
		return nil, err
	}
	right, err := p.parsePower()
	p.leaveDepth()
	if err != nil {
		return nil, err
	}
	n := &cql2.Op{Op: cql2.OpPow, Args: []cql2.Node{left, right}}
	p.recordPos(n, opTok.pos)
	return n, nil
}

func (p *parser) parseUnary() (cql2.Node, error) {
	t, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	if t.kind == tokMinus {
		_, _ = p.consumeToken()
		if err := p.enterDepth(t.pos); err != nil {
			return nil, err
		}
		inner, err := p.parseUnary()
		p.leaveDepth()
		if err != nil {
			return nil, err
		}
		// Fold unary minus into a numeric literal. Double-negation strips the
		// existing minus prefix. Non-numeric operands are rejected: CQL2 spec
		// only allows unary minus on numeric literals.
		nl, ok := inner.(*cql2.NumLit)
		if !ok {
			return nil, p.syntaxErrorAt(t.pos, "unary minus only applies to numeric literals", "-")
		}
		v := string(nl.Value)
		if strings.HasPrefix(v, "-") {
			v = v[1:]
		} else {
			v = "-" + v
		}
		folded := &cql2.NumLit{Value: json.Number(v)}
		p.recordPos(folded, t.pos)
		return folded, nil
	}
	if t.kind == tokPlus {
		_, _ = p.consumeToken()
		if err := p.enterDepth(t.pos); err != nil {
			return nil, err
		}
		n, err := p.parseUnary()
		p.leaveDepth()
		return n, err
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
		// Parenthesised expression OR bare array literal: "(a, b, c)".
		// Empty "()" is rejected as ambiguous.
		la, err := p.peekToken()
		if err != nil {
			return nil, err
		}
		if la.kind == tokRParen {
			_, _ = p.consumeToken()
			return nil, p.syntaxErrorAt(la.pos, "empty parenthesised expression", la.text)
		}
		inner, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		nxt, err := p.peekToken()
		if err != nil {
			return nil, err
		}
		switch nxt.kind {
		case tokRParen:
			_, _ = p.consumeToken()
			return inner, nil
		case tokComma:
			elements := []cql2.Node{inner}
			for {
				_, _ = p.consumeToken() // consume ','
				next, err := p.parseExpression()
				if err != nil {
					return nil, err
				}
				elements = append(elements, next)
				peek, err := p.peekToken()
				if err != nil {
					return nil, err
				}
				if peek.kind == tokRParen {
					_, _ = p.consumeToken()
					arr := &cql2.ArrayLit{Elements: elements}
					p.recordPos(arr, t.pos)
					return arr, nil
				}
				if peek.kind != tokComma {
					return nil, p.syntaxErrorAt(peek.pos, "expected ',' or ')' in array literal", peek.text)
				}
			}
		default:
			return nil, p.syntaxErrorAt(nxt.pos, "expected ')' to close expression", nxt.text)
		}
	case tokNumber:
		n := &cql2.NumLit{Value: json.Number(t.text)}
		p.recordPos(n, t.pos)
		return n, nil
	case tokString:
		n := &cql2.StringLit{Value: t.text}
		p.recordPos(n, t.pos)
		return n, nil
	case tokQuotedIdent:
		n := &cql2.PropertyRef{Name: t.text}
		p.recordPos(n, t.pos)
		return n, nil
	case tokIdent:
		n, err := p.parseIdentPrimary(t)
		if err == nil {
			p.recordPos(n, t.pos)
		}
		return n, err
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
	// Geometry keywords are reserved, never property names; route them before
	// the bare-property fallback. The lookahead may be `(`, `EMPTY`, or a
	// dimension tag (`Z`/`M`/`ZM`).
	if geometryKeywords[lower] {
		if la.kind == tokLParen || keywordEqual(la, "EMPTY") || isWKTDimTag(la) {
			return p.parseGeometryLiteral(t)
		}
	}

	if la.kind != tokLParen {
		// Bare property reference. Reserved keywords (AND, OR, NOT, …,
		// geometry types, typed-literal constructors) cannot appear here;
		// they must be quoted to be used as property names.
		if reservedKeywords[lower] {
			return nil, p.syntaxErrorAt(t.pos, fmt.Sprintf("reserved keyword %q cannot be used as a bare property reference (quote it as %q)", t.text, "\""+t.text+"\""), t.text)
		}
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

	// Otherwise: function-call (which may desugar into an Op). Geometry
	// keywords are routed earlier — see the dispatch above the LParen check.
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
	tm, err := time.ParseInLocation("2006-01-02", s, dateLocation(p.cfg))
	if err != nil {
		return nil, p.syntaxErrorAt(at, fmt.Sprintf("malformed DATE literal: %v", err), s)
	}
	return &cql2.DateLit{Value: tm}, nil
}

// dateLocation returns the timezone used for bare DATE literals,
// honouring WithDateTimezone and falling back to UTC.
func dateLocation(cfg *cql2.Config) *time.Location {
	if cfg != nil && cfg.DateTimezone != nil {
		return cfg.DateTimezone
	}
	return time.UTC
}

func (p *parser) parseIntervalConstructor(at cql2.Pos) (cql2.Node, error) {
	if err := p.expectKind(tokLParen, "("); err != nil {
		return nil, err
	}
	startEP, err := p.parseIntervalArg(at, "start")
	if err != nil {
		return nil, err
	}
	if err := p.expectKind(tokComma, ","); err != nil {
		return nil, err
	}
	endEP, err := p.parseIntervalArg(at, "end")
	if err != nil {
		return nil, err
	}
	if err := p.expectKind(tokRParen, ")"); err != nil {
		return nil, err
	}
	return &cql2.IntervalLit{Start: startEP, End: endEP}, nil
}

// parseIntervalArg accepts a single-quoted string literal (parsed via
// parseIntervalEndpoint into a TimestampLit/DateLit/Unbounded), an unquoted
// identifier / quoted-identifier (built into a *PropertyRef), or a function
// call (e.g. INTERVAL(now(), ..)).
func (p *parser) parseIntervalArg(at cql2.Pos, which string) (cql2.IntervalEndpoint, error) {
	t, err := p.peekToken()
	if err != nil {
		return nil, err
	}
	switch t.kind {
	case tokString:
		_, _ = p.consumeToken()
		ep, err := parseIntervalEndpoint(t.text, dateLocation(p.cfg))
		if err != nil {
			return nil, p.syntaxErrorAt(at, fmt.Sprintf("malformed INTERVAL %s: %v", which, err), t.text)
		}
		if n, ok := ep.(cql2.Node); ok {
			p.recordPos(n, t.pos)
		}
		return ep, nil
	case tokIdent:
		_, _ = p.consumeToken()
		// Function call: identifier immediately followed by '('.
		if next, _ := p.peekToken(); next.kind == tokLParen {
			node, err := p.parseFunctionCall(t)
			if err != nil {
				return nil, err
			}
			fn, ok := node.(*cql2.FunctionCall)
			if !ok {
				return nil, p.syntaxErrorAt(t.pos,
					"only user-defined functions are valid as INTERVAL endpoints", t.text)
			}
			return fn, nil
		}
		pr := &cql2.PropertyRef{Name: t.text}
		p.recordPos(pr, t.pos)
		return pr, nil
	case tokQuotedIdent:
		_, _ = p.consumeToken()
		pr := &cql2.PropertyRef{Name: t.text}
		p.recordPos(pr, t.pos)
		return pr, nil
	default:
		return nil, p.syntaxErrorAt(t.pos, "expected string literal, property reference, or function call in INTERVAL", t.text)
	}
}

func parseIntervalEndpoint(s string, dateLoc *time.Location) (cql2.IntervalEndpoint, error) {
	if s == ".." {
		return &cql2.Unbounded{}, nil
	}
	if tm, err := time.Parse(time.RFC3339Nano, s); err == nil {
		return &cql2.TimestampLit{Value: tm}, nil
	}
	if dateLoc == nil {
		dateLoc = time.UTC
	}
	if tm, err := time.ParseInLocation("2006-01-02", s, dateLoc); err == nil {
		return &cql2.DateLit{Value: tm}, nil
	}
	return nil, fmt.Errorf("not a recognised RFC3339 timestamp or date: %q", s)
}

func (p *parser) parseBBoxConstructor(at cql2.Pos) (cql2.Node, error) {
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
		return nil, p.syntaxErrorAt(at, fmt.Sprintf("BBOX requires 4 or 6 coordinates, got %d", len(coords)), "")
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
	// Consume any dimension tag so the next-token check passes; the captured
	// span (from startOff) still includes the tag, which wkt.Parse re-scans.
	if isWKTDimTag(la) {
		_, _ = p.consumeToken()
		la, err = p.peekToken()
		if err != nil {
			return nil, err
		}
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
	lower := strings.ToLower(name.text)
	if op, ok := operatorFunctionNames[lower]; ok {
		return &cql2.Op{Op: op, Args: args}, nil
	}
	// Reserved keywords (AND, OR, NOT, BETWEEN, IN, …) cannot be used
	// as user-defined function names. Typed-literal constructors and
	// geometry keywords are routed before parseFunctionCall, so they
	// never reach this check in their proper positions.
	if reservedKeywords[lower] {
		return nil, p.syntaxErrorAt(name.pos, fmt.Sprintf("reserved keyword %q cannot be used as a function name", name.text), name.text)
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
