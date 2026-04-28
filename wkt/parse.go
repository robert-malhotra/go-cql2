package wkt

import (
	"fmt"
	"strconv"
	"strings"

	cql2 "github.com/example/go-cql2"
)

// Parse parses a WKT string into a cql2.Geometry.
func Parse(input string) (cql2.Geometry, error) {
	return ParseBytes([]byte(input))
}

// ParseBytes parses a WKT byte slice into a cql2.Geometry.
func ParseBytes(input []byte) (cql2.Geometry, error) {
	p := &parser{src: string(input), line: 1, col: 1}
	p.skipWS()
	g, err := p.parseGeometry()
	if err != nil {
		return nil, err
	}
	p.skipWS()
	if p.pos < len(p.src) {
		return nil, p.errorf("unexpected trailing input")
	}
	return g, nil
}

type parser struct {
	src       string
	pos       int
	line, col int
}

func (p *parser) curPos() cql2.Pos {
	return cql2.Pos{Line: p.line, Column: p.col, Offset: p.pos}
}

func (p *parser) errorf(format string, args ...any) error {
	return p.errorAt(p.curPos(), format, args...)
}

func (p *parser) errorAt(at cql2.Pos, format string, args ...any) error {
	msg := format
	if len(args) > 0 {
		msg = fmt.Sprintf(format, args...)
	}
	return &cql2.GeometryError{
		Encoding: cql2.EncodingText,
		At:       at,
		Msg:      msg,
	}
}

// advance moves the cursor forward by n bytes, updating line/col.
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

// peek returns the rune at the current position without advancing.
// Returns 0 at EOF.
func (p *parser) peek() byte {
	if p.pos >= len(p.src) {
		return 0
	}
	return p.src[p.pos]
}

// expectByte consumes b or returns an error.
func (p *parser) expectByte(b byte) error {
	p.skipWS()
	if p.peek() != b {
		return p.errorf("expected %q", string(b))
	}
	p.advance(1)
	return nil
}

// parseTypeKeyword reads an alphabetic keyword (case-insensitive).
func (p *parser) parseTypeKeyword() (string, cql2.Pos, error) {
	p.skipWS()
	start := p.pos
	startPos := p.curPos()
	for p.pos < len(p.src) {
		ch := p.src[p.pos]
		if (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') {
			p.advance(1)
			continue
		}
		break
	}
	if p.pos == start {
		return "", startPos, p.errorf("expected geometry type keyword")
	}
	return strings.ToUpper(p.src[start:p.pos]), startPos, nil
}

func (p *parser) parseGeometry() (cql2.Geometry, error) {
	kw, kwPos, err := p.parseTypeKeyword()
	if err != nil {
		return nil, err
	}
	// Optional dimension tag: Z / M / ZM. CQL2 does not support a measure
	// dimension (M); reject M and ZM with a clear error. Accept Z and require
	// 3D coordinates in the body.
	zTag := false
	if dim, dimPos, ok := p.tryDimensionTag(); ok {
		switch dim {
		case "Z":
			zTag = true
		case "M", "ZM":
			return nil, p.errorAt(dimPos, "CQL2 does not support measure dimension (%q)", dim)
		default:
			return nil, p.errorAt(dimPos, "unknown dimension tag %q", dim)
		}
	}
	var (
		g   cql2.Geometry
		gerr error
	)
	switch kw {
	case "POINT":
		g, gerr = p.parsePointTail()
	case "LINESTRING":
		g, gerr = p.parseLineStringTail()
	case "POLYGON":
		g, gerr = p.parsePolygonTail()
	case "MULTIPOINT":
		g, gerr = p.parseMultiPointTail()
	case "MULTILINESTRING":
		g, gerr = p.parseMultiLineStringTail()
	case "MULTIPOLYGON":
		g, gerr = p.parseMultiPolygonTail()
	case "GEOMETRYCOLLECTION":
		g, gerr = p.parseGeometryCollectionTail()
	default:
		return nil, p.errorAt(kwPos, "unknown geometry type %q", kw)
	}
	if gerr != nil {
		return nil, gerr
	}
	if zTag {
		if err := requireZ(g, p, kwPos); err != nil {
			return nil, err
		}
	}
	return g, nil
}

// tryDimensionTag looks ahead for an alphabetic token (Z/M/ZM, case-insensitive)
// that is not the start of an EMPTY keyword and not a geometry-type keyword.
// Returns the upper-cased tag, its position, and true if consumed.
func (p *parser) tryDimensionTag() (string, cql2.Pos, bool) {
	saved := *p
	p.skipWS()
	start := p.pos
	startPos := p.curPos()
	for p.pos < len(p.src) {
		ch := p.src[p.pos]
		if (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') {
			p.advance(1)
			continue
		}
		break
	}
	if p.pos == start {
		*p = saved
		return "", cql2.Pos{}, false
	}
	tok := strings.ToUpper(p.src[start:p.pos])
	switch tok {
	case "Z", "M", "ZM":
		return tok, startPos, true
	}
	// Not a dimension tag (likely "EMPTY" or unrelated). Roll back.
	*p = saved
	return "", cql2.Pos{}, false
}

// requireZ validates that every Coord in g has HasZ set. Returns a
// *cql2.GeometryError otherwise.
func requireZ(g cql2.Geometry, p *parser, at cql2.Pos) error {
	check := func(c cql2.Coord) error {
		if !c.HasZ {
			return p.errorAt(at, "Z dimension tag requires 3D coordinates")
		}
		return nil
	}
	switch v := g.(type) {
	case *cql2.Point:
		if v.Empty {
			return nil
		}
		return check(v.Coord)
	case *cql2.LineString:
		for _, c := range v.Coords {
			if err := check(c); err != nil {
				return err
			}
		}
	case *cql2.Polygon:
		for _, r := range v.Rings {
			for _, c := range r {
				if err := check(c); err != nil {
					return err
				}
			}
		}
	case *cql2.MultiPoint:
		for _, pt := range v.Points {
			if pt.Empty {
				continue
			}
			if err := check(pt.Coord); err != nil {
				return err
			}
		}
	case *cql2.MultiLineStr:
		for _, ls := range v.Lines {
			for _, c := range ls.Coords {
				if err := check(c); err != nil {
					return err
				}
			}
		}
	case *cql2.MultiPolygon:
		for _, pg := range v.Polys {
			for _, r := range pg.Rings {
				for _, c := range r {
					if err := check(c); err != nil {
						return err
					}
				}
			}
		}
	case *cql2.GeometryColl:
		for _, sub := range v.Geoms {
			if err := requireZ(sub, p, at); err != nil {
				return err
			}
		}
	}
	return nil
}

// tryEmpty checks for the "EMPTY" keyword (case-insensitive). Returns true if consumed.
func (p *parser) tryEmpty() bool {
	p.skipWS()
	const kw = "EMPTY"
	if p.pos+len(kw) > len(p.src) {
		return false
	}
	if !strings.EqualFold(p.src[p.pos:p.pos+len(kw)], kw) {
		return false
	}
	// Make sure it isn't followed by another letter (so we don't eat "EMPTYISH").
	if p.pos+len(kw) < len(p.src) {
		nxt := p.src[p.pos+len(kw)]
		if (nxt >= 'A' && nxt <= 'Z') || (nxt >= 'a' && nxt <= 'z') {
			return false
		}
	}
	p.advance(len(kw))
	return true
}

func (p *parser) parsePointTail() (*cql2.Point, error) {
	p.skipWS()
	if p.tryEmpty() {
		return &cql2.Point{Empty: true}, nil
	}
	if err := p.expectByte('('); err != nil {
		return nil, err
	}
	c, err := p.parseCoord()
	if err != nil {
		return nil, err
	}
	if err := p.expectByte(')'); err != nil {
		return nil, err
	}
	return &cql2.Point{Coord: c}, nil
}

func (p *parser) parseLineStringTail() (*cql2.LineString, error) {
	coords, err := p.parseCoordList()
	if err != nil {
		return nil, err
	}
	if len(coords) < 2 {
		return nil, p.errorf("LINESTRING requires at least 2 points")
	}
	return &cql2.LineString{Coords: coords}, nil
}

func (p *parser) parsePolygonTail() (*cql2.Polygon, error) {
	rings, err := p.parseRingList()
	if err != nil {
		return nil, err
	}
	if len(rings) == 0 {
		return nil, p.errorf("POLYGON requires at least 1 ring")
	}
	for _, r := range rings {
		if err := validateRing(r, p); err != nil {
			return nil, err
		}
	}
	return &cql2.Polygon{Rings: rings}, nil
}

func validateRing(r []cql2.Coord, p *parser) error {
	if len(r) < 4 {
		return p.errorf("polygon ring requires at least 4 points")
	}
	first, last := r[0], r[len(r)-1]
	if first.X != last.X || first.Y != last.Y || first.HasZ != last.HasZ || (first.HasZ && first.Z != last.Z) {
		return p.errorf("polygon ring is not closed")
	}
	return nil
}

func (p *parser) parseMultiPointTail() (*cql2.MultiPoint, error) {
	if err := p.expectByte('('); err != nil {
		return nil, err
	}
	var pts []cql2.Point
	p.skipWS()
	if p.peek() == ')' {
		p.advance(1)
		return &cql2.MultiPoint{Points: pts}, nil
	}
	// Decide between canonical "((x y),(x y))" and pragmatic "(x y, x y)" form.
	// Lookahead: if next non-WS is '(' -> canonical, else -> pragmatic.
	p.skipWS()
	canonical := p.peek() == '('
	for {
		p.skipWS()
		if canonical {
			if err := p.expectByte('('); err != nil {
				return nil, err
			}
			c, err := p.parseCoord()
			if err != nil {
				return nil, err
			}
			if err := p.expectByte(')'); err != nil {
				return nil, err
			}
			pts = append(pts, cql2.Point{Coord: c})
		} else {
			c, err := p.parseCoord()
			if err != nil {
				return nil, err
			}
			pts = append(pts, cql2.Point{Coord: c})
		}
		p.skipWS()
		if p.peek() == ',' {
			p.advance(1)
			continue
		}
		break
	}
	if err := p.expectByte(')'); err != nil {
		return nil, err
	}
	return &cql2.MultiPoint{Points: pts}, nil
}

func (p *parser) parseMultiLineStringTail() (*cql2.MultiLineStr, error) {
	if err := p.expectByte('('); err != nil {
		return nil, err
	}
	var lines []cql2.LineString
	for {
		p.skipWS()
		coords, err := p.parseCoordList()
		if err != nil {
			return nil, err
		}
		if len(coords) < 2 {
			return nil, p.errorf("LINESTRING requires at least 2 points")
		}
		lines = append(lines, cql2.LineString{Coords: coords})
		p.skipWS()
		if p.peek() == ',' {
			p.advance(1)
			continue
		}
		break
	}
	if err := p.expectByte(')'); err != nil {
		return nil, err
	}
	return &cql2.MultiLineStr{Lines: lines}, nil
}

func (p *parser) parseMultiPolygonTail() (*cql2.MultiPolygon, error) {
	if err := p.expectByte('('); err != nil {
		return nil, err
	}
	var polys []cql2.Polygon
	for {
		p.skipWS()
		rings, err := p.parseRingList()
		if err != nil {
			return nil, err
		}
		if len(rings) == 0 {
			return nil, p.errorf("POLYGON requires at least 1 ring")
		}
		for _, r := range rings {
			if err := validateRing(r, p); err != nil {
				return nil, err
			}
		}
		polys = append(polys, cql2.Polygon{Rings: rings})
		p.skipWS()
		if p.peek() == ',' {
			p.advance(1)
			continue
		}
		break
	}
	if err := p.expectByte(')'); err != nil {
		return nil, err
	}
	return &cql2.MultiPolygon{Polys: polys}, nil
}

func (p *parser) parseGeometryCollectionTail() (*cql2.GeometryColl, error) {
	if err := p.expectByte('('); err != nil {
		return nil, err
	}
	var geoms []cql2.Geometry
	p.skipWS()
	if p.peek() == ')' {
		p.advance(1)
		return &cql2.GeometryColl{Geoms: geoms}, nil
	}
	for {
		p.skipWS()
		g, err := p.parseGeometry()
		if err != nil {
			return nil, err
		}
		geoms = append(geoms, g)
		p.skipWS()
		if p.peek() == ',' {
			p.advance(1)
			continue
		}
		break
	}
	if err := p.expectByte(')'); err != nil {
		return nil, err
	}
	return &cql2.GeometryColl{Geoms: geoms}, nil
}

// parseCoordList parses "(c, c, ...)".
func (p *parser) parseCoordList() ([]cql2.Coord, error) {
	if err := p.expectByte('('); err != nil {
		return nil, err
	}
	var out []cql2.Coord
	for {
		c, err := p.parseCoord()
		if err != nil {
			return nil, err
		}
		out = append(out, c)
		p.skipWS()
		if p.peek() == ',' {
			p.advance(1)
			continue
		}
		break
	}
	if err := p.expectByte(')'); err != nil {
		return nil, err
	}
	return out, nil
}

// parseRingList parses "(ring, ring, ...)" where each ring is a coord list.
func (p *parser) parseRingList() ([][]cql2.Coord, error) {
	if err := p.expectByte('('); err != nil {
		return nil, err
	}
	var rings [][]cql2.Coord
	for {
		p.skipWS()
		ring, err := p.parseCoordList()
		if err != nil {
			return nil, err
		}
		rings = append(rings, ring)
		p.skipWS()
		if p.peek() == ',' {
			p.advance(1)
			continue
		}
		break
	}
	if err := p.expectByte(')'); err != nil {
		return nil, err
	}
	return rings, nil
}

// parseCoord parses "x y" or "x y z".
func (p *parser) parseCoord() (cql2.Coord, error) {
	p.skipWS()
	x, err := p.parseNumber()
	if err != nil {
		return cql2.Coord{}, err
	}
	if !p.atCoordSep() {
		return cql2.Coord{}, p.errorf("expected whitespace between coordinate components")
	}
	p.skipWS()
	y, err := p.parseNumber()
	if err != nil {
		return cql2.Coord{}, err
	}
	// Optionally a Z.
	saved := *p
	if p.atCoordSep() {
		p.skipWS()
		// Only treat it as Z if the next token is a number.
		if p.atNumberStart() {
			z, err := p.parseNumber()
			if err != nil {
				return cql2.Coord{}, err
			}
			return cql2.Coord{X: x, Y: y, Z: z, HasZ: true}, nil
		}
		// Otherwise restore and let the caller see the separator.
		*p = saved
	}
	return cql2.Coord{X: x, Y: y}, nil
}

// atCoordSep reports whether at least one whitespace byte separates us from the next token.
func (p *parser) atCoordSep() bool {
	if p.pos >= len(p.src) {
		return false
	}
	ch := p.src[p.pos]
	return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'
}

func (p *parser) atNumberStart() bool {
	if p.pos >= len(p.src) {
		return false
	}
	ch := p.src[p.pos]
	if ch == '+' || ch == '-' || ch == '.' {
		return true
	}
	if ch >= '0' && ch <= '9' {
		return true
	}
	return false
}

// parseNumber parses an optionally-signed decimal number with optional fraction
// and optional exponent.
func (p *parser) parseNumber() (float64, error) {
	p.skipWS()
	start := p.pos
	if p.pos < len(p.src) && (p.src[p.pos] == '+' || p.src[p.pos] == '-') {
		p.advance(1)
	}
	digits := false
	for p.pos < len(p.src) && p.src[p.pos] >= '0' && p.src[p.pos] <= '9' {
		p.advance(1)
		digits = true
	}
	if p.pos < len(p.src) && p.src[p.pos] == '.' {
		p.advance(1)
		for p.pos < len(p.src) && p.src[p.pos] >= '0' && p.src[p.pos] <= '9' {
			p.advance(1)
			digits = true
		}
	}
	if !digits {
		return 0, p.errorf("expected number")
	}
	if p.pos < len(p.src) && (p.src[p.pos] == 'e' || p.src[p.pos] == 'E') {
		p.advance(1)
		if p.pos < len(p.src) && (p.src[p.pos] == '+' || p.src[p.pos] == '-') {
			p.advance(1)
		}
		expDigits := false
		for p.pos < len(p.src) && p.src[p.pos] >= '0' && p.src[p.pos] <= '9' {
			p.advance(1)
			expDigits = true
		}
		if !expDigits {
			return 0, p.errorf("malformed exponent")
		}
	}
	s := p.src[start:p.pos]
	f, err := strconv.ParseFloat(s, 64)
	if err != nil {
		return 0, p.errorf("invalid number %q", s)
	}
	return f, nil
}

