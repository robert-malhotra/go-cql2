package json

import (
	"bytes"
	stdjson "encoding/json"
	"fmt"
	"strconv"
	"time"

	cql2 "github.com/example/go-cql2"
	"github.com/example/go-cql2/geojson"
)

// Parse decodes a CQL2-JSON document into a cql2.Node.
//
// Errors are *cql2.SyntaxError with Encoding=EncodingJSON and a JSON-Pointer
// path identifying the offending location.
func Parse(data []byte, opts ...cql2.Option) (cql2.Node, error) {
	// TODO(wave-3): consume opts
	_ = opts
	return parseNode(stdjson.RawMessage(data), "")
}

// known operator set, computed once.
var knownOps = func() map[cql2.Operator]struct{} {
	all := []cql2.Operator{
		cql2.OpAnd, cql2.OpOr, cql2.OpNot,
		cql2.OpEq, cql2.OpNeq, cql2.OpLt, cql2.OpLte, cql2.OpGt, cql2.OpGte,
		cql2.OpLike, cql2.OpBetween, cql2.OpIn, cql2.OpIsNull,
		cql2.OpAdd, cql2.OpSub, cql2.OpMul, cql2.OpDiv, cql2.OpMod, cql2.OpPow, cql2.OpIDiv,
		cql2.OpSIntersects, cql2.OpSEquals, cql2.OpSDisjoint, cql2.OpSTouches,
		cql2.OpSWithin, cql2.OpSOverlaps, cql2.OpSCrosses, cql2.OpSContains,
		cql2.OpTAfter, cql2.OpTBefore, cql2.OpTContains, cql2.OpTDisjoint,
		cql2.OpTDuring, cql2.OpTEquals, cql2.OpTFinishedBy, cql2.OpTFinishes,
		cql2.OpTIntersects, cql2.OpTMeets, cql2.OpTMetBy, cql2.OpTOverlappedBy,
		cql2.OpTOverlaps, cql2.OpTStartedBy, cql2.OpTStarts,
		cql2.OpAContains, cql2.OpAContainedBy, cql2.OpAEquals, cql2.OpAOverlaps,
		cql2.OpCaseI, cql2.OpAccentI,
	}
	m := make(map[cql2.Operator]struct{}, len(all))
	for _, o := range all {
		m[o] = struct{}{}
	}
	return m
}()

// geometryTypes are the GeoJSON geometry "type" values that map to *GeomLit.
var geometryTypes = map[string]struct{}{
	"Point":              {},
	"LineString":         {},
	"Polygon":            {},
	"MultiPoint":         {},
	"MultiLineString":    {},
	"MultiPolygon":       {},
	"GeometryCollection": {},
}

// objectKeys is the canonical accepted-keys list reported in error messages.
var objectKeys = []string{
	"op", "function", "property", "timestamp", "date",
	"interval", "bbox", "type",
}

// serr builds a *cql2.SyntaxError at the given JSON-pointer path.
func serr(path, msg string) error {
	return &cql2.SyntaxError{
		Encoding: cql2.EncodingJSON,
		At:       cql2.Pos{JSONPath: path},
		Msg:      msg,
	}
}

// serrGE builds a *cql2.SyntaxError with Got/Expected populated.
func serrGE(path, msg, got string, expected []string) error {
	return &cql2.SyntaxError{
		Encoding: cql2.EncodingJSON,
		At:       cql2.Pos{JSONPath: path},
		Msg:      msg,
		Got:      got,
		Expected: expected,
	}
}

// trimWS returns raw with leading/trailing JSON whitespace removed.
func trimWS(raw []byte) []byte {
	i := 0
	for i < len(raw) {
		c := raw[i]
		if c == ' ' || c == '\t' || c == '\n' || c == '\r' {
			i++
			continue
		}
		break
	}
	j := len(raw)
	for j > i {
		c := raw[j-1]
		if c == ' ' || c == '\t' || c == '\n' || c == '\r' {
			j--
			continue
		}
		break
	}
	return raw[i:j]
}

// parseNode dispatches by first non-whitespace byte and recurses.
func parseNode(raw stdjson.RawMessage, path string) (cql2.Node, error) {
	t := trimWS(raw)
	if len(t) == 0 {
		return nil, serr(path, "empty value")
	}
	switch c := t[0]; {
	case c == 't' || c == 'f':
		var b bool
		if err := stdjson.Unmarshal(t, &b); err != nil {
			return nil, serr(path, fmt.Sprintf("invalid boolean: %v", err))
		}
		return &cql2.BoolLit{Value: b}, nil
	case c == 'n':
		var v any
		if err := stdjson.Unmarshal(t, &v); err != nil {
			return nil, serr(path, fmt.Sprintf("invalid null: %v", err))
		}
		if v != nil {
			return nil, serr(path, "expected null")
		}
		return &cql2.NullLit{}, nil
	case c == '"':
		var s string
		if err := stdjson.Unmarshal(t, &s); err != nil {
			return nil, serr(path, fmt.Sprintf("invalid string: %v", err))
		}
		return &cql2.StringLit{Value: s}, nil
	case c == '[':
		return parseArray(t, path)
	case c == '{':
		return parseObject(t, path)
	case c == '-' || (c >= '0' && c <= '9'):
		return parseNumber(t, path)
	}
	return nil, serr(path, fmt.Sprintf("unexpected character %q", t[0]))
}

func parseNumber(raw []byte, path string) (cql2.Node, error) {
	// Validate via stdjson.Number, then preserve verbatim source text.
	s := string(raw)
	// stdjson.Number uses the same grammar as JSON numbers; try ParseFloat
	// and ParseInt to validate.
	if _, err := strconv.ParseFloat(s, 64); err != nil {
		// Fallback: also try big-int-shaped numbers ParseFloat handles those too,
		// so a failure here is a real syntax issue.
		return nil, serr(path, fmt.Sprintf("invalid number: %v", err))
	}
	return &cql2.NumLit{Value: stdjson.Number(s)}, nil
}

func parseArray(raw []byte, path string) (cql2.Node, error) {
	var elems []stdjson.RawMessage
	if err := stdjson.Unmarshal(raw, &elems); err != nil {
		return nil, serr(path, fmt.Sprintf("invalid array: %v", err))
	}
	out := make([]cql2.Node, len(elems))
	for i, e := range elems {
		child, err := parseNode(e, joinPath(path, strconv.Itoa(i)))
		if err != nil {
			return nil, err
		}
		out[i] = child
	}
	return &cql2.ArrayLit{Elements: out}, nil
}

func parseObject(raw []byte, path string) (cql2.Node, error) {
	var obj map[string]stdjson.RawMessage
	dec := stdjson.NewDecoder(bytes.NewReader(raw))
	if err := dec.Decode(&obj); err != nil {
		return nil, serr(path, fmt.Sprintf("invalid object: %v", err))
	}

	// Geometry object: any object with type=<geometry-type-string>.
	if rawType, ok := obj["type"]; ok {
		var typ string
		if err := stdjson.Unmarshal(rawType, &typ); err == nil {
			if _, isGeom := geometryTypes[typ]; isGeom {
				return parseGeometry(raw, path)
			}
		}
	}

	switch {
	case has(obj, "op"):
		return parseOp(obj, path)
	case has(obj, "function"):
		return parseFunction(obj, path)
	case has(obj, "property"):
		return parseProperty(obj, path)
	case has(obj, "timestamp"):
		return parseTimestamp(obj, path)
	case has(obj, "date"):
		return parseDate(obj, path)
	case has(obj, "interval"):
		return parseInterval(obj, path)
	case has(obj, "bbox"):
		return parseBBox(obj, path)
	}

	return nil, serrGE(path, "object missing recognized discriminator key",
		keysOf(obj), objectKeys)
}

func parseOp(obj map[string]stdjson.RawMessage, path string) (cql2.Node, error) {
	rawOp, ok := obj["op"]
	if !ok {
		return nil, serr(path, "missing \"op\"")
	}
	var opStr string
	if err := stdjson.Unmarshal(rawOp, &opStr); err != nil {
		return nil, serr(joinPath(path, "op"),
			fmt.Sprintf("\"op\" must be a string: %v", err))
	}
	op := cql2.Operator(opStr)
	if _, ok := knownOps[op]; !ok {
		return nil, serrGE(joinPath(path, "op"), "unknown operator", opStr, nil)
	}

	rawArgs, ok := obj["args"]
	if !ok {
		return nil, serr(path, "operator object missing \"args\"")
	}
	var rawList []stdjson.RawMessage
	if err := stdjson.Unmarshal(rawArgs, &rawList); err != nil {
		return nil, serr(joinPath(path, "args"),
			fmt.Sprintf("\"args\" must be an array: %v", err))
	}
	args := make([]cql2.Node, len(rawList))
	for i, r := range rawList {
		child, err := parseNode(r, joinPath(path, "args", strconv.Itoa(i)))
		if err != nil {
			return nil, err
		}
		args[i] = child
	}
	return &cql2.Op{Op: op, Args: args}, nil
}

func parseFunction(obj map[string]stdjson.RawMessage, path string) (cql2.Node, error) {
	rawFn := obj["function"]
	var fn map[string]stdjson.RawMessage
	if err := stdjson.Unmarshal(rawFn, &fn); err != nil {
		return nil, serr(joinPath(path, "function"),
			fmt.Sprintf("\"function\" must be an object: %v", err))
	}
	rawName, ok := fn["name"]
	if !ok {
		return nil, serr(joinPath(path, "function"), "missing \"name\"")
	}
	var name string
	if err := stdjson.Unmarshal(rawName, &name); err != nil {
		return nil, serr(joinPath(path, "function", "name"),
			fmt.Sprintf("\"name\" must be a string: %v", err))
	}
	rawArgs, ok := fn["args"]
	if !ok {
		return nil, serr(joinPath(path, "function"), "missing \"args\"")
	}
	var rawList []stdjson.RawMessage
	if err := stdjson.Unmarshal(rawArgs, &rawList); err != nil {
		return nil, serr(joinPath(path, "function", "args"),
			fmt.Sprintf("\"args\" must be an array: %v", err))
	}
	args := make([]cql2.Node, len(rawList))
	for i, r := range rawList {
		child, err := parseNode(r, joinPath(path, "function", "args", strconv.Itoa(i)))
		if err != nil {
			return nil, err
		}
		args[i] = child
	}
	return &cql2.FunctionCall{Name: name, Args: args}, nil
}

func parseProperty(obj map[string]stdjson.RawMessage, path string) (cql2.Node, error) {
	var name string
	if err := stdjson.Unmarshal(obj["property"], &name); err != nil {
		return nil, serr(joinPath(path, "property"),
			fmt.Sprintf("\"property\" must be a string: %v", err))
	}
	return &cql2.PropertyRef{Name: name}, nil
}

func parseTimestamp(obj map[string]stdjson.RawMessage, path string) (cql2.Node, error) {
	var s string
	if err := stdjson.Unmarshal(obj["timestamp"], &s); err != nil {
		return nil, serr(joinPath(path, "timestamp"),
			fmt.Sprintf("\"timestamp\" must be a string: %v", err))
	}
	tt, err := time.Parse(time.RFC3339Nano, s)
	if err != nil {
		return nil, serr(joinPath(path, "timestamp"),
			fmt.Sprintf("invalid RFC3339 timestamp: %v", err))
	}
	return &cql2.TimestampLit{Value: tt}, nil
}

func parseDate(obj map[string]stdjson.RawMessage, path string) (cql2.Node, error) {
	var s string
	if err := stdjson.Unmarshal(obj["date"], &s); err != nil {
		return nil, serr(joinPath(path, "date"),
			fmt.Sprintf("\"date\" must be a string: %v", err))
	}
	tt, err := time.ParseInLocation("2006-01-02", s, time.UTC)
	if err != nil {
		return nil, serr(joinPath(path, "date"),
			fmt.Sprintf("invalid YYYY-MM-DD date: %v", err))
	}
	return &cql2.DateLit{Value: tt}, nil
}

func parseInterval(obj map[string]stdjson.RawMessage, path string) (cql2.Node, error) {
	var endpoints []string
	if err := stdjson.Unmarshal(obj["interval"], &endpoints); err != nil {
		return nil, serr(joinPath(path, "interval"),
			fmt.Sprintf("\"interval\" must be an array of two strings: %v", err))
	}
	if len(endpoints) != 2 {
		return nil, serr(joinPath(path, "interval"),
			fmt.Sprintf("interval must have exactly 2 endpoints, got %d", len(endpoints)))
	}
	parseEnd := func(s string, idx int) (cql2.IntervalEndpoint, error) {
		if s == ".." {
			return &cql2.Unbounded{}, nil
		}
		if tt, err := time.Parse(time.RFC3339Nano, s); err == nil {
			return &cql2.TimestampLit{Value: tt}, nil
		}
		if tt, err := time.ParseInLocation("2006-01-02", s, time.UTC); err == nil {
			return &cql2.DateLit{Value: tt}, nil
		}
		return nil, serr(joinPath(path, "interval", strconv.Itoa(idx)),
			fmt.Sprintf("invalid interval endpoint %q", s))
	}
	start, err := parseEnd(endpoints[0], 0)
	if err != nil {
		return nil, err
	}
	end, err := parseEnd(endpoints[1], 1)
	if err != nil {
		return nil, err
	}
	return &cql2.IntervalLit{Start: start, End: end}, nil
}

func parseBBox(obj map[string]stdjson.RawMessage, path string) (cql2.Node, error) {
	var coords []float64
	if err := stdjson.Unmarshal(obj["bbox"], &coords); err != nil {
		return nil, serr(joinPath(path, "bbox"),
			fmt.Sprintf("\"bbox\" must be a number array: %v", err))
	}
	if len(coords) != 4 && len(coords) != 6 {
		return nil, serr(joinPath(path, "bbox"),
			fmt.Sprintf("bbox must have 4 or 6 numbers, got %d", len(coords)))
	}
	return &cql2.BBoxLit{Coords: coords}, nil
}

func parseGeometry(raw []byte, path string) (cql2.Node, error) {
	g, err := geojson.Parse(raw)
	if err != nil {
		// Re-anchor the error's JSONPath under our current path. The geojson
		// path is already an RFC 6901 pointer ("" for root, "/coordinates"
		// for nested). Concatenation is the correct operation.
		if ge, ok := err.(*cql2.GeometryError); ok {
			ge.At.JSONPath = path + ge.At.JSONPath
			return nil, ge
		}
		return nil, err
	}
	return &cql2.GeomLit{Geom: g}, nil
}

// joinPath builds an RFC 6901 JSON Pointer by appending segments to base.
// Each segment is escaped per RFC 6901 (~ → ~0, / → ~1). An empty segment
// is permitted (yields a trailing "/").
func joinPath(base string, segments ...string) string {
	out := base
	for _, s := range segments {
		if s == "" {
			out += "/"
			continue
		}
		// Escape per RFC 6901.
		needs := false
		for i := 0; i < len(s); i++ {
			if s[i] == '~' || s[i] == '/' {
				needs = true
				break
			}
		}
		if !needs {
			out += "/" + s
			continue
		}
		var b bytes.Buffer
		b.WriteByte('/')
		for i := 0; i < len(s); i++ {
			switch s[i] {
			case '~':
				b.WriteString("~0")
			case '/':
				b.WriteString("~1")
			default:
				b.WriteByte(s[i])
			}
		}
		out += b.String()
	}
	return out
}

func has(obj map[string]stdjson.RawMessage, k string) bool {
	_, ok := obj[k]
	return ok
}

func keysOf(obj map[string]stdjson.RawMessage) string {
	if len(obj) == 0 {
		return "{}"
	}
	out := make([]byte, 0, 32)
	out = append(out, '{')
	first := true
	for k := range obj {
		if !first {
			out = append(out, ',')
		}
		first = false
		out = append(out, k...)
	}
	out = append(out, '}')
	return string(out)
}
