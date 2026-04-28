// Package cql2 implements parsing and encoding of OGC CQL2 filter expressions
// in both Text and JSON encodings.
//
// The top-level Parse / MustParse / Encode entry points dispatch to the
// json and text subpackages. Those subpackages register themselves with
// cql2 via init(); a side-effect import of either package therefore wires
// up its codec. The convenience package github.com/example/go-cql2/codecs
// imports both for callers that just want everything ready.
package cql2

import (
	"fmt"
	"sync"
)

var (
	codecMu      sync.RWMutex
	parseTextFn  func([]byte, []Option) (Node, error)
	parseJSONFn  func([]byte, []Option) (Node, error)
	encodeTextFn func(Node, []Option) ([]byte, error)
	encodeJSONFn func(Node, []Option) ([]byte, error)
)

// RegisterTextCodec installs the CQL2-Text parser and encoder so that
// the top-level Parse / Encode functions can dispatch to them. Called
// from text.init().
func RegisterTextCodec(
	parse func([]byte, []Option) (Node, error),
	encode func(Node, []Option) ([]byte, error),
) {
	codecMu.Lock()
	defer codecMu.Unlock()
	parseTextFn = parse
	encodeTextFn = encode
}

// RegisterJSONCodec installs the CQL2-JSON parser and encoder so that
// the top-level Parse / Encode functions can dispatch to them. Called
// from json.init().
func RegisterJSONCodec(
	parse func([]byte, []Option) (Node, error),
	encode func(Node, []Option) ([]byte, error),
) {
	codecMu.Lock()
	defer codecMu.Unlock()
	parseJSONFn = parse
	encodeJSONFn = encode
}

func getCodecs() (parseT, parseJ func([]byte, []Option) (Node, error),
	encT, encJ func(Node, []Option) ([]byte, error)) {
	codecMu.RLock()
	defer codecMu.RUnlock()
	return parseTextFn, parseJSONFn, encodeTextFn, encodeJSONFn
}

// Parse auto-detects the encoding and returns an AST.
//
// Detection rule, after trimming leading whitespace:
//   - first byte '{' or '[' -> JSON
//   - first byte 't', 'f', 'n', '"', a digit, or '-' -> ambiguous: try JSON
//     first; on failure, fall back to Text. (A bare CQL2-Text 'true' or
//     'null' is therefore parsed as a JSON boolean / null, which is the
//     intended behaviour for round-tripping.)
//   - anything else -> Text
//
// To use Parse, callers must side-effect-import a codec (e.g. the
// "github.com/example/go-cql2/codecs" convenience package).
//
// Recognized options vary by sub-codec; see text.Parse and json.Parse.
func Parse(input []byte, opts ...Option) (Node, error) {
	parseT, parseJ, _, _ := getCodecs()
	if parseT == nil || parseJ == nil {
		return nil, fmt.Errorf("cql2: codec not registered (import github.com/example/go-cql2/codecs or a specific codec)")
	}
	t := trimLeadingWS(input)
	if len(t) == 0 {
		return nil, &SyntaxError{Encoding: EncodingText, Msg: "empty input"}
	}
	switch c := t[0]; {
	case c == '{' || c == '[':
		return parseJ(input, opts)
	case c == 't' || c == 'f' || c == 'n' || c == '"' ||
		c == '-' || (c >= '0' && c <= '9'):
		// Ambiguous: try JSON first, then fall back to Text.
		if n, err := parseJ(input, opts); err == nil {
			return n, nil
		}
		return parseT(input, opts)
	default:
		return parseT(input, opts)
	}
}

// MustParse is Parse but panics on error.
func MustParse(input string, opts ...Option) Node {
	n, err := Parse([]byte(input), opts...)
	if err != nil {
		panic(err)
	}
	return n
}

// Encode emits the AST in the given encoding. opts are forwarded to the
// underlying codec.
func Encode(n Node, enc Encoding, opts ...Option) ([]byte, error) {
	_, _, encT, encJ := getCodecs()
	if encT == nil || encJ == nil {
		return nil, fmt.Errorf("cql2: codec not registered (import github.com/example/go-cql2/codecs or a specific codec)")
	}
	switch enc {
	case EncodingText:
		return encT(n, opts)
	case EncodingJSON:
		return encJ(n, opts)
	}
	return nil, fmt.Errorf("cql2: unknown encoding %d", enc)
}

func trimLeadingWS(b []byte) []byte {
	for i, c := range b {
		if c == ' ' || c == '\t' || c == '\r' || c == '\n' {
			continue
		}
		return b[i:]
	}
	return nil
}
