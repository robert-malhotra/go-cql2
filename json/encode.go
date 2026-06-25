package json

import (
	"bytes"
	"encoding/json"
	"fmt"
	"strconv"

	cql2 "github.com/exergy-dev/go-cql2"
	"github.com/exergy-dev/go-topology-suite/geojson"
)

// Encode serializes n as compact, deterministic CQL2-JSON. Object keys are
// emitted in fixed order so identical ASTs always produce identical bytes.
//
// Geometry literals are delegated to github.com/exergy-dev/go-cql2/geojson.
//
// Options are accepted for API symmetry but currently no option modifies
// JSON output.
func Encode(n cql2.Node, opts ...cql2.Option) ([]byte, error) {
	_ = cql2.ResolveOptions(opts...)
	var buf bytes.Buffer
	if err := encodeNode(&buf, n); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func encodeNode(buf *bytes.Buffer, n cql2.Node) error {
	if n == nil {
		buf.WriteString("null")
		return nil
	}
	switch x := n.(type) {
	case *cql2.BoolLit:
		if x.Value {
			buf.WriteString("true")
		} else {
			buf.WriteString("false")
		}
		return nil
	case *cql2.NullLit:
		buf.WriteString("null")
		return nil
	case *cql2.NumLit:
		// Preserve source verbatim. Validate non-empty.
		s := string(x.Value)
		if s == "" {
			return fmt.Errorf("cql2/json: NumLit has empty Value")
		}
		buf.WriteString(s)
		return nil
	case *cql2.StringLit:
		return writeJSONString(buf, x.Value)
	case *cql2.PropertyRef:
		buf.WriteString(`{"property":`)
		if err := writeJSONString(buf, x.Name); err != nil {
			return err
		}
		buf.WriteByte('}')
		return nil
	case *cql2.TimestampLit:
		buf.WriteString(`{"timestamp":`)
		if err := writeJSONString(buf, cql2.FormatTimestamp(x.Value)); err != nil {
			return err
		}
		buf.WriteByte('}')
		return nil
	case *cql2.DateLit:
		buf.WriteString(`{"date":`)
		if err := writeJSONString(buf, x.Value.UTC().Format("2006-01-02")); err != nil {
			return err
		}
		buf.WriteByte('}')
		return nil
	case *cql2.IntervalLit:
		buf.WriteString(`{"interval":[`)
		if err := writeEndpoint(buf, x.Start); err != nil {
			return err
		}
		buf.WriteByte(',')
		if err := writeEndpoint(buf, x.End); err != nil {
			return err
		}
		buf.WriteString(`]}`)
		return nil
	case *cql2.BBoxLit:
		buf.WriteString(`{"bbox":[`)
		for i, c := range x.Coords {
			if i > 0 {
				buf.WriteByte(',')
			}
			buf.WriteString(strconv.FormatFloat(c, 'g', -1, 64))
		}
		buf.WriteString(`]}`)
		return nil
	case *cql2.GeomLit:
		b, err := geojson.Marshal(x.Geom)
		if err != nil {
			return err
		}
		buf.Write(b)
		return nil
	case *cql2.ArrayLit:
		buf.WriteByte('[')
		for i, e := range x.Elements {
			if i > 0 {
				buf.WriteByte(',')
			}
			if err := encodeNode(buf, e); err != nil {
				return err
			}
		}
		buf.WriteByte(']')
		return nil
	case *cql2.Op:
		buf.WriteString(`{"op":`)
		if err := writeJSONString(buf, string(x.Op)); err != nil {
			return err
		}
		if err := writeArgs(buf, x.Args); err != nil {
			return err
		}
		buf.WriteByte('}')
		return nil
	case *cql2.FunctionCall:
		buf.WriteString(`{"function":{"name":`)
		if err := writeJSONString(buf, x.Name); err != nil {
			return err
		}
		if err := writeArgs(buf, x.Args); err != nil {
			return err
		}
		buf.WriteString(`}}`)
		return nil
	}
	return fmt.Errorf("cql2/json: unsupported node type %T", n)
}

// writeArgs emits `,"args":[ ... ]` for the shared op/function argument list.
func writeArgs(buf *bytes.Buffer, args []cql2.Node) error {
	buf.WriteString(`,"args":[`)
	for i, a := range args {
		if i > 0 {
			buf.WriteByte(',')
		}
		if err := encodeNode(buf, a); err != nil {
			return err
		}
	}
	buf.WriteByte(']')
	return nil
}

func writeEndpoint(buf *bytes.Buffer, e cql2.Node) error {
	switch v := e.(type) {
	case *cql2.Unbounded:
		return writeJSONString(buf, "..")
	case *cql2.TimestampLit:
		return writeJSONString(buf, cql2.FormatTimestamp(v.Value))
	case *cql2.DateLit:
		return writeJSONString(buf, v.Value.UTC().Format("2006-01-02"))
	case *cql2.PropertyRef:
		// Property endpoints are emitted as objects so they round-trip with
		// the JSON parser's interval-endpoint reader.
		return encodeNode(buf, v)
	case *cql2.FunctionCall:
		return encodeNode(buf, v)
	case nil:
		return writeJSONString(buf, "..")
	}
	return fmt.Errorf("cql2/json: unsupported interval endpoint %T", e)
}

func writeJSONString(buf *bytes.Buffer, s string) error {
	b, err := json.Marshal(s)
	if err != nil {
		return err
	}
	buf.Write(b)
	return nil
}
