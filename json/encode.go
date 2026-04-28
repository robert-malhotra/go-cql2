package json

import (
	"bytes"
	stdjson "encoding/json"
	"fmt"
	"strconv"

	cql2 "github.com/example/go-cql2"
	"github.com/example/go-cql2/geojson"
)

// Encode serializes n as compact, deterministic CQL2-JSON. Object keys are
// emitted in fixed order so identical ASTs always produce identical bytes.
//
// Geometry literals are delegated to github.com/example/go-cql2/geojson.
func Encode(n cql2.Node, opts ...cql2.Option) ([]byte, error) {
	// TODO(wave-3): consume opts
	_ = opts
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
		if err := writeJSONString(buf, x.Value.Format("2006-01-02T15:04:05.999999999Z07:00")); err != nil {
			return err
		}
		buf.WriteByte('}')
		return nil
	case *cql2.DateLit:
		buf.WriteString(`{"date":`)
		if err := writeJSONString(buf, x.Value.Format("2006-01-02")); err != nil {
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
		b, err := geojson.Encode(x.Geom)
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
		buf.WriteString(`,"args":[`)
		for i, a := range x.Args {
			if i > 0 {
				buf.WriteByte(',')
			}
			if err := encodeNode(buf, a); err != nil {
				return err
			}
		}
		buf.WriteString(`]}`)
		return nil
	case *cql2.FunctionCall:
		buf.WriteString(`{"function":{"name":`)
		if err := writeJSONString(buf, x.Name); err != nil {
			return err
		}
		buf.WriteString(`,"args":[`)
		for i, a := range x.Args {
			if i > 0 {
				buf.WriteByte(',')
			}
			if err := encodeNode(buf, a); err != nil {
				return err
			}
		}
		buf.WriteString(`]}}`)
		return nil
	}
	return fmt.Errorf("cql2/json: unsupported node type %T", n)
}

func writeEndpoint(buf *bytes.Buffer, e cql2.IntervalEndpoint) error {
	switch v := e.(type) {
	case *cql2.Unbounded:
		return writeJSONString(buf, "..")
	case *cql2.TimestampLit:
		return writeJSONString(buf, v.Value.Format("2006-01-02T15:04:05.999999999Z07:00"))
	case *cql2.DateLit:
		return writeJSONString(buf, v.Value.Format("2006-01-02"))
	case nil:
		return writeJSONString(buf, "..")
	}
	return fmt.Errorf("cql2/json: unsupported interval endpoint %T", e)
}

func writeJSONString(buf *bytes.Buffer, s string) error {
	b, err := stdjson.Marshal(s)
	if err != nil {
		return err
	}
	buf.Write(b)
	return nil
}
