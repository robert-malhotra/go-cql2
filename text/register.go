package text

import (
	cql2 "github.com/example/go-cql2"
)

// init registers the CQL2-Text codec with the root cql2 package so
// cql2.Parse / cql2.Encode can dispatch to it.
func init() {
	cql2.RegisterTextCodec(
		func(b []byte, opts []cql2.Option) (cql2.Node, error) {
			return ParseBytes(b, opts...)
		},
		func(n cql2.Node, opts []cql2.Option) ([]byte, error) {
			s, err := Encode(n, opts...)
			if err != nil {
				return nil, err
			}
			return []byte(s), nil
		},
	)
}
