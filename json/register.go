package json

import (
	cql2 "github.com/exergy-dev/go-cql2"
)

// init registers the CQL2-JSON codec with the root cql2 package so
// cql2.Parse / cql2.Encode can dispatch to it.
func init() {
	cql2.RegisterJSONCodec(
		func(b []byte, opts []cql2.Option) (cql2.Node, error) {
			return Parse(b, opts...)
		},
		func(n cql2.Node, opts []cql2.Option) ([]byte, error) {
			return Encode(n, opts...)
		},
	)
}
