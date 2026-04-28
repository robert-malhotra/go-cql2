// Package codecs is a convenience package: importing it (typically with
// a blank identifier) registers the CQL2-Text and CQL2-JSON codecs with
// the root cql2 package, enabling cql2.Parse, cql2.MustParse and
// cql2.Encode.
//
// Example:
//
//	import (
//	    cql2 "github.com/example/go-cql2"
//	    _ "github.com/example/go-cql2/codecs"
//	)
//
//	n, err := cql2.Parse([]byte("a = 1"))
package codecs

import (
	_ "github.com/example/go-cql2/json"
	_ "github.com/example/go-cql2/text"
)
