// Build a spatial filter from a WKT polygon and emit it in both encodings.
//
// Run: go run ./examples/spatial
package main

import (
	"fmt"
	"log"

	cql2 "github.com/exergy-dev/go-cql2"
	_ "github.com/exergy-dev/go-cql2/codecs"
	"github.com/exergy-dev/go-topology-suite/wkt"
)

func main() {
	geom, err := wkt.Unmarshal("POLYGON ((-10 -10, 10 -10, 10 10, -10 10, -10 -10))")
	if err != nil {
		log.Fatal(err)
	}

	expr := cql2.And(
		cql2.SIntersects("geometry", geom),
		cql2.Gt("magnitude", 4.5),
	)

	text, err := cql2.Encode(expr.Node(), cql2.EncodingText)
	if err != nil {
		log.Fatal(err)
	}

	jsn, err := cql2.Encode(expr.Node(), cql2.EncodingJSON)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("text:", string(text))
	fmt.Println("json:", string(jsn))
}
