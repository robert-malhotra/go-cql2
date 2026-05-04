// Build a CQL2 filter programmatically with the builder DSL, then encode it.
//
// Run: go run ./examples/builder
package main

import (
	"fmt"
	"log"
	"time"

	cql2 "github.com/exergy-dev/go-cql2"
	_ "github.com/exergy-dev/go-cql2/codecs"
)

func main() {
	expr := cql2.And(
		cql2.Or(
			cql2.Eq("category", "research"),
			cql2.In("category", "science", "engineering"),
		),
		cql2.Gte("cloud_cover", 0.0),
		cql2.Lt("cloud_cover", 0.2),
		cql2.TAfter("datetime", time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)),
		cql2.IsNotNull("thumbnail_url"),
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
