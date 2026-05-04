// Parse a CQL2-Text filter, then re-encode it as JSON and back to Text.
//
// Run: go run ./examples/parse
package main

import (
	"fmt"
	"log"

	cql2 "github.com/exergy-dev/go-cql2"
	_ "github.com/exergy-dev/go-cql2/codecs"
)

func main() {
	const filter = `(floors > 5 AND material = 'brick') OR swimming_pool = true`

	n, err := cql2.Parse([]byte(filter))
	if err != nil {
		log.Fatal(err)
	}

	jsonBytes, err := cql2.Encode(n, cql2.EncodingJSON)
	if err != nil {
		log.Fatal(err)
	}

	textBytes, err := cql2.Encode(n, cql2.EncodingText)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("input :", filter)
	fmt.Println("json  :", string(jsonBytes))
	fmt.Println("text  :", string(textBytes))
}
