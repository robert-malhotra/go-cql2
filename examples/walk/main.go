// Walk a parsed AST to extract every property name referenced by the filter.
//
// Run: go run ./examples/walk
package main

import (
	"fmt"
	"log"
	"sort"

	cql2 "github.com/exergy-dev/go-cql2"
	_ "github.com/exergy-dev/go-cql2/codecs"
)

func main() {
	const filter = `floors > 5 AND material IN ('brick', 'stone') AND owner LIKE 'A%'`

	n, err := cql2.Parse([]byte(filter))
	if err != nil {
		log.Fatal(err)
	}

	seen := map[string]struct{}{}
	cql2.Inspect(n, func(node cql2.Node) bool {
		if p, ok := node.(*cql2.PropertyRef); ok {
			seen[p.Name] = struct{}{}
		}
		return true
	})

	props := make([]string, 0, len(seen))
	for k := range seen {
		props = append(props, k)
	}
	sort.Strings(props)

	fmt.Println("filter    :", filter)
	fmt.Println("properties:", props)
}
