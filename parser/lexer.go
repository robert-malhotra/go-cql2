package cql2text

import "github.com/alecthomas/participle/v2/lexer"

var cqlDef = lexer.MustStateful(lexer.Rules{
	"Root": {
		{Name: "whitespace", Pattern: `\s+`, Action: nil},
		{Name: "Float", Pattern: `[-+]?\d+\.\d+([eE][-+]?\d+)?|[-+]?\d*\.\d+([eE][-+]?\d+)?|[-+]?\d+[eE][-+]?\d+`, Action: nil},
		{Name: "Int", Pattern: `[-+]?\d+`, Action: nil},
		{Name: "String", Pattern: `'[^']*'|"[^"]*"`, Action: nil},
		{Name: "Punct", Pattern: `[(),\[\]]`, Action: nil},
		{Name: "Operator", Pattern: `<>|<=|>=|[=<>]`, Action: nil},
		{Name: "Ident", Pattern: `[a-zA-Z_][a-zA-Z0-9_]*`, Action: nil},
	},
})
