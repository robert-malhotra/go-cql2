# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed
- `NOT NOT x` now parses correctly. Previously the second `NOT` was consumed
  as a property reference and produced a "trailing input" error.
  (`text/parse.go:parseNot` now recurses into itself.)
- The text number lexer no longer accepts JSON-incompatible numbers like
  `1.`, `.5`, or `1.e5`. These produced a `NumLit.Value` that re-encoded to
  invalid JSON and broke the round-trip invariant.
- Reserved keywords (`AND`, `OR`, `NOT`, `LIKE`, `BETWEEN`, `IN`, `IS`, `NULL`,
  `TRUE`, `FALSE`, geometry types, `DATE`, `TIMESTAMP`, `INTERVAL`, `BBOX`,
  `CASEI`, `ACCENTI`, `DIV`) are now rejected as bare property names and as
  user-defined function-call names. Quote them as `"NULL"` to use them as
  property names.
- BBOX arity errors are now anchored at the BBOX keyword position rather than
  after the closing paren.

### Added
- Text string literals now accept the backslash escape sequences required
  by `/req/cql2-text/escaping`: `\'`, `\\`, `\a`, `\b`, `\t`, `\n`, `\v`,
  `\f`, `\r`. Previously only `''` was honored as an embedded quote.
- `*FunctionCall` is a valid `IntervalEndpoint`. Both encodings parse
  forms like `INTERVAL(now(), '..')` and the JSON `{"interval":[{"op":"now",...},".."]}`
  per Annex B's `instantParameter` rule and Annex C's `intervalArray`
  schema. `Clone`, `Equal`, `Validate`, and the encoders all flow
  function endpoints through.
- `WithMaxDepth(n int)` and `DefaultMaxDepth` (256) cap parser recursion in
  both Text and JSON parsers. Adversarial inputs with millions of nested
  parens or operator chains are now rejected with a typed `*SyntaxError`.
- The CQL2-JSON parser now rejects unknown keys in CQL2 predicate objects
  (geometry objects still tolerate foreign members per RFC 7946).
- `Validate` rejects `*FunctionCall` whose `Name` is not a valid identifier
  `[A-Za-z_][A-Za-z0-9_]*`. Such names would re-emit as text the parser
  could not consume.
- `Validate` also rejects `*FunctionCall` whose `Name` collides with a
  reserved CQL2 keyword (`s_intersects`, `t_after`, `casei`, `date`, etc.).
  Such names would canonicalise back to a typed literal or `*Op` on
  re-parse, breaking round-trip for builder-constructed ASTs.
- Property-property conformance gating expanded to cover the full
  `/req/property-property/withdraw-permissions` surface: literal-LHS
  comparisons (`'foo' = name`), property-RHS spatial / temporal / array
  predicates (`s_within(geom, other_geom)`), in addition to the existing
  both-sides-property check. Arithmetic ops with two property operands
  also fire the rule.
- `ConfBasicSpatialPlus` conformance class implements the OGC
  basic-spatial-functions-plus tier. `S_INTERSECTS` with non-Point/non-BBox
  geometry literals now correctly requires this class on top of
  `ConfBasicSpatial`. `ConfSpatial` continues to imply it.
- Unicode identifiers per Annex B's `propertyName` grammar. Property
  names may now use letters from any Unicode block (Greek, Cyrillic,
  CJK, Latin-1 supplement, etc.) and combining marks. The colon (`:`)
  is also accepted as an identifier-start character per the spec.
- Text encoder escapes control characters (`\n`, `\t`, `\r`, `\v`, `\f`,
  `\b`, `\a`) and backslashes in string literals on output, producing
  cleaner round-trippable text suitable for HTTP query strings.
- LICENSE (Apache-2.0) and this CHANGELOG.

### Tests
- Position-tracking coverage now extends to every node kind in both
  encodings: logical / comparison / arithmetic / between / in / isnull /
  like ops, array literals, interval endpoints (incl. inline date and
  timestamp parses in JSON), and all primary literal kinds.
  `TestWithPositions_TextCoverage` and `TestWithPositions_JSONCoverage`
  enumerate the full surface to guard against regressions.
- Cross-encoding round-trip property test (`TestProperty_CrossEncoding-`
  `RoundTrip`) confirms text and JSON encoders agree on the same
  semantic shape for any rapid-generated AST.
- Encoder idempotence property test (`TestProperty_EncodeIsIdempotent`)
  confirms `Encode(Parse(Encode(n))) == Encode(n)` byte-for-byte in
  both encodings — pins the canonical-form invariant.
- Parse and Encode benchmarks for both encodings, using a representative
  subset of the OGC corpus, establish a v1 performance baseline.

### Changed
- `cql2.OpAContainedBy` is now `"a_containedBy"` (camelCase) to match
  the canonical spelling in Annex C's JSON schema enum. Previously
  the value was `"a_containedby"`, producing JSON output that failed
  schema validation. Lookup tables remain case-insensitive on input.
- `PositionMap.Of(n)` is now `PositionMap.Get(n)` for consistency with
  Go's standard accessor naming on `(value, ok)` returns. Final API
  rename before v1.0.

### Removed
- `cql2.StyleMinimal` — was exported but never implemented. Drop before v1
  rather than commit to maintaining it.
