package cql2

import "time"

// Encoding identifies a CQL2 surface syntax.
type Encoding int

const (
	EncodingText Encoding = iota
	EncodingJSON
)

// TextStyle controls formatting of text-encoded output.
type TextStyle uint8

const (
	StyleNormal TextStyle = iota
	StyleMinimal
	StyleVerbose
)

// Option configures Parse / Encode behavior.
type Option func(*config)

// config holds resolved options shared by parsers and encoders.
type config struct {
	Conformance     Conformance
	Positions       **PositionMap
	CustomOperators map[Operator]bool
	CRS             string
	DateTimezone    *time.Location
	CaseIAsFunction bool
	TextStyle       TextStyle
}

// defaultConfig returns the baseline configuration.
func defaultConfig() *config {
	return &config{
		Conformance:  ConfAll,
		DateTimezone: time.UTC,
		TextStyle:    StyleNormal,
	}
}

// WithConformance restricts accepted features to the given conformance set.
func WithConformance(c Conformance) Option {
	return func(cfg *config) { cfg.Conformance = c }
}

// WithPositions requests source-position tracking. The parser allocates a
// PositionMap and writes its address through out.
func WithPositions(out **PositionMap) Option {
	return func(cfg *config) { cfg.Positions = out }
}

// WithCustomOperators registers extra accepted operator names.
func WithCustomOperators(ops ...Operator) Option {
	return func(cfg *config) {
		if cfg.CustomOperators == nil {
			cfg.CustomOperators = make(map[Operator]bool, len(ops))
		}
		for _, op := range ops {
			cfg.CustomOperators[op] = true
		}
	}
}

// WithCRS sets the active spatial reference system identifier.
func WithCRS(crs string) Option {
	return func(cfg *config) { cfg.CRS = crs }
}

// WithDateTimezone sets the timezone used to interpret bare DATE literals.
func WithDateTimezone(loc *time.Location) Option {
	return func(cfg *config) { cfg.DateTimezone = loc }
}

// WithCaseInsensitiveAsFunction emits CASEI/ACCENTI as function calls
// instead of as wrapped operators when encoding.
func WithCaseInsensitiveAsFunction() Option {
	return func(cfg *config) { cfg.CaseIAsFunction = true }
}

// WithTextStyle sets the text-encoding style.
func WithTextStyle(s TextStyle) Option {
	return func(cfg *config) { cfg.TextStyle = s }
}
