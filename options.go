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
	StyleVerbose
)

// Option configures Parse / Encode behavior.
type Option func(*Config)

// Config is the resolved configuration after applying options.
// Public so subpackage parsers and encoders can read it via ResolveOptions.
type Config struct {
	Conformance     Conformance
	Positions       *PositionMap // non-nil if WithPositions was supplied
	positionsOut    **PositionMap
	CustomOperators map[Operator]bool
	DateTimezone    *time.Location
	TextStyle       TextStyle
	// MaxDepth caps expression nesting in both Text and JSON parsers.
	// A value of 0 means use DefaultMaxDepth; a negative value disables
	// the limit (not recommended for untrusted input).
	MaxDepth int
}

// DefaultMaxDepth is the default upper bound on expression nesting depth.
// Adversarial inputs can otherwise drive parser recursion until the Go
// runtime grows the goroutine stack to its hard limit.
const DefaultMaxDepth = 256

// DefaultConfig returns the default resolved config.
func DefaultConfig() *Config {
	return &Config{
		Conformance:  ConfAll,
		DateTimezone: time.UTC,
		TextStyle:    StyleNormal,
		MaxDepth:     DefaultMaxDepth,
	}
}

// ResolveOptions applies opts in order to a default Config and returns it.
// Subpackages call this at the top of their Parse/Encode entry points.
//
// If WithPositions was supplied, ResolveOptions allocates a fresh
// *PositionMap and writes it through to the user-provided **PositionMap.
func ResolveOptions(opts ...Option) *Config {
	cfg := DefaultConfig()
	for _, opt := range opts {
		if opt != nil {
			opt(cfg)
		}
	}
	// If WithPositions(&pm) was supplied, allocate the PositionMap now and
	// write its address back through the user's pointer.
	if cfg.Positions == nil && cfg.positionsOut != nil {
		pm := &PositionMap{}
		cfg.Positions = pm
		*cfg.positionsOut = pm
	}
	return cfg
}

// WithConformance restricts accepted features to the given conformance set.
func WithConformance(c Conformance) Option {
	return func(cfg *Config) { cfg.Conformance = c }
}

// WithPositions requests source-position tracking. The parser allocates a
// PositionMap and writes its address through out (if non-nil).
func WithPositions(out **PositionMap) Option {
	return func(cfg *Config) {
		if out != nil {
			cfg.positionsOut = out
		} else {
			// User just wants positions tracked internally.
			cfg.Positions = &PositionMap{}
		}
	}
}

// WithCustomOperators registers extra accepted operator names.
func WithCustomOperators(ops ...Operator) Option {
	return func(cfg *Config) {
		if cfg.CustomOperators == nil {
			cfg.CustomOperators = make(map[Operator]bool, len(ops))
		}
		for _, op := range ops {
			cfg.CustomOperators[op] = true
		}
	}
}

// WithDateTimezone sets the timezone used to interpret bare DATE literals.
// Affects both CQL2-Text DATE('YYYY-MM-DD') and CQL2-JSON {"date":"..."} parsing,
// including DATE-shaped INTERVAL endpoints. The default is time.UTC.
func WithDateTimezone(loc *time.Location) Option {
	return func(cfg *Config) {
		if loc != nil {
			cfg.DateTimezone = loc
		}
	}
}

// WithTextStyle sets the text-encoding style.
func WithTextStyle(s TextStyle) Option {
	return func(cfg *Config) { cfg.TextStyle = s }
}

// WithMaxDepth sets the maximum expression nesting depth accepted by Parse.
// Pass 0 to fall back to DefaultMaxDepth, or a negative value to disable
// the limit (only safe for trusted input).
func WithMaxDepth(d int) Option {
	return func(cfg *Config) { cfg.MaxDepth = d }
}
