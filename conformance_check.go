package cql2

// CheckConformance walks root and returns the first *ConformanceError for any
// node whose required conformance class is not satisfied by cfg, or nil if the
// whole tree is permitted. Both the text and JSON codecs call this after a
// successful parse; callers building ASTs by hand can invoke it directly.
//
// A nil cfg, or one whose Conformance is ConfAll, permits everything.
func CheckConformance(root Node, cfg *Config) error {
	if cfg == nil || cfg.Conformance == ConfAll {
		return nil
	}
	return walkConformance(root, cfg)
}

func walkConformance(n Node, cfg *Config) error {
	switch x := n.(type) {
	case *Op:
		// A custom operator bypasses conformance gating entirely.
		if !cfg.CustomOperators[x.Op] {
			if req := RequiredFor(x.Op); req != 0 && !cfg.Conformance.Has(req) {
				return mkConfErr(req, cfg, HumanFeatureName(x.Op), x)
			}
			// S_INTERSECTS with a non-Point/non-BBox literal needs
			// basic-spatial-functions-plus (ConfSpatial implies it).
			if RequiresBasicSpatialPlus(x.Op, x.Args) &&
				!cfg.Conformance.Has(ConfBasicSpatialPlus) &&
				!cfg.Conformance.Has(ConfSpatial) {
				return mkConfErr(ConfBasicSpatialPlus, cfg,
					"S_INTERSECTS with non-Point geometry literal", x)
			}
			// Property-property: a predicate whose operand shape deviates from
			// Basic CQL2's "property on left, literal on right", or an arithmetic
			// op over two property references.
			if (RequiresPropertyPropertyShape(x.Op, x.Args) || arithmeticBothProps(x.Op, x.Args)) &&
				!cfg.Conformance.Has(ConfPropertyProperty) {
				return mkConfErr(ConfPropertyProperty, cfg,
					HumanFeatureName(FeaturePropertyProperty), x)
			}
		}
	case *FunctionCall:
		if req := RequiredFor(FeatureFunction); req != 0 && !cfg.Conformance.Has(req) {
			return mkConfErr(req, cfg, HumanFeatureName(FeatureFunction), x)
		}
	}
	for _, c := range Children(n) {
		if err := walkConformance(c, cfg); err != nil {
			return err
		}
	}
	return nil
}

// arithmeticBothProps reports whether op is arithmetic and applied to exactly
// two property references (e.g. `a + b`). The op itself is gated by
// ConfArithmetic; the property-property nature of its operands by
// ConfPropertyProperty.
func arithmeticBothProps(op Operator, args []Node) bool {
	if !opTable[op].arith || len(args) != 2 {
		return false
	}
	_, lp := args[0].(*PropertyRef)
	_, rp := args[1].(*PropertyRef)
	return lp && rp
}

func mkConfErr(req Conformance, cfg *Config, feature string, n Node) error {
	var at Pos
	if cfg != nil && cfg.Positions != nil {
		if p, ok := cfg.Positions.Get(n); ok {
			at = p
		}
	}
	return &ConformanceError{
		Required: req,
		Active:   cfg.Conformance,
		Feature:  feature,
		At:       at,
	}
}
