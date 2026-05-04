package text

import (
	cql2 "github.com/exergy-dev/go-cql2"
)

// checkConformance walks the AST and returns the first *cql2.ConformanceError
// for any node whose required conformance class is not satisfied by cfg.
func checkConformance(root cql2.Node, cfg *cql2.Config) error {
	if cfg == nil || cfg.Conformance == cql2.ConfAll {
		return nil
	}
	return walkCheck(root, cfg)
}

func walkCheck(n cql2.Node, cfg *cql2.Config) error {
	if n == nil {
		return nil
	}
	switch x := n.(type) {
	case *cql2.Op:
		// Custom operator allowance bypasses conformance gating.
		if !cfg.CustomOperators[x.Op] {
			req := cql2.RequiredFor(x.Op)
			if req != 0 && !cfg.Conformance.Has(req) {
				return mkConfErr(req, cfg, cql2.HumanFeatureName(x.Op), x)
			}
			// S_INTERSECTS with a non-Point/non-BBox literal requires
			// basic-spatial-functions-plus on top of basic-spatial-functions.
			// ConfSpatial implies the plus class.
			if cql2.RequiresBasicSpatialPlus(x.Op, x.Args) &&
				!cfg.Conformance.Has(cql2.ConfBasicSpatialPlus) &&
				!cfg.Conformance.Has(cql2.ConfSpatial) {
				return mkConfErr(cql2.ConfBasicSpatialPlus, cfg,
					"S_INTERSECTS with non-Point geometry literal", x)
			}
			// Property-property: any comparison OR arithmetic op whose two
			// args are both *cql2.PropertyRef. The spec's property-property
			// class also covers nested cases like `a + 1 > b`; this conservative
			// check catches the direct cases that the parser generates today.
			// Property-property: predicates whose operand shape deviates
			// from Basic CQL2's "property on left, literal on right" rule.
			// Arithmetic LHS/RHS is gated by ConfArithmetic, not here.
			if cql2.RequiresPropertyPropertyShape(x.Op, x.Args) ||
				(arithmeticBothProps(x.Op, x.Args)) {
				if !cfg.Conformance.Has(cql2.ConfPropertyProperty) {
					return mkConfErr(cql2.ConfPropertyProperty, cfg,
						cql2.HumanFeatureName(cql2.FeaturePropertyProperty), x)
				}
			}
		}
		for _, a := range x.Args {
			if err := walkCheck(a, cfg); err != nil {
				return err
			}
		}
	case *cql2.FunctionCall:
		req := cql2.RequiredFor(cql2.FeatureFunction)
		if req != 0 && !cfg.Conformance.Has(req) {
			return mkConfErr(req, cfg, cql2.HumanFeatureName(cql2.FeatureFunction), x)
		}
		for _, a := range x.Args {
			if err := walkCheck(a, cfg); err != nil {
				return err
			}
		}
	case *cql2.ArrayLit:
		for _, a := range x.Elements {
			if err := walkCheck(a, cfg); err != nil {
				return err
			}
		}
	}
	return nil
}

// arithmeticBothProps catches the legacy property-property case where an
// arithmetic op is applied to two property references (e.g. `a + b`). The
// arithmetic op itself is gated by ConfArithmetic; the prop-prop nature of
// its operands is gated by ConfPropertyProperty.
func arithmeticBothProps(op cql2.Operator, args []cql2.Node) bool {
	switch op {
	case cql2.OpAdd, cql2.OpSub, cql2.OpMul, cql2.OpDiv,
		cql2.OpMod, cql2.OpPow, cql2.OpIDiv:
	default:
		return false
	}
	if len(args) != 2 {
		return false
	}
	_, lp := args[0].(*cql2.PropertyRef)
	_, rp := args[1].(*cql2.PropertyRef)
	return lp && rp
}

func mkConfErr(req cql2.Conformance, cfg *cql2.Config, feature string, n cql2.Node) error {
	at := cql2.Pos{}
	if cfg != nil && cfg.Positions != nil {
		if p, ok := cfg.Positions.Get(n); ok {
			at = p
		}
	}
	return &cql2.ConformanceError{
		Required: req,
		Active:   cfg.Conformance,
		Feature:  feature,
		At:       at,
	}
}
