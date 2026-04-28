package json

import (
	cql2 "github.com/example/go-cql2"
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
		if !cfg.CustomOperators[x.Op] {
			req := cql2.RequiredFor(x.Op)
			if req != 0 && !cfg.Conformance.Has(req) {
				return mkConfErr(req, cfg, cql2.HumanFeatureName(x.Op), x)
			}
			if isComparison(x.Op) && len(x.Args) == 2 {
				_, lp := x.Args[0].(*cql2.PropertyRef)
				_, rp := x.Args[1].(*cql2.PropertyRef)
				if lp && rp {
					if !cfg.Conformance.Has(cql2.ConfPropertyProperty) {
						return mkConfErr(cql2.ConfPropertyProperty, cfg,
							cql2.HumanFeatureName(cql2.FeaturePropertyProperty), x)
					}
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

func isComparison(op cql2.Operator) bool {
	switch op {
	case cql2.OpEq, cql2.OpNeq, cql2.OpLt, cql2.OpLte, cql2.OpGt, cql2.OpGte:
		return true
	}
	return false
}

func mkConfErr(req cql2.Conformance, cfg *cql2.Config, feature string, n cql2.Node) error {
	at := cql2.Pos{}
	if cfg != nil && cfg.Positions != nil {
		if p, ok := cfg.Positions.Of(n); ok {
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
