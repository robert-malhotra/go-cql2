package cql2

import "testing"

// allOps lists every Op* constant. It exists so TestOpTableExhaustive fails
// loudly when an operator is added without a corresponding opTable row.
var allOps = []Operator{
	OpAnd, OpOr, OpNot,
	OpEq, OpNeq, OpLt, OpLte, OpGt, OpGte,
	OpLike, OpBetween, OpIn, OpIsNull,
	OpAdd, OpSub, OpMul, OpDiv, OpMod, OpPow, OpIDiv,
	OpSIntersects, OpSEquals, OpSDisjoint, OpSTouches,
	OpSWithin, OpSOverlaps, OpSCrosses, OpSContains,
	OpTAfter, OpTBefore, OpTContains, OpTDisjoint,
	OpTDuring, OpTEquals, OpTFinishedBy, OpTFinishes,
	OpTIntersects, OpTMeets, OpTMetBy, OpTOverlappedBy,
	OpTOverlaps, OpTStartedBy, OpTStarts,
	OpAContains, OpAContainedBy, OpAEquals, OpAOverlaps,
	OpCaseI, OpAccentI,
}

func TestOpTableExhaustive(t *testing.T) {
	if len(opTable) != len(allOps) {
		t.Fatalf("opTable has %d rows, allOps lists %d operators", len(opTable), len(allOps))
	}
	for _, op := range allOps {
		m, ok := opTable[op]
		if !ok {
			t.Errorf("operator %q missing from opTable", op)
			continue
		}
		if m.conf == 0 {
			t.Errorf("operator %q has no conformance class", op)
		}
		if m.arity == 0 {
			t.Errorf("operator %q has no arity rule", op)
		}
		if humanOpName(op) == "" {
			t.Errorf("operator %q has empty human name", op)
		}
	}
}
