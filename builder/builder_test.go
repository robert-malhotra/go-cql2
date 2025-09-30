package cqlbuilder_test

import (
	"encoding/json"
	"fmt"
	"testing"
	"time"

	"github.com/planetlabs/go-ogc/filter"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	cql "github.com/robert-malhotra/go-cql-text/builder"
)

// ---------- helpers ----------

func asJSON(t *testing.T, be filter.BooleanExpression) string {
	t.Helper()
	f := &filter.Filter{Expression: be}
	b, err := json.Marshal(f)
	require.NoError(t, err)
	return string(b)
}

// ---------- logical ops ----------

func TestLogicalAndOrNotWithBools(t *testing.T) {
	and := cql.And(&filter.Boolean{Value: true}, &filter.Boolean{Value: false})
	assert.JSONEq(t, `{"op":"and","args":[true,false]}`, asJSON(t, and))

	or := cql.Or(&filter.Boolean{Value: true}, &filter.Boolean{Value: false})
	assert.JSONEq(t, `{"op":"or","args":[true,false]}`, asJSON(t, or))

	not := cql.Not(&filter.Boolean{Value: true})
	assert.JSONEq(t, `{"op":"not","args":[true]}`, asJSON(t, not))
}

// ---------- comparisons ----------

func TestComparisonOperatorsNumbersAndInts(t *testing.T) {
	eq := cql.Eq(cql.Property("a"), 1)
	assert.JSONEq(t, `{"op":"=","args":[{"property":"a"},1]}`, asJSON(t, eq))

	ne := cql.Ne(cql.Property("a"), int64(2))
	assert.JSONEq(t, `{"op":"<>","args":[{"property":"a"},2]}`, asJSON(t, ne))

	lt := cql.Lt(cql.Property("a"), 1.5)
	assert.JSONEq(t, `{"op":"<","args":[{"property":"a"},1.5]}`, asJSON(t, lt))

	le := cql.Le(cql.Property("a"), 2)
	assert.JSONEq(t, `{"op":"<=","args":[{"property":"a"},2]}`, asJSON(t, le))

	gt := cql.Gt(cql.Property("a"), 3)
	assert.JSONEq(t, `{"op":">","args":[{"property":"a"},3]}`, asJSON(t, gt))

	ge := cql.Ge(cql.Property("a"), 4)
	assert.JSONEq(t, `{"op":">=","args":[{"property":"a"},4]}`, asJSON(t, ge))
}

// ---------- between / in ----------

func TestBetweenAndIn(t *testing.T) {
	bt := cql.Between(cql.Property("p"), 1, 3.5)
	assert.JSONEq(t, `{"op":"between","args":[{"property":"p"},1,3.5]}`, asJSON(t, bt))

	inS := cql.In(cql.Property("state"), "CA", "NY", "TX")
	assert.JSONEq(t, `{"op":"in","args":[{"property":"state"},["CA","NY","TX"]]}`, asJSON(t, inS))

	inN := cql.In(cql.Property("scores"), 10, 20, 30)
	assert.JSONEq(t, `{"op":"in","args":[{"property":"scores"},[10,20,30]]}`, asJSON(t, inN))

	inB := cql.In(cql.Property("flags"), true, false)
	assert.JSONEq(t, `{"op":"in","args":[{"property":"flags"},[true,false]]}`, asJSON(t, inB))
}

// ---------- arrays & array comparisons ----------

func TestArrayLiteralsAndComparisons(t *testing.T) {
	arrStr := cql.Array("a", "b")
	arrNum := cql.Array(1, 2, 3)
	arrBool := cql.Array(true, false)

	// Mixed-type array: stitch together homogeneous chunks built via cql.Array.
	mixed := filter.Array{}
	mixed = append(mixed, cql.Array("z")...)
	mixed = append(mixed, cql.Array(true)...)
	mixed = append(mixed, cql.Array(4)...)
	mixed = append(mixed, cql.Array(4.2)...)

	overlaps := cql.ArrayCompare(cql.ArrayOverlaps, cql.Property("tags"), mixed)
	assert.JSONEq(t, `{"op":"a_overlaps","args":[{"property":"tags"},["z",true,4,4.2]]}`, asJSON(t, overlaps))

	empty := cql.Array[string]()
	contains := cql.ArrayCompare(cql.ArrayContains, cql.Property("xs"), empty)
	assert.JSONEq(t, `{"op":"a_contains","args":[{"property":"xs"},[]]}`, asJSON(t, contains))

	ae := cql.ArrayCompare(cql.ArrayEquals, arrNum, cql.Array(1, 2, 3))
	assert.JSONEq(t, `{"op":"a_equals","args":[[1,2,3],[1,2,3]]}`, asJSON(t, ae))

	acb := cql.ArrayCompare(cql.ArrayContainedBy, arrStr, cql.Array("a", "b", "c"))
	assert.JSONEq(t, `{"op":"a_containedBy","args":[["a","b"],["a","b","c"]]}`, asJSON(t, acb))

	_ = arrBool // keep coverage for bool array literal
}

// ---------- isNull ----------

func TestIsNullPaths(t *testing.T) {
	isn := cql.IsNullExpr(cql.Property("geom"))
	assert.JSONEq(t, `{"op":"isNull","args":[{"property":"geom"}]}`, asJSON(t, isn))

	isn2 := cql.IsNull(123)
	assert.JSONEq(t, `{"op":"isNull","args":[123]}`, asJSON(t, isn2))

	isn3 := cql.IsNull("abc")
	assert.JSONEq(t, `{"op":"isNull","args":["abc"]}`, asJSON(t, isn3))

	isn4 := cql.IsNull(true)
	assert.JSONEq(t, `{"op":"isNull","args":[true]}`, asJSON(t, isn4))
}

// ---------- LIKE & pattern wrappers ----------

func TestLikeWithCaseAndAccentAndRawString(t *testing.T) {
	ci := cql.CaseInsensitive("foo%")
	like := cql.Like(cql.Property("name"), ci)
	assert.JSONEq(t,
		`{"op":"like","args":[{"property":"name"},{"op":"casei","args":["foo%"]}]}`,
		asJSON(t, like),
	)

	ai := cql.AccentInsensitive("bar%")
	like = cql.Like(cql.Property("name"), ai)
	assert.JSONEq(t,
		`{"op":"like","args":[{"property":"name"},{"op":"accenti","args":["bar%"]}]}`,
		asJSON(t, like),
	)

	like = cql.Like(cql.Property("city"), &filter.String{Value: "San%"})
	assert.JSONEq(t,
		`{"op":"like","args":[{"property":"city"},"San%"]}`,
		asJSON(t, like),
	)
}

// ---------- functions (distance) ----------

func TestFunctionComparison(t *testing.T) {
	// Filter.Function commonly implements NumericExpression in go-ogc.
	fn := &filter.Function{
		Op:   "distance",
		Args: []filter.Expression{cql.Property("p1"), cql.Property("p2")},
	}
	lt := cql.Lt(fn, 10)
	assert.JSONEq(t, `{"op":"<","args":[{"op":"distance","args":[{"property":"p1"},{"property":"p2"}]},10]}`, asJSON(t, lt))
}

// ---------- spatial (all operators) ----------

func TestSpatialComparisons_All(t *testing.T) {
	tests := []struct {
		name string
		op   cql.SpatialOp
		json string
	}{
		{"SpatialContains", cql.SpatialContains, `{"op":"s_contains","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SpatialCrosses", cql.SpatialCrosses, `{"op":"s_crosses","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SpatialDisjoint", cql.SpatialDisjoint, `{"op":"s_disjoint","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SpatialEquals", cql.SpatialEquals, `{"op":"s_equals","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SpatialIntersects", cql.SpatialIntersects, `{"op":"s_intersects","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SpatialOverlaps", cql.SpatialOverlaps, `{"op":"s_overlaps","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SpatialTouches", cql.SpatialTouches, `{"op":"s_touches","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SpatialWithin", cql.SpatialWithin, `{"op":"s_within","args":[{"property":"g1"},{"property":"g2"}]}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := cql.Spatial(tt.op, cql.Property("g1"), cql.Property("g2"))
			assert.JSONEq(t, tt.json, asJSON(t, got))
		})
	}

	// property vs geometry literal wrapper (just ensure it marshals)
	g := cql.Geometry(map[string]any{"type": "Point", "coordinates": []float64{1, 2}})
	got := cql.Spatial(cql.SpatialWithin, cql.Property("geom"), g)
	_ = asJSON(t, got) // should not panic or error
}

// ---------- temporal (all operators) ----------

func TestTemporalComparisons_All(t *testing.T) {
	left := cql.Property("t1")
	right := cql.Property("t2")

	tests := []struct {
		name string
		op   cql.TemporalOp
		code string
	}{
		{"TemporalAfter", cql.TemporalAfter, "t_after"},
		{"TemporalBefore", cql.TemporalBefore, "t_before"},
		{"TemporalContains", cql.TemporalContains, "t_contains"},
		{"TemporalDisjoint", cql.TemporalDisjoint, "t_disjoint"},
		{"TemporalDuring", cql.TemporalDuring, "t_during"},
		{"TemporalEquals", cql.TemporalEquals, "t_equals"},
		{"TemporalFinishedBy", cql.TemporalFinishedBy, "t_finishedBy"},
		{"TemporalFinishes", cql.TemporalFinishes, "t_finishes"},
		{"TemporalIntersects", cql.TemporalIntersects, "t_intersects"},
		{"TemporalMeets", cql.TemporalMeets, "t_meets"},
		{"TemporalMetBy", cql.TemporalMetBy, "t_metBy"},
		{"TemporalOverlappedBy", cql.TemporalOverlappedBy, "t_overlappedBy"},
		{"TemporalOverlaps", cql.TemporalOverlaps, "t_overlaps"},
		{"TemporalStartedBy", cql.TemporalStartedBy, "t_startedBy"},
		{"TemporalStarts", cql.TemporalStarts, "t_starts"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := cql.Temporal(tt.op, left, right)
			want := fmt.Sprintf(`{"op":"%s","args":[{"property":"t1"},{"property":"t2"}]}`, tt.code)
			assert.JSONEq(t, want, asJSON(t, got))
		})
	}

	// Interval variant for one op to cover Interval serialization
	start := cql.Date(time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC))
	end := cql.Timestamp(time.Date(2020, 12, 31, 12, 0, 0, 0, time.UTC))
	intv := cql.Interval(start, end)
	td := cql.Temporal(cql.TemporalDuring, cql.Property("ts"), intv)
	assert.JSONEq(t, `{"op":"t_during","args":[{"property":"ts"},{"interval":["2020-01-01","2020-12-31T12:00:00Z"]}]}`, asJSON(t, td))
}
