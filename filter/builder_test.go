package cqlbuilder_test

import (
	"encoding/json"
	"fmt"
	"testing"
	"time"

	"github.com/planetlabs/go-ogc/filter"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	cql "github.com/robert-malhotra/go-cql-text/filter"
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
	and, err := cql.And(true, false)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"and","args":[true,false]}`, asJSON(t, and))

	or, err := cql.Or(true, false)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"or","args":[true,false]}`, asJSON(t, or))

	not, err := cql.Not(true)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"not","args":[true]}`, asJSON(t, not))
}

func TestLogicalErrors(t *testing.T) {
	_, err := cql.Or(123) // invalid boolean arg
	require.Error(t, err)
}

// ---------- comparisons ----------

func TestComparisonOperatorsNumbersAndInts(t *testing.T) {
	eq, err := cql.Eq(cql.Property("a"), 1)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"=","args":[{"property":"a"},1]}`, asJSON(t, eq))

	ne, err := cql.Ne(cql.Property("a"), int64(2))
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"<>","args":[{"property":"a"},2]}`, asJSON(t, ne))

	lt, err := cql.Lt(cql.Property("a"), 1.5)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"<","args":[{"property":"a"},1.5]}`, asJSON(t, lt))

	le, err := cql.Le(cql.Property("a"), 2)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"<=","args":[{"property":"a"},2]}`, asJSON(t, le))

	gt, err := cql.Gt(cql.Property("a"), 3)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":">","args":[{"property":"a"},3]}`, asJSON(t, gt))

	ge, err := cql.Ge(cql.Property("a"), 4)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":">=","args":[{"property":"a"},4]}`, asJSON(t, ge))
}

func TestComparisonErrors(t *testing.T) {
	_, err := cql.Eq(cql.Property("p"), struct{ X int }{1})
	require.Error(t, err)
}

// ---------- between / in ----------

func TestBetweenAndInHappyAndError(t *testing.T) {
	bt, err := cql.Between(cql.Property("p"), 1, 3.5)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"between","args":[{"property":"p"},1,3.5]}`, asJSON(t, bt))

	in, err := cql.In(cql.Property("state"), "CA", "NY", "TX")
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"in","args":[{"property":"state"},["CA","NY","TX"]]}`, asJSON(t, in))

	// errors
	_, err = cql.Between(cql.Property("p"), "x", 10)
	require.Error(t, err)
	_, err = cql.Between(cql.Property("p"), 1, "y")
	require.Error(t, err)
	_, err = cql.In(cql.Property("p"), "ok", struct{ X int }{1})
	require.Error(t, err)
	_, err = cql.In(struct{ Nope string }{"n"}, "a")
	require.Error(t, err)
}

// ---------- arrays & array comparisons ----------

func TestArrayLiteralAndErrors(t *testing.T) {
	arr, err := cql.Array("a", true, 3, 4.2)
	require.NoError(t, err)

	overlaps := cql.AOverlaps(cql.Property("tags"), arr)
	assert.JSONEq(t, `{"op":"a_overlaps","args":[{"property":"tags"},["a",true,3,4.2]]}`, asJSON(t, overlaps))

	empty, err := cql.Array()
	require.NoError(t, err)
	contains := cql.AContains(cql.Property("xs"), empty)
	assert.JSONEq(t, `{"op":"a_contains","args":[{"property":"xs"},[]]}`, asJSON(t, contains))

	a1, err := cql.Array(1, 2, 3)
	require.NoError(t, err)
	a2, err := cql.Array(1, 2, 3)
	require.NoError(t, err)

	assert.JSONEq(t, `{"op":"a_equals","args":[[1,2,3],[1,2,3]]}`, asJSON(t, cql.AEquals(a1, a2)))
	assert.JSONEq(t, `{"op":"a_containedBy","args":[[1,2,3],[1,2,3]]}`, asJSON(t, cql.AContainedBy(a1, a2)))

	_, err = cql.Array(struct{ A int }{1})
	require.Error(t, err)
}

// ---------- isNull ----------

func TestIsNullPaths(t *testing.T) {
	isn, err := cql.IsNull(cql.Property("geom"))
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"isNull","args":[{"property":"geom"}]}`, asJSON(t, isn))

	isn, err = cql.IsNull(123) // toExpression fails; toScalar succeeds
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"isNull","args":[123]}`, asJSON(t, isn))

	_, err = cql.IsNull(struct{ Z int }{9})
	require.Error(t, err)
}

// ---------- LIKE & pattern wrappers ----------

func TestLikeWithCaseAndAccentAndRawString(t *testing.T) {
	ci, err := cql.CaseInsensitive("foo%")
	require.NoError(t, err)
	like, err := cql.Like(cql.Property("name"), ci)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"like","args":[{"property":"name"},{"op":"casei","args":["foo%"]}]}`, asJSON(t, like))

	ai, err := cql.AccentInsensitive("bar%")
	require.NoError(t, err)
	like, err = cql.Like(cql.Property("name"), ai)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"like","args":[{"property":"name"},{"op":"accenti","args":["bar%"]}]}`, asJSON(t, like))

	like, err = cql.Like(cql.Property("city"), "San%")
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"like","args":[{"property":"city"},"San%"]}`, asJSON(t, like))
}

func TestLikePatternErrors(t *testing.T) {
	_, err := cql.Like(cql.Property("name"), 123) // invalid pattern type
	require.Error(t, err)
}

// ---------- functions ----------

func TestFuncSuccessAndError(t *testing.T) {
	f, err := cql.Func("distance", cql.Property("p1"), cql.Property("p2"))
	require.NoError(t, err)

	lt, err := cql.Lt(f, 10)
	require.NoError(t, err)
	assert.JSONEq(t, `{"op":"<","args":[{"op":"distance","args":[{"property":"p1"},{"property":"p2"}]},10]}`, asJSON(t, lt))

	_, err = cql.Func("badArgs", "ok", struct{ K string }{"x"}) // invalid later arg
	require.Error(t, err)
}

// ---------- spatial (all operators) ----------

func TestSpatialComparisons_All(t *testing.T) {
	type tc struct {
		name string
		op   func(left, right filter.SpatialExpression) *filter.SpatialComparison
		json string
	}
	tests := []tc{
		{"SContains", cql.SContains, `{"op":"s_contains","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SCrosses", cql.SCrosses, `{"op":"s_crosses","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SDisjoint", cql.SDisjoint, `{"op":"s_disjoint","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SEquals", cql.SEquals, `{"op":"s_equals","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SIntersects", cql.SIntersects, `{"op":"s_intersects","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SOverlaps", cql.SOverlaps, `{"op":"s_overlaps","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"STouches", cql.STouches, `{"op":"s_touches","args":[{"property":"g1"},{"property":"g2"}]}`},
		{"SWithin", cql.SWithin, `{"op":"s_within","args":[{"property":"g1"},{"property":"g2"}]}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.op(cql.Property("g1"), cql.Property("g2"))
			assert.JSONEq(t, tt.json, asJSON(t, got))
		})
	}

	// property vs geometry literal wrapper (just ensure it marshals)
	g := cql.Geometry(map[string]any{"type": "Point", "coordinates": []float64{1, 2}})
	got := cql.SWithin(cql.Property("geom"), g)
	_ = asJSON(t, got) // should not panic or error
}

// ---------- temporal (all operators) ----------

func TestTemporalComparisons_All(t *testing.T) {
	// Use two simple property operands for compact JSON checks.
	left := cql.Property("t1")
	right := cql.Property("t2")

	type tcase struct {
		name string
		op   func(filter.TemporalExpression, filter.TemporalExpression) *filter.TemporalComparison
		code string // op code in JSON
	}
	tests := []tcase{
		{"TAfter", cql.TAfter, "t_after"},
		{"TBefore", cql.TBefore, "t_before"},
		{"TContains", cql.TContains, "t_contains"},
		{"TDisjoint", cql.TDisjoint, "t_disjoint"},
		{"TDuring", cql.TDuring, "t_during"},
		{"TEquals", cql.TEquals, "t_equals"},
		{"TFinishedBy", cql.TFinishedBy, "t_finishedBy"},
		{"TFinishes", cql.TFinishes, "t_finishes"},
		{"TIntersects", cql.TIntersects, "t_intersects"},
		{"TMeets", cql.TMeets, "t_meets"},
		{"TMetBy", cql.TMetBy, "t_metBy"},
		{"TOverlappedBy", cql.TOverlappedBy, "t_overlappedBy"},
		{"TOverlaps", cql.TOverlaps, "t_overlaps"},
		{"TStartedBy", cql.TStartedBy, "t_startedBy"},
		{"TStarts", cql.TStarts, "t_starts"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.op(left, right)
			want := fmt.Sprintf(`{"op":"%s","args":[{"property":"t1"},{"property":"t2"}]}`, tt.code)
			assert.JSONEq(t, want, asJSON(t, got))
		})
	}

	// Interval variant for one op to cover Interval serialization
	start := cql.Date(time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC))
	end := cql.Timestamp(time.Date(2020, 12, 31, 12, 0, 0, 0, time.UTC))
	intv := cql.Interval(start, end)
	td := cql.TDuring(cql.Property("ts"), intv)
	require.NotNil(t, td)
	assert.JSONEq(t, `{"op":"t_during","args":[{"property":"ts"},{"interval":["2020-01-01","2020-12-31T12:00:00Z"]}]}`, asJSON(t, td))
}
