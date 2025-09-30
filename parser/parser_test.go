package cql2text

import (
	"encoding/json"
	"testing"

	"github.com/planetlabs/go-ogc/filter"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestParse(t *testing.T) {
	tests := []struct {
		name     string
		cqlText  string
		expected string
	}{
		{
			name:     "simple equals comparison",
			cqlText:  "property = 'value'",
			expected: `{"op":"=","args":[{"property":"property"},"value"]}`,
		},
		{
			name:     "numeric comparison",
			cqlText:  "population > 1000000",
			expected: `{"op":">","args":[{"property":"population"},1000000]}`,
		},
		{
			name:     "boolean equality",
			cqlText:  "is_active = TRUE",
			expected: `{"op":"=","args":[{"property":"is_active"},true]}`,
		},
		{
			name:     "and expression",
			cqlText:  "city = 'New York' AND population > 8000000",
			expected: `{"op":"and","args":[{"op":"=","args":[{"property":"city"},"New York"]},{"op":">","args":[{"property":"population"},8000000]}]}`,
		},
		{
			name:     "or expression",
			cqlText:  "state = 'CA' OR state = 'NY'",
			expected: `{"op":"or","args":[{"op":"=","args":[{"property":"state"},"CA"]},{"op":"=","args":[{"property":"state"},"NY"]}]}`,
		},
		{
			name:     "not expression",
			cqlText:  "NOT (population < 1000)",
			expected: `{"op":"not","args":[{"op":"<","args":[{"property":"population"},1000]}]}`,
		},
		{
			name:     "like expression",
			cqlText:  "name LIKE 'San%'",
			expected: `{"op":"like","args":[{"property":"name"},"San%"]}`,
		},
		{
			name:     "like expression with CASEI as function",
			cqlText:  "name LIKE CASEI('san%')",
			expected: `{"op":"like","args":[{"property":"name"},{"op":"casei","args":["san%"]}]}`,
		},
		{
			name:     "between expression",
			cqlText:  "population BETWEEN 10000 AND 50000",
			expected: `{"op":"between","args":[{"property":"population"},10000,50000]}`,
		},
		{
			name:     "in expression",
			cqlText:  "state IN ('CA', 'NY', 'TX')",
			expected: `{"op":"in","args":[{"property":"state"},["CA","NY","TX"]]}`,
		},
		{
			name:     "case insensitive function",
			cqlText:  "CASEI(road_class) IN (CASEI('Οδος'), CASEI('Straße'))",
			expected: `{"op":"in","args":[{"op":"casei","args":[{"property":"road_class"}]},[{"op":"casei","args":["Οδος"]},{"op":"casei","args":["Straße"]}]]}`,
		},
		{
			name:     "case insensitive property comparison",
			cqlText:  "CASEI(city) = 'New York'",
			expected: `{"op":"=","args":[{"op":"casei","args":[{"property":"city"}]},"New York"]}`,
		},
		{
			name:     "case insensitive literal in comparison",
			cqlText:  "city = CASEI('New York')",
			expected: `{"op":"=","args":[{"property":"city"},{"op":"casei","args":["New York"]}]}`,
		},
		{
			name:     "is null",
			cqlText:  "geometry IS NULL",
			expected: `{"op":"isNull","args":[{"property":"geometry"}]}`,
		},
		{
			name:     "function call",
			cqlText:  "distance(point, city) < 10",
			expected: `{"op":"<","args":[{"op":"distance","args":[{"property":"point"},{"property":"city"}]},10]}`,
		},
		{
			name:     "spatial predicate",
			cqlText:  "S_INTERSECTS(geometry, user_polygon)",
			expected: `{"op":"s_intersects","args":[{"property":"geometry"},{"property":"user_polygon"}]}`,
		},
		{
			name:     "temporal predicate",
			cqlText:  "T_AFTER(date, '2020-01-01')",
			expected: `{"op":"t_after","args":[{"property":"date"},{"date":"2020-01-01"}]}`,
		},
		{
			name:     "array predicate",
			cqlText:  "A_CONTAINS(tags, ['forest', 'park'])",
			expected: `{"op":"a_contains","args":[{"property":"tags"},["forest","park"]]}`,
		},
		{
			name:     "complex nested expression",
			cqlText:  "(city = 'New York' OR city = 'Los Angeles') AND (population > 1000000 OR area > 500)",
			expected: `{"op":"and","args":[{"op":"or","args":[{"op":"=","args":[{"property":"city"},"New York"]},{"op":"=","args":[{"property":"city"},"Los Angeles"]}]},{"op":"or","args":[{"op":">","args":[{"property":"population"},1000000]},{"op":">","args":[{"property":"area"},500]}]}]}`,
		},
		{
			name:     "spatial relationship between a property and a function",
			cqlText:  "S_WITHIN(road,Buffer(geometry,10,'m'))",
			expected: `{"op":"s_within","args":[{"property":"road"},{"op":"Buffer","args":[{"property":"geometry"},10,"m"]}]}`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			filter, err := Parse(tt.cqlText)
			require.NoError(t, err)

			actual, err := json.Marshal(filter)
			require.NoError(t, err)

			assert.JSONEq(t, tt.expected, string(actual))
		})
	}
}

func TestParseErrors(t *testing.T) {
	tests := []struct {
		name    string
		cqlText string
	}{
		{
			name:    "empty expression",
			cqlText: "",
		},
		{
			name:    "invalid syntax",
			cqlText: "property = = value",
		},
		{
			name:    "mismatched parentheses",
			cqlText: "(city = 'New York'",
		},
		{
			name:    "missing comparison operator",
			cqlText: "property value",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := Parse(tt.cqlText)
			assert.Error(t, err)
		})
	}
}

// Test additional features not covered by basic tests
func TestAdvancedFeatures(t *testing.T) {
	// Test time interval
	f, err := Parse("T_DURING(timestamp, ['2020-01-01', '2020-12-31'])")
	require.NoError(t, err)

	temporal, ok := f.Expression.(*filter.TemporalComparison)
	require.True(t, ok)
	assert.Equal(t, filter.TimeDuring, temporal.Name)

	interval, ok := temporal.Right.(*filter.Interval)
	require.True(t, ok)
	assert.NotNil(t, interval.Start)
	assert.NotNil(t, interval.End)

	// Test float numbers
	f, err = Parse("elevation > 123.45")
	require.NoError(t, err)

	comp, ok := f.Expression.(*filter.Comparison)
	require.True(t, ok)

	num, ok := comp.Right.(*filter.Number)
	require.True(t, ok)
	assert.Equal(t, 123.45, num.Value)

	// Test CASEI function
	f, err = Parse("CASEI(road_class) IN (CASEI('Οδος'), CASEI('Straße'))")
	require.NoError(t, err)

	inOp, ok := f.Expression.(*filter.In)
	require.True(t, ok)

	// Check that the left side is a CASEI function wrapping road_class
	caseiFunc, ok := inOp.Item.(*filter.Function)
	require.True(t, ok)
	assert.Equal(t, "casei", caseiFunc.Op)
	require.Len(t, caseiFunc.Args, 1)

	prop, ok := caseiFunc.Args[0].(*filter.Property)
	require.True(t, ok)
	assert.Equal(t, "road_class", prop.Name)

	// Check that the right side contains two CASEI functions
	require.Len(t, inOp.List, 2)

	caseiItem1, ok := inOp.List[0].(*filter.Function)
	require.True(t, ok)
	assert.Equal(t, "casei", caseiItem1.Op)
	require.Len(t, caseiItem1.Args, 1)

	caseiItem2, ok := inOp.List[1].(*filter.Function)
	require.True(t, ok)
	assert.Equal(t, "casei", caseiItem2.Op)
	require.Len(t, caseiItem2.Args, 1)
}
