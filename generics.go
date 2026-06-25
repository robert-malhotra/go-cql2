package cql2

// mapSlice returns a new slice holding f applied to each element of s.
// It centralizes the make-then-loop pattern used by the builder and Clone.
func mapSlice[T, U any](s []T, f func(T) U) []U {
	out := make([]U, len(s))
	for i, v := range s {
		out[i] = f(v)
	}
	return out
}

// clonePtr returns a pointer to a shallow copy of *x. Used by Clone for the
// scalar literal node types whose fields are all value types.
func clonePtr[T any](x *T) *T {
	c := *x
	return &c
}
