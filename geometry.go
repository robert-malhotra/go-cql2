package cql2

import (
	"github.com/exergy-dev/go-topology-suite/geojson"
	"github.com/exergy-dev/go-topology-suite/geom"
)

// Geometry is a polymorphic GeoJSON wrapper around a go-topology-suite
// geom.Geometry that satisfies encoding/json's Marshaler and Unmarshaler.
//
// Go's encoding/json cannot decode directly into a sealed interface — there
// is no sum-type discriminator hook — so callers wanting json.Unmarshal of
// an arbitrary GeoJSON shape embed this type instead:
//
//	type Record struct {
//	    ID   int           `json:"id"`
//	    Geom cql2.Geometry `json:"geom"`
//	}
//
//	var r Record
//	json.Unmarshal(data, &r)
//	switch g := r.Geom.G.(type) {
//	case *geom.Point:      ...
//	case *geom.LineString: ...
//	}
//
// Encoding and decoding go through gts's geojson package; encoder options
// (precision, forced ring orientation) and CRS attachment are not available
// through encoding/json's argument-less methods. Callers needing those
// should call geojson.Marshal / geojson.UnmarshalWithCRS directly.
//
// The zero value's MarshalJSON emits "null"; a JSON null UnmarshalJSON
// clears G to nil rather than erroring.
type Geometry struct {
	G geom.Geometry
}

// MarshalJSON implements json.Marshaler.
func (w Geometry) MarshalJSON() ([]byte, error) {
	if w.G == nil {
		return []byte("null"), nil
	}
	return geojson.Marshal(w.G)
}

// UnmarshalJSON implements json.Unmarshaler.
func (w *Geometry) UnmarshalJSON(data []byte) error {
	if string(data) == "null" {
		w.G = nil
		return nil
	}
	g, err := geojson.Unmarshal(data)
	if err != nil {
		return err
	}
	w.G = g
	return nil
}
