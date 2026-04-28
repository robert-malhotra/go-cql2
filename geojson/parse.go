package geojson

// GeoJSON coordinate components are decoded as float64. RFC 7946 §3.1.1 permits
// any JSON number; we deliberately do not preserve the original lexical form
// (json.Number) here — that's a documented simplification per the design.

import (
	"bytes"
	"encoding/json"
	"fmt"

	cql2 "github.com/example/go-cql2"
)

// Parse decodes a GeoJSON geometry document into a cql2.Geometry.
//
// Supported types: Point, LineString, Polygon, MultiPoint, MultiLineString,
// MultiPolygon, GeometryCollection. A top-level "bbox" member is permitted by
// RFC 7946 and is silently ignored. Foreign members on the geometry object are
// also ignored.
//
// Errors are *cql2.GeometryError with Encoding=EncodingJSON and a JSON-Pointer
// path identifying the offending location.
func Parse(data []byte) (cql2.Geometry, error) {
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.UseNumber()
	var raw map[string]json.RawMessage
	if err := dec.Decode(&raw); err != nil {
		return nil, gerr("", fmt.Sprintf("invalid JSON: %v", err))
	}
	return parseGeometry(raw, "")
}

// gerr builds a *cql2.GeometryError with a JSON-pointer path.
func gerr(path, msg string) error {
	return &cql2.GeometryError{
		Encoding: cql2.EncodingJSON,
		At:       cql2.Pos{JSONPath: path},
		Msg:      msg,
	}
}

// parseGeometry dispatches on the "type" field. path is the JSON-pointer to
// the current geometry object (empty for the root).
func parseGeometry(obj map[string]json.RawMessage, path string) (cql2.Geometry, error) {
	rawType, ok := obj["type"]
	if !ok {
		return nil, gerr(path, "missing required member \"type\"")
	}
	var typ string
	if err := json.Unmarshal(rawType, &typ); err != nil {
		return nil, gerr(path+"/type", "\"type\" must be a string")
	}
	switch typ {
	case "Point":
		return parsePoint(obj, path)
	case "LineString":
		return parseLineString(obj, path)
	case "Polygon":
		return parsePolygon(obj, path)
	case "MultiPoint":
		return parseMultiPoint(obj, path)
	case "MultiLineString":
		return parseMultiLineString(obj, path)
	case "MultiPolygon":
		return parseMultiPolygon(obj, path)
	case "GeometryCollection":
		return parseGeometryCollection(obj, path)
	default:
		return nil, gerr(path+"/type", fmt.Sprintf("unknown geometry type %q", typ))
	}
}

func requireCoords(obj map[string]json.RawMessage, path string) (json.RawMessage, error) {
	c, ok := obj["coordinates"]
	if !ok {
		return nil, gerr(path, "missing required member \"coordinates\"")
	}
	// RFC 7946 doesn't allow null Point coords; we reject explicit null at any level.
	if isJSONNull(c) {
		return nil, gerr(path+"/coordinates", "\"coordinates\" must not be null")
	}
	return c, nil
}

func isJSONNull(raw json.RawMessage) bool {
	return bytes.Equal(bytes.TrimSpace(raw), []byte("null"))
}

// parseCoord decodes a single position [x, y] or [x, y, z]. path points to the
// position array.
func parseCoord(raw json.RawMessage, path string) (cql2.Coord, error) {
	var arr []json.RawMessage
	if err := json.Unmarshal(raw, &arr); err != nil {
		return cql2.Coord{}, gerr(path, "position must be an array of numbers")
	}
	if len(arr) < 2 {
		return cql2.Coord{}, gerr(path, fmt.Sprintf("position must have at least 2 elements, got %d", len(arr)))
	}
	x, err := parseNumber(arr[0], fmt.Sprintf("%s/0", path))
	if err != nil {
		return cql2.Coord{}, err
	}
	y, err := parseNumber(arr[1], fmt.Sprintf("%s/1", path))
	if err != nil {
		return cql2.Coord{}, err
	}
	c := cql2.Coord{X: x, Y: y}
	if len(arr) >= 3 {
		z, err := parseNumber(arr[2], fmt.Sprintf("%s/2", path))
		if err != nil {
			return cql2.Coord{}, err
		}
		c.Z = z
		c.HasZ = true
	}
	return c, nil
}

func parseNumber(raw json.RawMessage, path string) (float64, error) {
	var n json.Number
	if err := json.Unmarshal(raw, &n); err != nil {
		return 0, gerr(path, "coordinate component must be a number")
	}
	f, err := n.Float64()
	if err != nil {
		return 0, gerr(path, fmt.Sprintf("invalid number %q", n.String()))
	}
	return f, nil
}

// parseCoordList decodes [pos, pos, ...].
func parseCoordList(raw json.RawMessage, path string) ([]cql2.Coord, error) {
	var arr []json.RawMessage
	if err := json.Unmarshal(raw, &arr); err != nil {
		return nil, gerr(path, "expected array of positions")
	}
	out := make([]cql2.Coord, len(arr))
	for i, item := range arr {
		c, err := parseCoord(item, fmt.Sprintf("%s/%d", path, i))
		if err != nil {
			return nil, err
		}
		out[i] = c
	}
	return out, nil
}

// parseRingList decodes [[pos,pos,...], ...] as polygon rings (closed, ≥4 pts).
func parseRingList(raw json.RawMessage, path string) ([][]cql2.Coord, error) {
	var arr []json.RawMessage
	if err := json.Unmarshal(raw, &arr); err != nil {
		return nil, gerr(path, "expected array of rings")
	}
	rings := make([][]cql2.Coord, len(arr))
	for i, item := range arr {
		ringPath := fmt.Sprintf("%s/%d", path, i)
		ring, err := parseCoordList(item, ringPath)
		if err != nil {
			return nil, err
		}
		if len(ring) < 4 {
			return nil, gerr(ringPath, fmt.Sprintf("polygon ring must have at least 4 positions, got %d", len(ring)))
		}
		first, last := ring[0], ring[len(ring)-1]
		if first.X != last.X || first.Y != last.Y || first.HasZ != last.HasZ || (first.HasZ && first.Z != last.Z) {
			return nil, gerr(ringPath, "polygon ring is not closed (first and last positions differ)")
		}
		rings[i] = ring
	}
	return rings, nil
}

func parsePoint(obj map[string]json.RawMessage, path string) (cql2.Geometry, error) {
	rawCoords, err := requireCoords(obj, path)
	if err != nil {
		return nil, err
	}
	c, err := parseCoord(rawCoords, path+"/coordinates")
	if err != nil {
		return nil, err
	}
	return &cql2.Point{Coord: c}, nil
}

func parseLineString(obj map[string]json.RawMessage, path string) (cql2.Geometry, error) {
	rawCoords, err := requireCoords(obj, path)
	if err != nil {
		return nil, err
	}
	coords, err := parseCoordList(rawCoords, path+"/coordinates")
	if err != nil {
		return nil, err
	}
	if len(coords) < 2 {
		return nil, gerr(path+"/coordinates", fmt.Sprintf("LineString must have at least 2 positions, got %d", len(coords)))
	}
	return &cql2.LineString{Coords: coords}, nil
}

func parsePolygon(obj map[string]json.RawMessage, path string) (cql2.Geometry, error) {
	rawCoords, err := requireCoords(obj, path)
	if err != nil {
		return nil, err
	}
	rings, err := parseRingList(rawCoords, path+"/coordinates")
	if err != nil {
		return nil, err
	}
	return &cql2.Polygon{Rings: rings}, nil
}

func parseMultiPoint(obj map[string]json.RawMessage, path string) (cql2.Geometry, error) {
	rawCoords, err := requireCoords(obj, path)
	if err != nil {
		return nil, err
	}
	coords, err := parseCoordList(rawCoords, path+"/coordinates")
	if err != nil {
		return nil, err
	}
	pts := make([]cql2.Point, len(coords))
	for i, c := range coords {
		pts[i] = cql2.Point{Coord: c}
	}
	return &cql2.MultiPoint{Points: pts}, nil
}

func parseMultiLineString(obj map[string]json.RawMessage, path string) (cql2.Geometry, error) {
	rawCoords, err := requireCoords(obj, path)
	if err != nil {
		return nil, err
	}
	var arr []json.RawMessage
	if err := json.Unmarshal(rawCoords, &arr); err != nil {
		return nil, gerr(path+"/coordinates", "expected array of line strings")
	}
	lines := make([]cql2.LineString, len(arr))
	for i, item := range arr {
		linePath := fmt.Sprintf("%s/coordinates/%d", path, i)
		coords, err := parseCoordList(item, linePath)
		if err != nil {
			return nil, err
		}
		if len(coords) < 2 {
			return nil, gerr(linePath, fmt.Sprintf("LineString must have at least 2 positions, got %d", len(coords)))
		}
		lines[i] = cql2.LineString{Coords: coords}
	}
	return &cql2.MultiLineStr{Lines: lines}, nil
}

func parseMultiPolygon(obj map[string]json.RawMessage, path string) (cql2.Geometry, error) {
	rawCoords, err := requireCoords(obj, path)
	if err != nil {
		return nil, err
	}
	var arr []json.RawMessage
	if err := json.Unmarshal(rawCoords, &arr); err != nil {
		return nil, gerr(path+"/coordinates", "expected array of polygons")
	}
	polys := make([]cql2.Polygon, len(arr))
	for i, item := range arr {
		polyPath := fmt.Sprintf("%s/coordinates/%d", path, i)
		rings, err := parseRingList(item, polyPath)
		if err != nil {
			return nil, err
		}
		polys[i] = cql2.Polygon{Rings: rings}
	}
	return &cql2.MultiPolygon{Polys: polys}, nil
}

func parseGeometryCollection(obj map[string]json.RawMessage, path string) (cql2.Geometry, error) {
	rawGeoms, ok := obj["geometries"]
	if !ok {
		return nil, gerr(path, "GeometryCollection requires a \"geometries\" member")
	}
	var arr []json.RawMessage
	if err := json.Unmarshal(rawGeoms, &arr); err != nil {
		return nil, gerr(path+"/geometries", "\"geometries\" must be an array")
	}
	geoms := make([]cql2.Geometry, len(arr))
	for i, item := range arr {
		childPath := fmt.Sprintf("%s/geometries/%d", path, i)
		var child map[string]json.RawMessage
		if err := json.Unmarshal(item, &child); err != nil {
			return nil, gerr(childPath, "expected geometry object")
		}
		g, err := parseGeometry(child, childPath)
		if err != nil {
			return nil, err
		}
		geoms[i] = g
	}
	return &cql2.GeometryColl{Geoms: geoms}, nil
}
