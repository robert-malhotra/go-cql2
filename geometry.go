package cql2

// Geometry is the sealed interface implemented by every geometry variant.
type Geometry interface {
	GeometryType() GeometryType
	isGeometry()
}

// GeometryType identifies a geometry's concrete variant.
type GeometryType uint8

const (
	GeomPoint GeometryType = iota + 1
	GeomLineString
	GeomPolygon
	GeomMultiPoint
	GeomMultiLineString
	GeomMultiPolygon
	GeomGeometryCollection
)

// Coord is a 2D or 3D coordinate; HasZ reports whether Z is meaningful.
type Coord struct {
	X, Y float64
	Z    float64
	HasZ bool
}

// Point is a single coordinate; Empty marks "POINT EMPTY".
type Point struct {
	Coord Coord
	Empty bool
}

// LineString is an ordered sequence of coordinates.
type LineString struct{ Coords []Coord }

// Polygon is a sequence of rings (outer first, then holes).
type Polygon struct{ Rings [][]Coord }

// MultiPoint is a collection of points.
type MultiPoint struct{ Points []Point }

// MultiLineString is a collection of line strings.
type MultiLineString struct{ Lines []LineString }

// MultiPolygon is a collection of polygons.
type MultiPolygon struct{ Polys []Polygon }

// GeometryCollection is a heterogeneous collection of geometries.
type GeometryCollection struct{ Geoms []Geometry }

func (*Point) GeometryType() GeometryType              { return GeomPoint }
func (*Point) isGeometry()                             {}
func (*LineString) GeometryType() GeometryType         { return GeomLineString }
func (*LineString) isGeometry()                        {}
func (*Polygon) GeometryType() GeometryType            { return GeomPolygon }
func (*Polygon) isGeometry()                           {}
func (*MultiPoint) GeometryType() GeometryType         { return GeomMultiPoint }
func (*MultiPoint) isGeometry()                        {}
func (*MultiLineString) GeometryType() GeometryType    { return GeomMultiLineString }
func (*MultiLineString) isGeometry()                   {}
func (*MultiPolygon) GeometryType() GeometryType       { return GeomMultiPolygon }
func (*MultiPolygon) isGeometry()                      {}
func (*GeometryCollection) GeometryType() GeometryType { return GeomGeometryCollection }
func (*GeometryCollection) isGeometry()                {}
