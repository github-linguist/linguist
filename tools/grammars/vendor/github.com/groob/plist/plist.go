package plist

import "sort"

type plistKind uint

const (
	Invalid plistKind = iota
	Dictionary
	Array
	String
	Integer
	Real
	Boolean
	Data
	Date
)

var plistKindNames = map[plistKind]string{
	Invalid:    "invalid",
	Dictionary: "dictionary",
	Array:      "array",
	String:     "string",
	Integer:    "integer",
	Real:       "real",
	Boolean:    "boolean",
	Data:       "data",
	Date:       "date",
}

type plistValue struct {
	kind  plistKind
	value interface{}
}

type signedInt struct {
	value  uint64
	signed bool
}

type sizedFloat struct {
	value float64
	bits  int
}

type dictionary struct {
	count  int
	m      map[string]*plistValue
	keys   sort.StringSlice
	values []*plistValue
}

func (d *dictionary) Len() int {
	return len(d.m)
}

func (d *dictionary) Less(i, j int) bool {
	return d.keys.Less(i, j)
}

func (d *dictionary) Swap(i, j int) {
	d.keys.Swap(i, j)
	d.values[i], d.values[j] = d.values[j], d.values[i]
}

func (d *dictionary) populateArrays() {
	d.keys = make([]string, len(d.m))
	d.values = make([]*plistValue, len(d.m))
	i := 0
	for k, v := range d.m {
		d.keys[i] = k
		d.values[i] = v
		i++
	}
	sort.Sort(d)
}
