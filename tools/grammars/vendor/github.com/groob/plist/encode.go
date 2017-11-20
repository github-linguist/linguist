package plist

import (
	"bytes"
	"io"
	"reflect"
	"time"
)

type Marshaler interface {
	MarshalPlist() (interface{}, error)
}

// Encoder ...
type Encoder struct {
	w io.Writer

	indent string
}

// Marshal ...
func Marshal(v interface{}) ([]byte, error) {
	var buf bytes.Buffer
	if err := NewEncoder(&buf).Encode(v); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

// MarshalIndent ...
func MarshalIndent(v interface{}, indent string) ([]byte, error) {
	var buf bytes.Buffer
	enc := NewEncoder(&buf)
	enc.Indent(indent)
	if err := enc.Encode(v); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

// NewEncoder returns a new encoder that writes to w.
func NewEncoder(w io.Writer) *Encoder {
	return &Encoder{w: w}
}

// Encode ...
func (e *Encoder) Encode(v interface{}) error {
	pval, err := e.marshal(reflect.ValueOf(v))
	if err != nil {
		return err
	}

	enc := newXMLEncoder(e.w)
	enc.Indent("", e.indent)
	return enc.generateDocument(pval)
}

// Indent ...
func (e *Encoder) Indent(indent string) {
	e.indent = indent
}

func (e *Encoder) marshal(v reflect.Value) (*plistValue, error) {
	marshalerType := reflect.TypeOf((*Marshaler)(nil)).Elem()

	if v.CanInterface() && v.Type().Implements(marshalerType) {
		m := v.Interface().(Marshaler)
		val, err := m.MarshalPlist()
		if err != nil {
			return nil, err
		}
		return e.marshal(reflect.ValueOf(val))
	}

	if v.CanAddr() {
		pv := v.Addr()
		if pv.CanInterface() && pv.Type().Implements(marshalerType) {
			m := pv.Interface().(Marshaler)
			val, err := m.MarshalPlist()
			if err != nil {
				return nil, err
			}
			return e.marshal(reflect.ValueOf(val))
		}
	}

	// check for empty interface v type
	if v.Kind() == reflect.Interface && v.NumMethod() == 0 || v.Kind() == reflect.Ptr {
		v = v.Elem()
	}

	// check for time type
	if v.Type() == reflect.TypeOf((*time.Time)(nil)).Elem() {
		if date, ok := v.Interface().(time.Time); ok {
			return &plistValue{Date, date}, nil
		}
		return nil, &UnsupportedValueError{v, v.String()}
	}

	switch v.Kind() {
	case reflect.String:
		return &plistValue{String, v.String()}, nil
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return &plistValue{Integer, signedInt{uint64(v.Int()), true}}, nil
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return &plistValue{Integer, signedInt{uint64(v.Uint()), false}}, nil
	case reflect.Float32, reflect.Float64:
		return &plistValue{Real, sizedFloat{v.Float(), v.Type().Bits()}}, nil
	case reflect.Bool:
		return &plistValue{Boolean, v.Bool()}, nil
	case reflect.Slice, reflect.Array:
		return e.marshalArray(v)
	case reflect.Map:
		return e.marshalMap(v)
	case reflect.Struct:
		return e.marshalStruct(v)
	default:
		return nil, &UnsupportedTypeError{v.Type()}
	}
}

func (e *Encoder) marshalStruct(v reflect.Value) (*plistValue, error) {
	fields := cachedTypeFields(v.Type())
	dict := &dictionary{
		m: make(map[string]*plistValue, len(fields)),
	}
	for _, field := range fields {
		val := field.value(v)
		if field.omitEmpty && isEmptyValue(val) {
			continue
		}
		value, err := e.marshal(field.value(v))
		if err != nil {
			return nil, err
		}
		dict.m[field.name] = value
	}
	return &plistValue{Dictionary, dict}, nil
}

func (e *Encoder) marshalArray(v reflect.Value) (*plistValue, error) {
	if v.Type().Elem().Kind() == reflect.Uint8 {
		bytes := []byte(nil)
		if v.CanAddr() {
			bytes = v.Slice(0, v.Len()).Bytes()
		} else {
			bytes = make([]byte, v.Len())
			reflect.Copy(reflect.ValueOf(bytes), v)
		}
		return &plistValue{Data, bytes}, nil
	}
	subvalues := make([]*plistValue, v.Len())
	for idx, length := 0, v.Len(); idx < length; idx++ {
		subpval, err := e.marshal(v.Index(idx))
		if err != nil {
			return nil, err
		}
		if subpval != nil {
			subvalues[idx] = subpval
		}
	}
	return &plistValue{Array, subvalues}, nil
}

func (e *Encoder) marshalMap(v reflect.Value) (*plistValue, error) {
	if v.Type().Key().Kind() != reflect.String {
		return nil, &UnsupportedTypeError{v.Type()}
	}

	l := v.Len()
	dict := &dictionary{
		m: make(map[string]*plistValue, l),
	}
	for _, keyv := range v.MapKeys() {
		subpval, err := e.marshal(v.MapIndex(keyv))
		if err != nil {
			return nil, err
		}
		if subpval != nil {
			dict.m[keyv.String()] = subpval
		}
	}
	return &plistValue{Dictionary, dict}, nil
}

// An UnsupportedTypeError is returned by Marshal when attempting
// to encode an unsupported value type.
type UnsupportedTypeError struct {
	Type reflect.Type
}

func (e *UnsupportedTypeError) Error() string {
	return "plist: unsupported type: " + e.Type.String()
}

// UnsupportedValueError ...
type UnsupportedValueError struct {
	Value reflect.Value
	Str   string
}

func (e *UnsupportedValueError) Error() string {
	return "plist: unsupported value: " + e.Str
}

func isEmptyValue(v reflect.Value) bool {
	switch v.Kind() {
	case reflect.Array, reflect.Map, reflect.Slice, reflect.String:
		return v.Len() == 0
	case reflect.Bool:
		return !v.Bool()
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return v.Int() == 0
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return v.Uint() == 0
	case reflect.Float32, reflect.Float64:
		return v.Float() == 0
	case reflect.Interface, reflect.Ptr:
		return v.IsNil()
	}
	return false
}
