package plist

import (
	"encoding/base64"
	"encoding/xml"
	"fmt"
	"io"
	"math"
	"reflect"
	"time"
)

const xmlDOCTYPE = `<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">`

type xmlEncoder struct {
	writer io.Writer
	*xml.Encoder
}

func newXMLEncoder(w io.Writer) *xmlEncoder {
	return &xmlEncoder{w, xml.NewEncoder(w)}
}

func (e *xmlEncoder) generateDocument(pval *plistValue) error {
	// xml version=1.0
	_, err := e.writer.Write([]byte(xml.Header))
	if err != nil {
		return err
	}

	//!DOCTYPE plist
	_, err = e.writer.Write([]byte(xmlDOCTYPE))
	if err != nil {
		return err
	}

	// newline after doctype
	// <plist> tag starts on new line
	_, err = e.writer.Write([]byte("\n"))
	if err != nil {
		return err
	}

	tokenFunc := func(pval *plistValue) error {
		if err := e.writePlistValue(pval); err != nil {
			return err
		}
		return nil
	}
	err = e.writeElement("plist", pval, tokenFunc)
	if err != nil {
		return err
	}
	// newline at the end of a plist document
	_, err = e.writer.Write([]byte("\n"))
	if err != nil {
		return err
	}
	return nil
}

func (e *xmlEncoder) writePlistValue(pval *plistValue) error {
	switch pval.kind {
	case String:
		return e.writeStringValue(pval)
	case Boolean:
		return e.writeBoolValue(pval)
	case Integer:
		return e.writeIntegerValue(pval)
	case Dictionary:
		return e.writeDictionaryValue(pval)
	case Date:
		return e.writeDateValue(pval)
	case Array:
		return e.writeArrayValue(pval)
	case Real:
		return e.writeRealValue(pval)
	case Data:
		return e.writeDataValue(pval)
	default:
		return &UnsupportedTypeError{reflect.ValueOf(pval.value).Type()}
	}
}

func (e *xmlEncoder) writeDataValue(pval *plistValue) error {
	encodedValue := base64.StdEncoding.EncodeToString(pval.value.([]byte))
	return e.EncodeElement(encodedValue, xml.StartElement{Name: xml.Name{Local: "data"}})
}

func (e *xmlEncoder) writeRealValue(pval *plistValue) error {
	encodedValue := pval.value
	switch {
	case math.IsInf(pval.value.(sizedFloat).value, 1):
		encodedValue = "inf"
	case math.IsInf(pval.value.(sizedFloat).value, -1):
		encodedValue = "-inf"
	case math.IsNaN(pval.value.(sizedFloat).value):
		encodedValue = "nan"
	default:
		encodedValue = pval.value.(sizedFloat).value
	}
	return e.EncodeElement(encodedValue, xml.StartElement{Name: xml.Name{Local: "real"}})
}

// writeElement writes an xml element like <plist>, <array> or <dict>
func (e *xmlEncoder) writeElement(name string, pval *plistValue, valFunc func(*plistValue) error) error {
	startElement := xml.StartElement{
		Name: xml.Name{
			Space: "",
			Local: name,
		}}

	if name == "plist" {
		startElement.Attr = []xml.Attr{{
			Name: xml.Name{
				Space: "",
				Local: "version"},
			Value: "1.0"},
		}
	}

	// Encode xml.StartElement token
	if err := e.EncodeToken(startElement); err != nil {
		return err
	}

	// flush
	if err := e.Flush(); err != nil {
		return err
	}

	// execute valFunc()
	if err := valFunc(pval); err != nil {
		return err
	}

	// Encode xml.EndElement token
	if err := e.EncodeToken(startElement.End()); err != nil {
		return err
	}

	// flush
	return e.Flush()
}

func (e *xmlEncoder) writeArrayValue(pval *plistValue) error {
	tokenFunc := func(pval *plistValue) error {
		encodedValue := pval.value
		values := encodedValue.([]*plistValue)
		for _, v := range values {
			if err := e.writePlistValue(v); err != nil {
				return err
			}
		}
		return nil
	}
	return e.writeElement("array", pval, tokenFunc)

}

func (e *xmlEncoder) writeDictionaryValue(pval *plistValue) error {
	tokenFunc := func(pval *plistValue) error {
		encodedValue := pval.value
		dict := encodedValue.(*dictionary)
		dict.populateArrays()
		for i, k := range dict.keys {
			if err := e.EncodeElement(k, xml.StartElement{Name: xml.Name{Local: "key"}}); err != nil {
				return err
			}
			if err := e.writePlistValue(dict.values[i]); err != nil {
				return err
			}
		}
		return nil
	}
	return e.writeElement("dict", pval, tokenFunc)
}

// encode strings as CharData, which doesn't escape newline
// see https://github.com/golang/go/issues/9204
func (e *xmlEncoder) writeStringValue(pval *plistValue) error {
	startElement := xml.StartElement{Name: xml.Name{Local: "string"}}
	// Encode xml.StartElement token
	if err := e.EncodeToken(startElement); err != nil {
		return err
	}

	// flush
	if err := e.Flush(); err != nil {
		return err
	}

	stringValue := pval.value.(string)
	if err := e.EncodeToken(xml.CharData(stringValue)); err != nil {
		return err
	}

	// flush
	if err := e.Flush(); err != nil {
		return err
	}

	// Encode xml.EndElement token
	if err := e.EncodeToken(startElement.End()); err != nil {
		return err
	}

	// flush
	return e.Flush()

}

func (e *xmlEncoder) writeBoolValue(pval *plistValue) error {
	b := pval.value.(bool)
	return e.EncodeElement("", xml.StartElement{Name: xml.Name{Local: fmt.Sprintf("%t", b)}})
}

func (e *xmlEncoder) writeIntegerValue(pval *plistValue) error {
	encodedValue := pval.value
	if pval.value.(signedInt).signed {
		encodedValue = int64(pval.value.(signedInt).value)
	} else {
		encodedValue = pval.value.(signedInt).value
	}
	return e.EncodeElement(encodedValue, xml.StartElement{Name: xml.Name{Local: "integer"}})
}

func (e *xmlEncoder) writeDateValue(pval *plistValue) error {
	encodedValue := pval.value.(time.Time).In(time.UTC).Format(time.RFC3339)
	return e.EncodeElement(encodedValue, xml.StartElement{Name: xml.Name{Local: "date"}})
}
