package compiler

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strings"

	grammar "github.com/github/linguist/tools/grammars/proto"
	"github.com/groob/plist"
	"github.com/mitchellh/mapstructure"
	yaml "gopkg.in/yaml.v2"
)

func looseDecoder(f reflect.Kind, t reflect.Kind, data interface{}) (interface{}, error) {
	dataVal := reflect.ValueOf(data)
	switch t {
	case reflect.Bool:
		switch f {
		case reflect.Bool:
			return dataVal.Bool(), nil
		case reflect.Float32, reflect.Float64:
			return (int(dataVal.Float()) != 0), nil
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			return (dataVal.Int() != 0), nil
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			return (dataVal.Uint() != 0), nil
		case reflect.String:
			switch dataVal.String() {
			case "1":
				return true, nil
			case "0":
				return false, nil
			}
		}
	}

	return data, nil
}

func filterUnusedKeys(keys []string) (out []string) {
	for _, k := range keys {
		parts := strings.Split(k, ".")
		field := parts[len(parts)-1]
		if !KnownFields[field] {
			out = append(out, k)
		}
	}
	return
}

func ConvertProto(path, ext string, data []byte) (*grammar.Rule, []string, error) {
	var (
		raw map[string]interface{}
		out grammar.Rule
		err error
		md  mapstructure.Metadata
	)

	switch strings.ToLower(ext) {
	case ".plist", ".tmlanguage":
		err = plist.Unmarshal(data, &raw)
	case ".yaml-tmlanguage":
		err = yaml.Unmarshal(data, &raw)
	case ".cson":
		data, err = ConvertCSON(data)
		if err == nil {
			err = json.Unmarshal(data, &raw)
		}
	case ".json":
		err = json.Unmarshal(data, &raw)
	default:
		err = fmt.Errorf("grammars: unsupported extension '%s'", ext)
	}

	if err != nil {
		return nil, nil, err
	}

	config := mapstructure.DecoderConfig{
		Result:     &out,
		Metadata:   &md,
		DecodeHook: looseDecoder,
	}

	decoder, err := mapstructure.NewDecoder(&config)
	if err != nil {
		return nil, nil, err
	}

	if err := decoder.Decode(raw); err != nil {
		return nil, nil, err
	}

	if out.ScopeName == "" {
		return nil, nil, &UndeclaredScopeError{path}
	}

	return &out, filterUnusedKeys(md.Unused), nil
}
