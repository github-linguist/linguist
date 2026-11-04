package compiler

import (
	"encoding/json"
	"testing"

	"github.com/groob/plist"
	yaml "gopkg.in/yaml.v2"
)

func TestConvertProtoScopeName(t *testing.T) {
	tests := []struct {
		name  string
		scope string
		valid bool
	}{
		{"dotted scope", "source.go", true},
		{"plus signs", "source.c++", true},
		{"nested dotted scope", "source.c++.qt", true},
		{"hyphen and plus signs", "source.cuda-c++", true},
		{"consecutive dots", "source.foo..bar", true},
		{"single dot", ".", true},
		{"two dots", "..", true},
		{"reserved word after dot", "source.CON", true},
		{"reserved port after dot", "source.COM1", true},
		{"reserved name prefix", "CONsole", true},
		{"reserved name suffix", "xAUX", true},
		{"reserved port prefix", "COM1suffix", true},
		{"COM without number", "COM", true},
		{"LPT without number", "LPT", true},
		{"COM zero", "COM0", true},
		{"LPT zero", "LPT0", true},
		{"COM two digits", "COM10", true},
		{"LPT two digits", "LPT10", true},
		{"COM unreserved superscript", "COM⁴", true},
		{"LPT unreserved superscript", "LPT⁴", true},
		{"reserved AUX", "AUX", false},
		{"reserved CON", "CON", false},
		{"reserved NUL", "NUL", false},
		{"reserved PRN", "PRN", false},
		{"reserved COM1", "COM1", false},
		{"reserved COM2", "COM2", false},
		{"reserved COM3", "COM3", false},
		{"reserved COM4", "COM4", false},
		{"reserved COM5", "COM5", false},
		{"reserved COM6", "COM6", false},
		{"reserved COM7", "COM7", false},
		{"reserved COM8", "COM8", false},
		{"reserved COM9", "COM9", false},
		{"reserved LPT1", "LPT1", false},
		{"reserved LPT2", "LPT2", false},
		{"reserved LPT3", "LPT3", false},
		{"reserved LPT4", "LPT4", false},
		{"reserved LPT5", "LPT5", false},
		{"reserved LPT6", "LPT6", false},
		{"reserved LPT7", "LPT7", false},
		{"reserved LPT8", "LPT8", false},
		{"reserved LPT9", "LPT9", false},
		{"reserved COM superscript one", "COM¹", false},
		{"reserved COM superscript two", "COM²", false},
		{"reserved COM superscript three", "COM³", false},
		{"reserved LPT superscript one", "LPT¹", false},
		{"reserved LPT superscript two", "LPT²", false},
		{"reserved LPT superscript three", "LPT³", false},
		{"reserved lowercase name", "aux", false},
		{"reserved mixed case name", "cOn", false},
		{"reserved lowercase port", "com1", false},
		{"reserved mixed case port", "lPt9", false},
		{"reserved name with extension", "NUL.txt", false},
		{"reserved name with multiple extensions", "prn.tar.gz", false},
		{"reserved port with extension", "COM1.source", false},
		{"reserved superscript port with extension", "lpt².source", false},
		{"reserved name with trailing dot", "AUX.", false},
		{"reported traversal", "../.devcontainer/devcontainer", false},
		{"nested traversal", "nested/../../outside", false},
		{"nested path", "source/nested", false},
		{"rooted path", "/outside", false},
		{"trailing slash", "source.go/", false},
		{"backslash traversal", `..\outside`, false},
		{"backslash path", `source\nested`, false},
		{"drive path", `C:\outside`, false},
		{"UNC path", `\\server\share\outside`, false},
		{"mixed separators", `nested\../outside`, false},
	}
	formats := []struct {
		ext     string
		marshal func(interface{}) ([]byte, error)
	}{
		{".json", json.Marshal},
		{".yaml-tmlanguage", yaml.Marshal},
		{".plist", plist.Marshal},
		{".tmLanguage", plist.Marshal},
	}
	for _, format := range formats {
		for _, tt := range tests {
			t.Run(format.ext+"/"+tt.name, func(t *testing.T) {
				data, err := format.marshal(map[string]string{"scopeName": tt.scope})
				if err != nil {
					t.Fatal(err)
				}
				path := "grammar" + format.ext
				rule, unknown, err := ConvertProto(path, format.ext, data)
				if tt.valid {
					if err != nil {
						t.Fatal(err)
					}
					if rule == nil || rule.ScopeName != tt.scope {
						t.Errorf("got rule %v, want scope %q unchanged", rule, tt.scope)
					}
					return
				}
				invalid, ok := err.(*InvalidScopeError)
				if !ok {
					t.Fatalf("got error %T (%v), want *InvalidScopeError", err, err)
				}
				if rule != nil || unknown != nil {
					t.Errorf("rejected grammar returned a rule or unused keys: %v, %v", rule, unknown)
				}
				if invalid.Path != path || invalid.Scope != tt.scope {
					t.Errorf("invalid scope error = %#v, want path %q and scope %q", invalid, path, tt.scope)
				}
			})
		}
	}
}

func TestConvertProtoEscapedInvalidScopeName(t *testing.T) {
	for _, data := range []string{
		`{"scopeName":"..\u002foutside"}`,
		`{"scopeName":"..\u005coutside"}`,
		`{"scopeName":"\u0043ON"}`,
		`{"scopeName":"COM\u00b9"}`,
		`{"scopeName":"LPT\u00b2"}`,
		`{"scopeName":"COM\u00b3"}`,
	} {
		t.Run(data, func(t *testing.T) {
			rule, _, err := ConvertProto("grammar.json", ".json", []byte(data))
			if _, ok := err.(*InvalidScopeError); !ok {
				t.Fatalf("got error %T (%v), want *InvalidScopeError", err, err)
			}
			if rule != nil {
				t.Errorf("rejected grammar returned a rule: %v", rule)
			}
		})
	}
}

func TestConvertProtoUndeclaredScope(t *testing.T) {
	for _, data := range []string{`{}`, `{"scopeName":""}`} {
		t.Run(data, func(t *testing.T) {
			rule, unknown, err := ConvertProto("grammar.json", ".json", []byte(data))
			undeclared, ok := err.(*UndeclaredScopeError)
			if !ok {
				t.Fatalf("got error %T (%v), want *UndeclaredScopeError", err, err)
			}
			if undeclared.Path != "grammar.json" {
				t.Errorf("error path = %q, want grammar.json", undeclared.Path)
			}
			if rule != nil || unknown != nil {
				t.Errorf("undeclared scope returned a rule or unused keys: %v, %v", rule, unknown)
			}
		})
	}
}
