package compiler

import (
	"encoding/json"
	"testing"

	"github.com/groob/plist"
	yaml "gopkg.in/yaml.v2"
)

func TestConvertProtoScopeName(t *testing.T) {
	tests := []struct {
		name    string
		scope   string
		invalid bool
	}{
		{"dotted scope", "source.go", false},
		{"plus signs", "source.c++", false},
		{"nested dotted scope", "source.c++.qt", false},
		{"hyphen and plus signs", "source.cuda-c++", false},
		{"consecutive dots", "source.foo..bar", false},
		{"single dot", ".", false},
		{"two dots", "..", false},
		{"reserved word after dot", "source.CON", false},
		{"reserved port after dot", "source.COM1", false},
		{"reserved name prefix", "CONsole", false},
		{"reserved name suffix", "xAUX", false},
		{"reserved port prefix", "COM1suffix", false},
		{"COM without number", "COM", false},
		{"LPT without number", "LPT", false},
		{"COM zero", "COM0", false},
		{"LPT zero", "LPT0", false},
		{"COM two digits", "COM10", false},
		{"LPT two digits", "LPT10", false},
		{"COM unreserved superscript", "COM⁴", false},
		{"LPT unreserved superscript", "LPT⁴", false},
		{"reserved AUX", "AUX", true},
		{"reserved CON", "CON", true},
		{"reserved NUL", "NUL", true},
		{"reserved PRN", "PRN", true},
		{"reserved COM1", "COM1", true},
		{"reserved COM2", "COM2", true},
		{"reserved COM3", "COM3", true},
		{"reserved COM4", "COM4", true},
		{"reserved COM5", "COM5", true},
		{"reserved COM6", "COM6", true},
		{"reserved COM7", "COM7", true},
		{"reserved COM8", "COM8", true},
		{"reserved COM9", "COM9", true},
		{"reserved LPT1", "LPT1", true},
		{"reserved LPT2", "LPT2", true},
		{"reserved LPT3", "LPT3", true},
		{"reserved LPT4", "LPT4", true},
		{"reserved LPT5", "LPT5", true},
		{"reserved LPT6", "LPT6", true},
		{"reserved LPT7", "LPT7", true},
		{"reserved LPT8", "LPT8", true},
		{"reserved LPT9", "LPT9", true},
		{"reserved COM superscript one", "COM¹", true},
		{"reserved COM superscript two", "COM²", true},
		{"reserved COM superscript three", "COM³", true},
		{"reserved LPT superscript one", "LPT¹", true},
		{"reserved LPT superscript two", "LPT²", true},
		{"reserved LPT superscript three", "LPT³", true},
		{"reserved lowercase name", "aux", true},
		{"reserved mixed case name", "cOn", true},
		{"reserved lowercase port", "com1", true},
		{"reserved mixed case port", "lPt9", true},
		{"reserved name with extension", "NUL.txt", true},
		{"reserved name with multiple extensions", "prn.tar.gz", true},
		{"reserved port with extension", "COM1.source", true},
		{"reserved superscript port with extension", "lpt².source", true},
		{"reserved name with trailing dot", "AUX.", true},
		{"reported traversal", "../.devcontainer/devcontainer", true},
		{"nested traversal", "nested/../../outside", true},
		{"nested path", "source/nested", true},
		{"rooted path", "/outside", true},
		{"trailing slash", "source.go/", true},
		{"backslash traversal", `..\outside`, true},
		{"backslash path", `source\nested`, true},
		{"drive path", `C:\outside`, true},
		{"UNC path", `\\server\share\outside`, true},
		{"mixed separators", `nested\../outside`, true},
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
				if tt.invalid {
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
					return
				}
				if err != nil {
					t.Fatal(err)
				}
				if rule == nil || rule.ScopeName != tt.scope {
					t.Errorf("got rule %v, want scope %q unchanged", rule, tt.scope)
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
