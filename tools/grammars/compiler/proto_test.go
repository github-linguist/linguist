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

func TestConvertProtoEscapedScopeSeparators(t *testing.T) {
	for _, data := range []string{
		`{"scopeName":"..\u002foutside"}`,
		`{"scopeName":"..\u005coutside"}`,
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
