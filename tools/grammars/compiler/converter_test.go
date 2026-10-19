package compiler

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	grammar "github.com/github/linguist/tools/grammars/proto"
)

func TestWriteJSONSkipsInvalidScope(t *testing.T) {
	root := t.TempDir()
	const invalidScope = "../.devcontainer/devcontainer"
	const sentinel = `{"name":"keep this file"}`
	sentinelPath := filepath.Join(root, ".devcontainer", "devcontainer.json")
	for name, data := range map[string]string{
		sentinelPath: sentinel,
		filepath.Join(root, "grammar", "syntaxes", "invalid.json"): `{"scopeName":"../.devcontainer/devcontainer"}`,
		filepath.Join(root, "grammar", "syntaxes", "valid.json"):   `{"scopeName":"source.c++","name":"Valid grammar"}`,
	} {
		if err := os.MkdirAll(filepath.Dir(name), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(name, []byte(data), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	repo := LoadFromFilesystem(root, "grammar")
	conv := &Converter{
		version: "test-version",
		Loaded:  map[string]*Repository{"grammar": repo},
	}
	output := filepath.Join(root, "output")
	if err := conv.WriteJSON(output); err != nil {
		t.Fatal(err)
	}
	data, err := os.ReadFile(sentinelPath)
	if err != nil {
		t.Fatal(err)
	}
	if string(data) != sentinel {
		t.Errorf("outside file overwritten: got %q, want %q", data, sentinel)
	}
	if _, ok := repo.Files[invalidScope]; ok {
		t.Error("invalid scope was admitted to the repository")
	}
	if len(repo.Files) != 1 {
		t.Errorf("got %d loaded grammars, want 1", len(repo.Files))
	}
	entries, err := os.ReadDir(output)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 2 || entries[0].Name() != "source.c++.json" || entries[1].Name() != "version" {
		t.Fatalf("unexpected output files: %v", entries)
	}
	data, err = os.ReadFile(filepath.Join(output, "source.c++.json"))
	if err != nil {
		t.Fatal(err)
	}
	var rule grammar.Rule
	if err := json.Unmarshal(data, &rule); err != nil {
		t.Fatal(err)
	}
	if rule.ScopeName != "source.c++" || rule.Name != "Valid grammar" {
		t.Errorf("valid grammar changed: %v", &rule)
	}
	data, err = os.ReadFile(filepath.Join(output, "version"))
	if err != nil {
		t.Fatal(err)
	}
	if string(data) != conv.version {
		t.Errorf("version = %q, want %q", data, conv.version)
	}
	if len(repo.Errors) != 1 {
		t.Fatalf("got %d conversion errors, want 1: %v", len(repo.Errors), repo.Errors)
	}
	conversion, ok := repo.Errors[0].(*ConversionError)
	if !ok {
		t.Fatalf("got error %T, want *ConversionError", repo.Errors[0])
	}
	invalid, ok := conversion.Err.(*InvalidScopeError)
	if !ok {
		t.Fatalf("got conversion error %T, want *InvalidScopeError", conversion.Err)
	}
	if invalid.Scope != invalidScope {
		t.Errorf("invalid scope = %q, want %q", invalid.Scope, invalidScope)
	}
	if err := conv.Report(false); err == nil {
		t.Error("Report did not return the conversion failure")
	}
}
