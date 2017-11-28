package compiler

import (
	"os"
	"path/filepath"
	"strings"

	grammar "github.com/github/linguist/tools/grammars/proto"
)

type Loaded struct {
	source      string
	rule        *grammar.Rule
	unknownKeys []string
}

type Loader interface {
	Load(map[string]Loaded) error
}

func isValidGrammar(path string, info os.FileInfo) bool {
	if info.IsDir() {
		return false
	}

	dir := filepath.Dir(path)
	ext := filepath.Ext(path)

	switch strings.ToLower(ext) {
	case ".plist":
		return strings.HasSuffix(dir, "/Syntaxes")
	case ".tmlanguage", ".yaml-tmlanguage":
		return true
	case ".cson", ".json":
		return strings.HasSuffix(dir, "/grammars")
	default:
		return false
	}
}
