package compiler

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	grammar "github.com/github/linguist/tools/grammars/proto"
)

type LoadedFile struct {
	Path string
	Rule *grammar.Rule
}

func (f *LoadedFile) String() string {
	return fmt.Sprintf("`%s` (in `%s`)", f.Rule.ScopeName, f.Path)
}

type Repository struct {
	Source   string
	Upstream string
	Files    map[string]*LoadedFile
	Errors   []error
}

func newRepository(src string) *Repository {
	return &Repository{
		Source: src,
		Files:  make(map[string]*LoadedFile),
	}
}

func (repo *Repository) String() string {
	str := fmt.Sprintf("repository `%s`", repo.Source)
	if repo.Upstream != "" {
		str = str + fmt.Sprintf(" (from %s)", repo.Upstream)
	}
	return str
}

func (repo *Repository) Fail(err error) {
	repo.Errors = append(repo.Errors, err)
}

func (repo *Repository) AddFile(path string, rule *grammar.Rule, uk []string) {
	file := &LoadedFile{
		Path: path,
		Rule: rule,
	}

	repo.Files[rule.ScopeName] = file
	if len(uk) > 0 {
		repo.Fail(&UnknownKeysError{file, uk})
	}
}

func toMap(slice []string) map[string]bool {
	m := make(map[string]bool)
	for _, s := range slice {
		m[s] = true
	}
	return m
}

func (repo *Repository) CompareScopes(scopes []string) {
	expected := toMap(scopes)

	for scope, file := range repo.Files {
		if !expected[scope] {
			repo.Fail(&UnexpectedScopeError{file, scope})
		}
	}

	for scope := range expected {
		if _, ok := repo.Files[scope]; !ok {
			repo.Fail(&MissingScopeError{scope})
		}
	}
}

func (repo *Repository) FixRules(knownScopes map[string]bool) {
	for _, file := range repo.Files {
		w := walker{
			File:    file,
			Known:   knownScopes,
			Missing: make(map[string]bool),
		}

		w.walk(file.Rule)
		repo.Errors = append(repo.Errors, w.Errors...)

	}
}

func (repo *Repository) Scopes() (scopes []string) {
	for s := range repo.Files {
		scopes = append(scopes, s)
	}
	sort.Strings(scopes)
	return
}

func isValidGrammar(path string, info os.FileInfo) bool {
	if info.IsDir() {
		return false
	}

	// Tree-Sitter grammars are not supported
	if strings.HasPrefix(filepath.Base(path), "tree-sitter-") {
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
		return strings.HasSuffix(dir, "/grammars") || strings.HasSuffix(dir, "/syntaxes") || GrammarsInNonStdPath[filepath.Base(dir)]
	default:
		return false
	}
}
