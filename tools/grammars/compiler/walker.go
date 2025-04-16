package compiler

import (
	"strings"

	grammar "github.com/github/linguist/tools/grammars/proto"
)

func (w *walker) checkInclude(rule *grammar.Rule) {
	include := rule.Include

	if include == "" || include[0] == '#' || include[0] == '$' {
		return
	}

	if alias, ok := GrammarAliases[include]; ok {
		rule.Include = alias
		return
	}

	include = strings.Split(include, "#")[0]
	ok := w.Known[include]
	if !ok {
		if !w.Missing[include] {
			w.Missing[include] = true
			w.Errors = append(w.Errors, &MissingIncludeError{w.File, include})
		}
		rule.Include = ""
	}
}

func (w *walker) checkRegexps(rule *grammar.Rule) {
	check := func(re string) string {
		re2, err := CheckPCRE(re)
		if err != nil {
			w.Errors = append(w.Errors, &InvalidRegexError{w.File, err})
		}
		return re2
	}

	rule.Match = check(rule.Match)
	rule.Begin = check(rule.Begin)
	rule.While = check(rule.While)
	rule.End = check(rule.End)
}

func (w *walker) walk(rule *grammar.Rule) {
	w.checkInclude(rule)
	w.checkRegexps(rule)

	for _, rule := range rule.Patterns {
		w.walk(rule)
	}
	for _, rule := range rule.Captures {
		w.walk(rule)
	}
	for _, rule := range rule.BeginCaptures {
		w.walk(rule)
	}
	for _, rule := range rule.WhileCaptures {
		w.walk(rule)
	}
	for _, rule := range rule.EndCaptures {
		w.walk(rule)
	}
	for _, rule := range rule.Repository {
		w.walk(rule)
	}
	for _, rule := range rule.Injections {
		w.walk(rule)
	}
}

type walker struct {
	File    *LoadedFile
	Known   map[string]bool
	Missing map[string]bool
	Errors  []error
}
