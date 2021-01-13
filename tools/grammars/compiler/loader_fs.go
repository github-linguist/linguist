package compiler

import (
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"sort"
	"strings"
)

type fsLoader struct {
	*Repository
	abspath string
}

var preferredGrammars = map[string]int{
	".tmlanguage":      0,
	".cson":            1,
	".json":            1,
	".plist":           2,
	".yaml-tmlanguage": 3,
}

func findPreferredExtension(ext []string) string {
	if len(ext) > 1 {
		sort.Slice(ext, func(i, j int) bool {
			a := strings.ToLower(ext[i])
			b := strings.ToLower(ext[j])
			return preferredGrammars[a] < preferredGrammars[b]
		})
	}
	return ext[0]
}

func (l *fsLoader) findGrammars() (files []string, err error) {
	grammars := make(map[string][]string)

	err = filepath.Walk(l.abspath,
		func(path string, info os.FileInfo, err error) error {
			if err == nil && isValidGrammar(path, info) {
				ext := filepath.Ext(path)
				base := path[0 : len(path)-len(ext)]
				grammars[base] = append(grammars[base], ext)
			}
			return nil
		})

	for base, ext := range grammars {
		pref := findPreferredExtension(ext)
		files = append(files, base+pref)
	}

	return
}

func (l *fsLoader) load() {
	grammars, err := l.findGrammars()
	if err != nil {
		l.Fail(err)
		return
	}

	for _, path := range grammars {
		rel, err := filepath.Rel("/src/linguist/vendor/grammars/", path)
		if err != nil {
			l.Fail(err)
			return
		}

		// Ignore all files under src directories
		if ok, _ := filepath.Match("*/src/*", rel); ok {
			continue
		}

		if IgnoredFiles[rel] {
			continue
		}

		data, err := ioutil.ReadFile(path)
		if err != nil {
			l.Fail(err)
			continue
		}

		if rel, err := filepath.Rel(l.abspath, path); err == nil {
			path = rel
		}

		rule, unknown, err := ConvertProto(path, filepath.Ext(path), data)
		if err != nil {
			l.Fail(&ConversionError{path, err})
			continue
		}

		if _, ok := l.Files[rule.ScopeName]; ok {
			continue
		}

		l.AddFile(path, rule, unknown)
	}
}

func gitRemoteName(path string) (string, error) {
	remote, err := exec.Command("git", "-C", path, "remote", "get-url", "origin").Output()
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(remote)), nil
}

func LoadFromFilesystem(root, src string) *Repository {
	loader := fsLoader{
		Repository: newRepository(src),
		abspath:    path.Join(root, src),
	}
	loader.load()

	if ups, err := gitRemoteName(loader.abspath); err == nil {
		loader.Repository.Upstream = ups
	}

	return loader.Repository
}
