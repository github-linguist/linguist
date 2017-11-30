package compiler

import (
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strings"
)

type fsLoader struct {
	*Repository
	abspath string
}

func (l *fsLoader) findGrammars() (files []string, err error) {
	err = filepath.Walk(l.abspath,
		func(path string, info os.FileInfo, err error) error {
			if err == nil && isValidGrammar(path, info) {
				files = append(files, path)
			}
			return nil
		})
	return
}

func (l *fsLoader) load() {
	grammars, err := l.findGrammars()
	if err != nil {
		l.Fail(err)
		return
	}

	for _, path := range grammars {
		data, err := ioutil.ReadFile(path)
		if err != nil {
			l.Fail(err)
			continue
		}

		if rel, err := filepath.Rel(l.abspath, path); err == nil {
			path = rel
		}

		rule, unknown, err := ConvertProto(filepath.Ext(path), data)
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
