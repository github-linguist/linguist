package compiler

import (
	"io/ioutil"
	"os"
	"path/filepath"
)

type fsLoader struct {
	path string
}

func (l *fsLoader) findGrammars() (files []string, err error) {
	err = filepath.Walk(l.path,
		func(path string, info os.FileInfo, err error) error {
			if err == nil && isValidGrammar(path, info) {
				files = append(files, path)
			}
			return nil
		})
	return
}

func (l *fsLoader) Load(loaded map[string]Loaded) error {
	grammars, err := l.findGrammars()
	if err != nil {
		return err
	}

	for _, path := range grammars {
		data, err := ioutil.ReadFile(path)
		if err != nil {
			return err
		}

		rule, unknown, err := ConvertProto(filepath.Ext(path), data)
		if err != nil {
			return &ConversionError{err, path}
		}

		if _, ok := loaded[rule.ScopeName]; ok {
			continue
		}

		loaded[rule.ScopeName] = Loaded{path, rule, unknown}
	}

	return nil
}
