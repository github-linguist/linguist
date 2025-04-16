package compiler

import (
	"archive/tar"
	"compress/gzip"
	"io"
	"io/ioutil"
	"net/http"
	"path/filepath"
	"strings"
)

type urlLoader struct {
	*Repository
}

func (l *urlLoader) loadTarball(r io.Reader) {
	gzf, err := gzip.NewReader(r)
	if err != nil {
		l.Fail(err)
		return
	}
	defer gzf.Close()

	tarReader := tar.NewReader(gzf)
	for true {
		header, err := tarReader.Next()

		if err != nil {
			if err != io.EOF {
				l.Fail(err)
			}
			return
		}

		if isValidGrammar(header.Name, header.FileInfo()) {
			data, err := ioutil.ReadAll(tarReader)
			if err != nil {
				l.Fail(err)
				return
			}

			ext := filepath.Ext(header.Name)
			rule, unknown, err := ConvertProto(header.Name, ext, data)
			if err != nil {
				l.Fail(&ConversionError{header.Name, err})
				continue
			}

			if _, ok := l.Files[rule.ScopeName]; ok {
				continue
			}

			l.AddFile(header.Name, rule, unknown)
		}
	}
}

func (l *urlLoader) load() {
	res, err := http.Get(l.Source)
	if err != nil {
		l.Fail(err)
		return
	}
	defer res.Body.Close()

	if strings.HasSuffix(l.Source, ".tar.gz") {
		l.loadTarball(res.Body)
		return
	}

	data, err := ioutil.ReadAll(res.Body)
	if err != nil {
		l.Fail(err)
		return
	}

	ext := filepath.Ext(l.Source)
	filename := filepath.Base(l.Source)
	rule, unknown, err := ConvertProto(l.Source, ext, data)
	if err != nil {
		l.Fail(&ConversionError{filename, err})
		return
	}

	l.AddFile(filename, rule, unknown)
}

func LoadFromURL(src string) *Repository {
	loader := urlLoader{newRepository(src)}
	loader.load()
	return loader.Repository
}
