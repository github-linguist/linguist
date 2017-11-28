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
	url string
}

func (l *urlLoader) loadTarball(r io.Reader, loaded map[string]Loaded) error {
	gzf, err := gzip.NewReader(r)
	if err != nil {
		return err
	}
	defer gzf.Close()

	tarReader := tar.NewReader(gzf)
	for true {
		header, err := tarReader.Next()

		if err == io.EOF {
			break
		}

		if err != nil {
			return err
		}

		if isValidGrammar(header.Name, header.FileInfo()) {
			data, err := ioutil.ReadAll(tarReader)
			if err != nil {
				return err
			}

			ext := filepath.Ext(header.Name)
			rule, uk, err := ConvertProto(ext, data)
			if err != nil {
				return &ConversionError{err, header.Name}
			}

			if _, ok := loaded[rule.ScopeName]; ok {
				continue
			}

			loaded[rule.ScopeName] = Loaded{header.Name, rule, uk}
		}
	}

	return nil
}

func (l *urlLoader) Load(loaded map[string]Loaded) error {
	res, err := http.Get(l.url)
	if err != nil {
		return err
	}
	defer res.Body.Close()

	if strings.HasSuffix(l.url, ".tar.gz") {
		return l.loadTarball(res.Body, loaded)
	}

	data, err := ioutil.ReadAll(res.Body)
	if err != nil {
		return err
	}

	ext := filepath.Ext(l.url)
	rule, unknown, err := ConvertProto(ext, data)
	if err != nil {
		return &ConversionError{err, l.url}
	}

	loaded[rule.ScopeName] = Loaded{l.url, rule, unknown}
	return nil
}
