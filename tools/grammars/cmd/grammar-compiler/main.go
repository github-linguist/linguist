package main

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"reflect"
	"runtime"
	"strings"
	"sync"

	"github.com/github/linguist/tools/grammars/progress"
	grammar "github.com/github/linguist/tools/grammars/proto"
	"github.com/golang/protobuf/proto"
	"github.com/groob/plist"
	"github.com/mitchellh/mapstructure"
	"gopkg.in/yaml.v2"
)

var GrammarAliases = map[string]string{
	"source.erb":         "text.html.erb",
	"source.cpp":         "source.c++",
	"source.less":        "source.css.less",
	"text.html.markdown": "source.gfm",
	"text.md":            "source.gfm",
	"source.php":         "text.html.php",
	"text.plain":         "",
	"source.asciidoc":    "text.html.asciidoc",
	"source.perl6":       "source.perl6fe",
	"source.css.scss":    "source.scss",
}

var KnownFields = map[string]bool{
	"comment":            true,
	"uuid":               true,
	"author":             true,
	"comments":           true,
	"macros":             true,
	"fileTypes":          true,
	"firstLineMatch":     true,
	"keyEquivalent":      true,
	"foldingStopMarker":  true,
	"foldingStartMarker": true,
	"foldingEndMarker":   true,
	"limitLineLength":    true,
}

type ConversionError struct {
	Conv   error
	Source string
}

func (err *ConversionError) Error() string {
	return fmt.Sprintf("conversion failed\n\tfile: %s\n\terror: %s", err.Source, err.Conv)
}

type MissingScopeError struct {
	Scope  string
	Source string
}

func (err *MissingScopeError) Error() string {
	return fmt.Sprintf("missing scope in grammar bundle: '%s' was expected\n\tfile: %s", err.Scope, err.Source)
}

type UnexpectedScopeError struct {
	Scope  string
	Source string
}

func (err *UnexpectedScopeError) Error() string {
	return fmt.Sprintf("unexpected scope in grammar bundle: '%s'\n\tfile: %s", err.Scope, err.Source)
}

type MissingIncludeError struct {
	Include string
	Source  string
}

func (err *MissingIncludeError) Error() string {
	return fmt.Sprintf("scope '%s' cannot be found in library\n\tincluded from grammar: %s", err.Include, err.Source)
}

type UnknownKeysError struct {
	Keys   []string
	Source string
}

func (err *UnknownKeysError) Error() string {
	return fmt.Sprintf("unknown keys in grammar\n\tfile: %s\n\tkeys: %+v", err.Source, err.Keys)
}

type Loader interface {
	Load() ([]*grammar.Rule, error)
	UnknownKeys() map[string][]string
}

type fsLoader struct {
	path    string
	unknown map[string][]string
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

func (l *fsLoader) Load() (files []*grammar.Rule, err error) {
	grammars, err := l.findGrammars()
	if err != nil {
		return nil, err
	}

	var rules []*grammar.Rule
	for _, path := range grammars {
		data, err := ioutil.ReadFile(path)
		if err != nil {
			return nil, err
		}

		rule, unknown, err := ConvertProto(filepath.Ext(path), data)
		if err != nil {
			return nil, &ConversionError{err, path}
		}

		rules = append(rules, rule)
		l.unknown[path] = unknown
	}

	return rules, nil
}

func (l *fsLoader) UnknownKeys() map[string][]string {
	return l.unknown
}

type urlLoader struct {
	url     string
	unknown map[string][]string
}

func (l *urlLoader) loadTarball(r io.Reader) ([]*grammar.Rule, error) {
	gzf, err := gzip.NewReader(r)
	if err != nil {
		return nil, err
	}
	defer gzf.Close()

	var rules []*grammar.Rule
	tarReader := tar.NewReader(gzf)

	for true {
		header, err := tarReader.Next()

		if err == io.EOF {
			break
		}

		if err != nil {
			return nil, err
		}

		if isValidGrammar(header.Name, header.FileInfo()) {
			data, err := ioutil.ReadAll(tarReader)
			if err != nil {
				return nil, err
			}

			ext := filepath.Ext(header.Name)
			rule, uk, err := ConvertProto(ext, data)
			if err != nil {
				return nil, &ConversionError{err, header.Name}
			}

			rules = append(rules, rule)
			l.unknown[header.Name] = uk
		}
	}

	return rules, nil
}

func (l *urlLoader) Load() ([]*grammar.Rule, error) {
	res, err := http.Get(l.url)
	if err != nil {
		return nil, err
	}
	defer res.Body.Close()

	if strings.HasSuffix(l.url, ".tar.gz") {
		return l.loadTarball(res.Body)
	}

	data, err := ioutil.ReadAll(res.Body)
	if err != nil {
		return nil, err
	}

	ext := filepath.Ext(l.url)
	rule, unknown, err := ConvertProto(ext, data)
	if err != nil {
		return nil, &ConversionError{err, l.url}
	}

	l.unknown[l.url] = unknown
	return []*grammar.Rule{rule}, nil
}

func (l *urlLoader) UnknownKeys() map[string][]string {
	return l.unknown
}

func ConvertCSON(data []byte) ([]byte, error) {
	stdin := bytes.NewBuffer(data)
	stdout := &bytes.Buffer{}

	cmd := exec.Command("csonc")
	cmd.Stdin = stdin
	cmd.Stdout = stdout

	if err := cmd.Run(); err != nil {
		return nil, err
	}

	return stdout.Bytes(), nil
}

func looseDecoder(f reflect.Kind, t reflect.Kind, data interface{}) (interface{}, error) {
	dataVal := reflect.ValueOf(data)
	switch t {
	case reflect.Bool:
		switch f {
		case reflect.Bool:
			return dataVal.Bool(), nil
		case reflect.Float32, reflect.Float64:
			return (int(dataVal.Float()) != 0), nil
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			return (dataVal.Int() != 0), nil
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			return (dataVal.Uint() != 0), nil
		case reflect.String:
			switch dataVal.String() {
			case "1":
				return true, nil
			case "0":
				return false, nil
			}
		}
	}

	return data, nil
}

func filterUnusedKeys(keys []string) (out []string) {
	for _, k := range keys {
		parts := strings.Split(k, ".")
		field := parts[len(parts)-1]
		if !KnownFields[field] {
			out = append(out, k)
		}
	}
	return
}

func ConvertProto(ext string, data []byte) (*grammar.Rule, []string, error) {
	var (
		raw map[string]interface{}
		out grammar.Rule
		err error
		md  mapstructure.Metadata
	)

	switch strings.ToLower(ext) {
	case ".plist", ".tmlanguage":
		err = plist.Unmarshal(data, &raw)
	case ".yaml-tmlanguage":
		err = yaml.Unmarshal(data, &raw)
	case ".cson":
		data, err = ConvertCSON(data)
		if err == nil {
			err = json.Unmarshal(data, &raw)
		}
	case ".json":
		err = json.Unmarshal(data, &raw)
	default:
		err = fmt.Errorf("grammars: unsupported extension '%s'", ext)
	}

	if err != nil {
		return nil, nil, err
	}

	config := mapstructure.DecoderConfig{
		Result:     &out,
		Metadata:   &md,
		DecodeHook: looseDecoder,
	}

	decoder, err := mapstructure.NewDecoder(&config)
	if err != nil {
		return nil, nil, err
	}

	if err := decoder.Decode(raw); err != nil {
		return nil, nil, err
	}

	return &out, filterUnusedKeys(md.Unused), nil
}

type job struct {
	src    string
	scopes []string
}

type Converter struct {
	Root string

	errors   []error
	progress *progress.ProgressBar
	wg       sync.WaitGroup
	queue    chan job
	library  grammar.Library
	mu       sync.Mutex
}

func (conv *Converter) NewLoader(src string) Loader {
	if strings.HasPrefix(src, "http://") || strings.HasPrefix(src, "https://") {
		return &urlLoader{src, make(map[string][]string)}
	}

	src = path.Join(conv.Root, src)
	return &fsLoader{src, make(map[string][]string)}
}

func compareScopes(src string, found []*grammar.Rule, expected []string) (errors []error) {
	foundM := make(map[string]bool)
	expectM := make(map[string]bool)

	for _, v := range found {
		foundM[v.ScopeName] = true
	}
	for _, v := range expected {
		expectM[v] = true
	}
	for scope := range foundM {
		if !expectM[scope] {
			errors = append(errors, &UnexpectedScopeError{scope, src})
		}
	}
	for scope := range expectM {
		if !foundM[scope] {
			errors = append(errors, &MissingScopeError{scope, src})
		}
	}
	return
}

func (conv *Converter) work() {
	var errors []error
	grammars := make(map[string]*grammar.Rule)

	for j := range conv.queue {
		loader := conv.NewLoader(j.src)

		rules, err := loader.Load()
		if err != nil {
			errors = append(errors, err)
			continue
		}

		for _, r := range rules {
			grammars[r.ScopeName] = r
		}

		for src, keys := range loader.UnknownKeys() {
			if len(keys) > 0 {
				errors = append(errors, &UnknownKeysError{keys, src})
			}
		}

		errors = append(errors, compareScopes(j.src, rules, j.scopes)...)
		conv.progress.Update(len(rules))
	}

	conv.mu.Lock()
	conv.errors = append(conv.errors, errors...)
	for scope, rule := range grammars {
		conv.library.Grammars[scope] = rule
	}
	conv.mu.Unlock()

	conv.wg.Done()
}

func countScopes(grammars map[string][]string) (count int) {
	for _, v := range grammars {
		count += len(v)
	}
	return
}

func (conv *Converter) readGrammarYML() (grammars map[string][]string, err error) {
	var yml []byte

	yml, err = ioutil.ReadFile(path.Join(conv.Root, "grammars.yml"))
	if err != nil {
		return
	}

	err = yaml.Unmarshal(yml, &grammars)
	return
}

func (conv *Converter) AddGrammar(source string) error {
	grammars, err := conv.readGrammarYML()
	if err != nil {
		return err
	}

	if _, ok := grammars[source]; ok {
		// replace scopes
		grammars[source] = nil
	}

	loader := conv.NewLoader(source)
	rules, err := loader.Load()
	if err != nil {
		return err
	}

	var scopes []string
	for _, r := range rules {
		scopes = append(scopes, r.ScopeName)
	}
	grammars[source] = scopes

	outyml, err := yaml.Marshal(grammars)
	if err != nil {
		return err
	}

	ymlpath := path.Join(conv.Root, "grammars.yml")
	if err := ioutil.WriteFile(ymlpath, outyml, 0666); err != nil {
		return err
	}

	fmt.Printf("OK! added grammar source '%s' to grammars.yml\n", source)
	for _, scope := range scopes {
		fmt.Printf("\tnew scope: %s\n", scope)
	}

	return nil
}

func (conv *Converter) ConvertGrammars() error {
	grammars, err := conv.readGrammarYML()
	if err != nil {
		return err
	}

	scopeCount := countScopes(grammars)
	progress, err := progress.NewProgressBar(scopeCount)
	if err != nil {
		return err
	}

	conv.progress = progress
	conv.library.Grammars = make(map[string]*grammar.Rule)
	conv.queue = make(chan job, 128)

	for i := 0; i < runtime.NumCPU(); i++ {
		conv.wg.Add(1)
		go conv.work()
	}

	for src, scopes := range grammars {
		conv.queue <- job{src, scopes}
	}

	close(conv.queue)
	conv.wg.Wait()
	conv.progress.Done()

	conv.resolveIncludes()
	return nil
}

func (conv *Converter) WriteProto(path string) error {
	pb, err := proto.Marshal(&conv.library)
	if err != nil {
		return err
	}

	return ioutil.WriteFile(path, pb, 0666)
}

func (conv *Converter) writeJSONFile(path string, rule *grammar.Rule) error {
	j, err := os.Create(path)
	if err != nil {
		return err
	}
	defer j.Close()

	enc := json.NewEncoder(j)
	enc.SetIndent("", "  ")
	return enc.Encode(rule)
}

func (conv *Converter) WriteJSON(rulePath string) error {
	if err := os.MkdirAll(rulePath, os.ModePerm); err != nil {
		return err
	}

	for scope, rule := range conv.library.Grammars {
		p := path.Join(rulePath, scope+".json")
		if err := conv.writeJSONFile(p, rule); err != nil {
			return err
		}
	}

	return nil
}

func (conv *Converter) checkInclude(missing map[string]bool, rule *grammar.Rule) {
	include := rule.Include

	if include == "" || include[0] == '#' || include[0] == '$' {
		return
	}

	if alias, ok := GrammarAliases[include]; ok {
		rule.Include = alias
		return
	}

	include = strings.Split(include, "#")[0]
	_, ok := conv.library.Grammars[include]
	if !ok {
		missing[include] = true
		rule.Include = ""
	}
}

func (conv *Converter) resolveIncludesForRule(missing map[string]bool, rule *grammar.Rule) {
	conv.checkInclude(missing, rule)

	for _, rule := range rule.Patterns {
		conv.resolveIncludesForRule(missing, rule)
	}
	for _, rule := range rule.Captures {
		conv.resolveIncludesForRule(missing, rule)
	}
	for _, rule := range rule.BeginCaptures {
		conv.resolveIncludesForRule(missing, rule)
	}
	for _, rule := range rule.WhileCaptures {
		conv.resolveIncludesForRule(missing, rule)
	}
	for _, rule := range rule.EndCaptures {
		conv.resolveIncludesForRule(missing, rule)
	}
	for _, rule := range rule.Repository {
		conv.resolveIncludesForRule(missing, rule)
	}
	for _, rule := range rule.Injections {
		conv.resolveIncludesForRule(missing, rule)
	}
}

func (conv *Converter) resolveIncludes() {
	for scope, rule := range conv.library.Grammars {
		missing := make(map[string]bool)
		conv.resolveIncludesForRule(missing, rule)

		for include := range missing {
			conv.errors = append(conv.errors, &MissingIncludeError{include, scope})
		}
	}
}

func (conv *Converter) Report() {
	fmt.Printf("done! %d scopes converted (%d errors)\n",
		len(conv.library.Grammars), len(conv.errors))

	for _, err := range conv.errors {
		fmt.Printf("- %s\n\n", err)
	}
}

var linguistRoot = flag.String("linguist", "", "path to Linguist installation")
var protoOut = flag.String("proto", "", "dump Protobuf library")
var jsonOut = flag.String("json", "", "dump JSON output")

func main() {
	flag.Parse()

	if _, err := exec.LookPath("csonc"); err != nil {
		panic(err)
	}

	if *linguistRoot == "" {
		ex, err := os.Executable()
		if err != nil {
			panic(err)
		}
		*linguistRoot = path.Join(filepath.Dir(ex), "../../")
	}

	conv := Converter{Root: *linguistRoot}
	if err := conv.ConvertGrammars(); err != nil {
		panic(err)
	}

	if *protoOut != "" {
		if err := conv.WriteProto(*protoOut); err != nil {
			panic(err)
		}
	}

	if *jsonOut != "" {
		if err := conv.WriteJSON(*jsonOut); err != nil {
			panic(err)
		}
	}

	conv.Report()
}
