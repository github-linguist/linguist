package compiler

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"runtime"
	"sort"
	"strings"
	"sync"

	grammar "github.com/github/linguist/tools/grammars/proto"
	"github.com/golang/protobuf/proto"
	pb "gopkg.in/cheggaaa/pb.v1"
	yaml "gopkg.in/yaml.v2"
)

type Converter struct {
	root string

	grammars    map[string][]string
	newGrammars map[string][]string
	modified    bool

	errors   []error
	progress *pb.ProgressBar
	wg       sync.WaitGroup
	queue    chan string
	library  grammar.Library
	mu       sync.Mutex
}

func (conv *Converter) NewLoader(src string) Loader {
	if strings.HasPrefix(src, "http://") || strings.HasPrefix(src, "https://") {
		return &urlLoader{src}
	}

	src = path.Join(conv.root, src)
	return &fsLoader{src}
}

func (conv *Converter) work() {
	var (
		localErrors []error
		localScopes = make(map[string][]string)
		localRules  = make(map[string]*grammar.Rule)
	)

	for source := range conv.queue {
		loader := conv.NewLoader(source)
		rules := make(map[string]Loaded)

		err := loader.Load(rules)
		if err != nil {
			localErrors = append(localErrors, err)
			continue
		}

		var scopes []string
		for scope, l := range rules {
			if len(l.unknownKeys) > 0 {
				localErrors = append(localErrors, &UnknownKeysError{l.unknownKeys, l.source})
			}
			scopes = append(scopes, scope)
			localRules[scope] = l.rule
		}

		sort.Strings(scopes)
		localScopes[source] = scopes

		conv.progress.Increment()
	}

	conv.mu.Lock()
	for source, scopes := range localScopes {
		conv.newGrammars[source] = scopes
	}
	for scope, rule := range localRules {
		conv.library.Grammars[scope] = rule
	}
	conv.errors = append(conv.errors, localErrors...)
	conv.mu.Unlock()

	conv.wg.Done()
}

func (conv *Converter) AddGrammar(source string) error {
	if _, ok := conv.grammars[source]; ok {
		// replace scopes
		conv.grammars[source] = nil
	}

	loader := conv.NewLoader(source)
	rules := make(map[string]Loaded)

	err := loader.Load(rules)
	if err != nil {
		return err
	}
	if len(rules) == 0 {
		return fmt.Errorf("source '%s' contains no grammar files", source)
	}

	var scopes []string
	for scope := range rules {
		scopes = append(scopes, scope)
	}
	sort.Strings(scopes)
	conv.grammars[source] = scopes
	conv.modified = true

	fmt.Printf("OK! added grammar source '%s'\n", source)
	for _, scope := range scopes {
		fmt.Printf("\tnew scope: %s\n", scope)
	}

	return nil
}

func compareScopes(src string, expected []string, found []string) (errors []error) {
	foundM := make(map[string]bool)
	expectM := make(map[string]bool)

	for _, v := range found {
		foundM[v] = true
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

func (conv *Converter) compareGrammarLists() {
	for source, expected := range conv.grammars {
		found := conv.newGrammars[source]
		errors := compareScopes(source, expected, found)
		conv.errors = append(conv.errors, errors...)
	}
}

func (conv *Converter) ConvertGrammars(update bool) error {
	conv.progress = pb.New(len(conv.grammars))
	conv.progress.Start()

	conv.newGrammars = make(map[string][]string)
	conv.library.Grammars = make(map[string]*grammar.Rule)
	conv.queue = make(chan string, 128)

	for i := 0; i < runtime.NumCPU(); i++ {
		conv.wg.Add(1)
		go conv.work()
	}

	for src := range conv.grammars {
		conv.queue <- src
	}

	close(conv.queue)
	conv.wg.Wait()
	conv.walkGrammars()

	if update {
		conv.grammars = conv.newGrammars
		conv.modified = true
	} else {
		conv.compareGrammarLists()
	}

	done := fmt.Sprintf("done! %d scopes converted from %d grammars (%d errors)\n",
		len(conv.library.Grammars), len(conv.grammars), len(conv.errors))
	conv.progress.FinishPrint(done)

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

func (conv *Converter) checkInclude(scope string, missing map[string]bool, rule *grammar.Rule) {
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
		if !missing[include] {
			missing[include] = true
			conv.errors = append(conv.errors, &MissingIncludeError{include, scope})
		}
		rule.Include = ""
	}
}

func (conv *Converter) checkRegexps(scope string, rule *grammar.Rule) {
	check := func(re string) string {
		re2, err := CheckPCRE(re)
		if err != nil {
			conv.errors = append(conv.errors, &InvalidRegexError{err, scope})
		}
		return re2
	}

	rule.Match = check(rule.Match)
	rule.Begin = check(rule.Begin)
	rule.While = check(rule.While)
	rule.End = check(rule.End)
}

func (conv *Converter) walkRule(scope string, missing map[string]bool, rule *grammar.Rule) {
	conv.checkInclude(scope, missing, rule)
	conv.checkRegexps(scope, rule)

	for _, rule := range rule.Patterns {
		conv.walkRule(scope, missing, rule)
	}
	for _, rule := range rule.Captures {
		conv.walkRule(scope, missing, rule)
	}
	for _, rule := range rule.BeginCaptures {
		conv.walkRule(scope, missing, rule)
	}
	for _, rule := range rule.WhileCaptures {
		conv.walkRule(scope, missing, rule)
	}
	for _, rule := range rule.EndCaptures {
		conv.walkRule(scope, missing, rule)
	}
	for _, rule := range rule.Repository {
		conv.walkRule(scope, missing, rule)
	}
	for _, rule := range rule.Injections {
		conv.walkRule(scope, missing, rule)
	}
}

func (conv *Converter) walkGrammars() {
	for scope, rule := range conv.library.Grammars {
		missing := make(map[string]bool)
		conv.walkRule(scope, missing, rule)
	}
}

func (conv *Converter) WriteGrammarList() error {
	if !conv.modified {
		return nil
	}

	outyml, err := yaml.Marshal(conv.grammars)
	if err != nil {
		return err
	}

	ymlpath := path.Join(conv.root, "grammars.yml")
	return ioutil.WriteFile(ymlpath, outyml, 0666)
}

func (conv *Converter) Report() {
	for _, err := range conv.errors {
		fmt.Printf("- %s\n\n", err)
	}
}

func NewConverter(root string) (*Converter, error) {
	yml, err := ioutil.ReadFile(path.Join(root, "grammars.yml"))
	if err != nil {
		return nil, err
	}

	conv := &Converter{root: root}

	if err := yaml.Unmarshal(yml, &conv.grammars); err != nil {
		return nil, err
	}

	return conv, nil
}
