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

	modified bool
	grammars map[string][]string
	Loaded   map[string]*Repository

	progress *pb.ProgressBar
	wg       sync.WaitGroup
	queue    chan string
	mu       sync.Mutex
}

func (conv *Converter) Load(src string) *Repository {
	if strings.HasPrefix(src, "http://") || strings.HasPrefix(src, "https://") {
		return LoadFromURL(src)
	}
	return LoadFromFilesystem(conv.root, src)
}

func (conv *Converter) work() {
	for source := range conv.queue {
		repo := conv.Load(source)

		conv.mu.Lock()
		conv.Loaded[source] = repo
		conv.mu.Unlock()

		conv.progress.Increment()
	}

	conv.wg.Done()
}

func (conv *Converter) AddGrammar(source string) error {
	repo := conv.Load(source)
	if len(repo.Files) == 0 {
		return fmt.Errorf("source '%s' contains no grammar files", source)
	}

	conv.grammars[source] = repo.Scopes()
	conv.modified = true

	fmt.Printf("OK! added grammar source '%s'\n", source)
	for scope := range repo.Files {
		fmt.Printf("\tnew scope: %s\n", scope)
	}

	return nil
}

func (conv *Converter) ScopeMap() map[string]*Repository {
	allScopes := make(map[string]*Repository)

	for _, repo := range conv.Loaded {
		for scope := range repo.Files {
			if original := allScopes[scope]; original != nil {
				repo.Fail(&DuplicateScopeError{original, scope})
			} else {
				allScopes[scope] = repo
			}
		}
	}

	return allScopes
}

func (conv *Converter) ConvertGrammars(update bool) error {
	conv.Loaded = make(map[string]*Repository)
	conv.queue = make(chan string, 128)
	conv.progress = pb.New(len(conv.grammars))
	conv.progress.Start()

	for i := 0; i < runtime.NumCPU(); i++ {
		conv.wg.Add(1)
		go conv.work()
	}

	for src := range conv.grammars {
		conv.queue <- src
	}

	close(conv.queue)
	conv.wg.Wait()

	done := fmt.Sprintf("done! processed %d grammars\n", len(conv.Loaded))
	conv.progress.FinishPrint(done)

	if update {
		conv.grammars = make(map[string][]string)
		conv.modified = true
	}

	knownScopes := conv.ScopeMap()

	for source, repo := range conv.Loaded {
		repo.FixRules(knownScopes)

		if update {
			conv.grammars[source] = repo.Scopes()
		} else {
			expected := conv.grammars[source]
			repo.CompareScopes(expected)
		}
	}

	return nil
}

func (conv *Converter) WriteProto(path string) error {
	library := grammar.Library{
		Grammars: make(map[string]*grammar.Rule),
	}

	for _, repo := range conv.Loaded {
		for scope, file := range repo.Files {
			library.Grammars[scope] = file.Rule
		}
	}

	pb, err := proto.Marshal(&library)
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

	for _, repo := range conv.Loaded {
		for scope, file := range repo.Files {
			p := path.Join(rulePath, scope+".json")
			if err := conv.writeJSONFile(p, file.Rule); err != nil {
				return err
			}
		}
	}

	return nil
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
	var failed []*Repository
	for _, repo := range conv.Loaded {
		if len(repo.Errors) > 0 {
			failed = append(failed, repo)
		}
	}

	sort.Slice(failed, func(i, j int) bool {
		return failed[i].Source < failed[j].Source
	})

	for _, repo := range failed {
		fmt.Printf("- %s (%d errors)\n", repo, len(repo.Errors))
		for i, err := range repo.Errors {
			fmt.Printf("    %d. %s\n", i+1, err)
		}
		fmt.Printf("\n")
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
