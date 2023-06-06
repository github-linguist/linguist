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
	root    string
	version string

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

func (conv *Converter) tmpScopes() map[string]bool {
	scopes := make(map[string]bool)
	for _, ary := range conv.grammars {
		for _, s := range ary {
			scopes[s] = true
		}
	}
	return scopes
}

func (conv *Converter) AddGrammar(source string) error {
	repo := conv.Load(source)
	if len(repo.Files) == 0 && len(repo.Errors) == 0 {
		return fmt.Errorf("source '%s' contains no grammar files", source)
	}

	conv.grammars[source] = repo.Scopes()
	conv.modified = true

	knownScopes := conv.tmpScopes()
	repo.FixRules(knownScopes)

	if len(repo.Errors) > 0 {
		// Split out warnings from errors
		warnings := []error{}
		errors := []error{}
		for _, e := range repo.Errors {
			switch e.(type) {
			case *MissingIncludeError, *UnknownKeysError:
				warnings = append(warnings, e)
			default:
				errors = append(errors, e)
			}
		}
		if len(errors) > 0 {
			fmt.Fprintf(os.Stderr, "%d errors found in new grammar '%s':\n",
				len(errors), repo)
			for _, err := range errors {
				fmt.Fprintf(os.Stderr, "- %s\n", err)
			}
			fmt.Fprintf(os.Stderr, "\n")
			return fmt.Errorf("Compilation failed. Aborting")
		}
		if len(warnings) > 0 {
			fmt.Fprintf(os.Stderr, "%d warnings found when compiling new grammar '%s':\n",
				len(warnings), repo)
			for _, err := range warnings {
				fmt.Fprintf(os.Stderr, "- %s\n", err)
			}
			fmt.Fprintf(os.Stderr, "\n")
			fmt.Fprintf(os.Stderr, "These warnings are not fatal, but may mean the syntax highlighting on GitHub.com may not be as expected.\n\n")
		}
	}

	fmt.Printf("OK! added grammar source '%s'\n", source)
	for scope := range repo.Files {
		fmt.Printf("\tnew scope: %s\n", scope)
	}
	return nil
}

func (conv *Converter) AllScopes() map[string]bool {
	// Map from scope -> Repository first to error check
	// possible duplicates
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

	// Convert to scope -> bool
	scopes := make(map[string]bool)
	for s := range allScopes {
		scopes[s] = true
	}
	return scopes
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

	knownScopes := conv.AllScopes()

	for source, repo := range conv.Loaded {
		repo.FixRules(knownScopes)

		if update {
			scopes := repo.Scopes()
			if len(scopes) > 0 {
				conv.grammars[source] = scopes
			}
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
	return enc.Encode(rule)
}

func (conv *Converter) WriteJSON(rulePath string) error {
	if err := os.MkdirAll(rulePath, os.ModePerm); err != nil {
		return err
	}

	f, err := os.Create(path.Join(rulePath, "version"))
	if err != nil {
		return err
	}
	f.Write([]byte(conv.version))
	f.Close()

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

func (conv *Converter) Report(verbose bool) error {
	var failed []*Repository
	for _, repo := range conv.Loaded {
		if len(repo.Errors) > 0 {
			failed = append(failed, repo)
		}
	}

	sort.Slice(failed, func(i, j int) bool {
		return failed[i].Source < failed[j].Source
	})

	total := 0
	for _, repo := range failed {
		n := 0
		// Only show warning-like errors in verbose output
		if ! verbose {
			for _, err := range repo.Errors {
				switch err.(type) {
				case *MissingIncludeError, *UnknownKeysError:
					break
				default:
					repo.Errors[n] = err
					n++
				}
			}
			repo.Errors = repo.Errors[:n]
		}
		if len(repo.Errors) > 0 {
			fmt.Fprintf(os.Stderr, "- [ ] %s (%d errors)\n", repo, len(repo.Errors))
			for _, err := range repo.Errors {
				fmt.Fprintf(os.Stderr, "  - %s\n", err)
			}
			fmt.Fprintf(os.Stderr, "\n")
		}
		total += len(repo.Errors)
	}

	if total > 0 {
		return fmt.Errorf("The grammar library contains %d errors", total)
	}
	return nil
}

func NewConverter(root string) (*Converter, error) {
	ver, err := ioutil.ReadFile(path.Join(root, "lib", "linguist", "VERSION"))
	if err != nil {
		return nil, err
	}

	yml, err := ioutil.ReadFile(path.Join(root, "grammars.yml"))
	if err != nil {
		return nil, err
	}

	conv := &Converter{root: root, version: strings.TrimSpace(string(ver))}

	if err := yaml.Unmarshal(yml, &conv.grammars); err != nil {
		return nil, err
	}

	return conv, nil
}
