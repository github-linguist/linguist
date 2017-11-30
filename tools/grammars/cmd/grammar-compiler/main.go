package main

import (
	"flag"
	"fmt"
	"os"
	"os/exec"

	"github.com/github/linguist/tools/grammars/compiler"
)

var linguistRoot = flag.String("linguist", "", "path to Linguist installation")
var protoOut = flag.String("proto", "", "dump Protobuf library")
var jsonOut = flag.String("json", "", "dump JSON output")
var addGrammar = flag.String("add", "", "add a new grammar source")
var updateList = flag.Bool("update", false, "update grammars.yml instead of verifying its contents")
var report = flag.String("report", "", "write report to file")

func fatal(err error) {
	fmt.Fprintf(os.Stderr, "FATAL: %s\n", err)
	os.Exit(1)
}

func main() {
	flag.Parse()

	if _, err := exec.LookPath("csonc"); err != nil {
		fatal(err)
	}

	if *linguistRoot == "" {
		cwd, err := os.Getwd()
		if err != nil {
			fatal(err)
		}
		*linguistRoot = cwd
	}

	conv, err := compiler.NewConverter(*linguistRoot)
	if err != nil {
		fatal(err)
	}

	if *addGrammar != "" {
		if err := conv.AddGrammar(*addGrammar); err != nil {
			fatal(err)
		}
	}

	if err := conv.ConvertGrammars(*updateList); err != nil {
		fatal(err)
	}

	if err := conv.WriteGrammarList(); err != nil {
		fatal(err)
	}

	if *protoOut != "" {
		if err := conv.WriteProto(*protoOut); err != nil {
			fatal(err)
		}
	}

	if *jsonOut != "" {
		if err := conv.WriteJSON(*jsonOut); err != nil {
			fatal(err)
		}
	}

	if *report == "" {
		conv.Report(os.Stderr)
	} else {
		f, err := os.Create(*report)
		if err != nil {
			fatal(err)
		}
		conv.Report(f)
		f.Close()
	}
}
