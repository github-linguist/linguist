package compiler

var GrammarAliases = map[string]string{
	"source.erb":         "text.html.erb",
	"source.cpp":         "source.c++",
	"source.less":        "source.css.less",
	"text.html.markdown": "source.gfm",
	"text.md":            "source.gfm",
	"source.php":         "text.html.php",
	"text.plain":         "",
	"source.asciidoc":    "text.html.asciidoc",
	"source.perl6":       "source.raku",
	"source.scss":        "source.css.scss",
	"source.git-config":  "source.gitconfig",
	"source.smarty":      "text.html.smarty",
	"text.slm":           "text.slim",
	"text.pug":           "text.jade",
}

var KnownFields = map[string]bool{
	"$schema":               true,
	"comment":               true,
	"uuid":                  true,
	"author":                true,
	"comments":              true,
	"macros":                true,
	"fileTypes":             true,
	"firstLineMatch":        true,
	"keyEquivalent":         true,
	"foldingStopMarker":     true,
	"foldingStartMarker":    true,
	"foldingEndMarker":      true,
	"maxTokensPerLine":      true,
	"limitLineLength":       true,
	"hideFromUser":          true,
	"injectionSelector":     true,
	"swallow":               true,
	"foregroundColor":       true,
	"backgroundColor":       true,
	"increaseIndentPattern": true,
	"beginComment":          true,
	"endComment":            true,
}

// GrammarsInNonStdPath is a list of grammars known to have their syntax .cson or .json files in non-standard directories.
var GrammarsInNonStdPath = map[string]bool{
	"conllu-linguist-grammar": true,
	"hy.tmLanguage":           true,
	"abl-tmlanguage":          true,
	"avro.tmLanguage":         true,
}

// IgnoredFiles is a list of files that look like syntax files but aren't, or are known to be broken and never likely to be fixed.
var IgnoredFiles = map[string]bool{
	"ballerina-grammar/syntaxes/ballerina.monarch.json": true,
	"oz-tmbundle/Originals/Oz.tmLanguage":               true,
	"abl-tmlanguage/package-lock.json":                  true,
	"abl-tmlanguage/package.json":                       true,
	"avro.tmLanguage/package-lock.json":                 true,
	"avro.tmLanguage/package.json":                      true,
	"avro.tmLanguage/avro-avsc-json-schema.json":        true,
}
