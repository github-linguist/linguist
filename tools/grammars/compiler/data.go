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
