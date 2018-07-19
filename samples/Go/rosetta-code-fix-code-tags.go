package main

import "fmt"
import "io/ioutil"
import "log"
import "os"
import "regexp"
import "strings"

func main() {
	err := fix()
	if err != nil {
		log.Fatalln(err)
	}
}

func fix() (err error) {
	buf, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		return err
	}
	out, err := Lang(string(buf))
	if err != nil {
		return err
	}
	fmt.Println(out)
	return nil
}

func Lang(in string) (out string, err error) {
	reg := regexp.MustCompile("<[^>]+>")
	out = reg.ReplaceAllStringFunc(in, repl)
	return out, nil
}

func repl(in string) (out string) {
	if in == "</code>" {
		// Change </code> to </ lang>
		return "</"+"lang>"
	}

	// mid is the content in between '<' and '>'.
	mid := in[1 : len(in)-1]

	// thanks, random lua guy
	var langs = []string{
		"abap", "actionscript", "actionscript3", "ada", "apache", "applescript",
		"apt_sources", "asm", "asp", "autoit", "avisynth", "bash", "basic4gl",
		"bf", "blitzbasic", "bnf", "boo", "c", "caddcl", "cadlisp", "cfdg", "cfm",
		"cil", "c_mac", "cobol", "cpp", "cpp-qt", "csharp", "css", "d", "delphi",
		"diff", "_div", "dos", "dot", "eiffel", "email", "fortran", "freebasic",
		"genero", "gettext", "glsl", "gml", "gnuplot", "go", "groovy", "haskell",
		"hq9plus", "html4strict", "idl", "ini", "inno", "intercal", "io", "java",
		"java5", "javascript", "kixtart", "klonec", "klonecpp", "latex", "lisp",
		"lolcode", "lotusformulas", "lotusscript", "lscript", "lua", "m68k",
		"make", "matlab", "mirc", "modula3", "mpasm", "mxml", "mysql", "nsis",
		"objc", "ocaml", "ocaml-brief", "oobas", "oracle11", "oracle8", "pascal",
		"per", "perl", "php", "php-brief", "pic16", "pixelbender", "plsql",
		"povray", "powershell", "progress", "prolog", "providex", "python",
		"qbasic", "rails", "reg", "robots", "ruby", "sas", "scala", "scheme",
		"scilab", "sdlbasic", "smalltalk", "smarty", "sql", "tcl", "teraterm",
		"text", "thinbasic", "tsql", "typoscript", "vb", "vbnet", "verilog",
		"vhdl", "vim", "visualfoxpro", "visualprolog", "whitespace", "winbatch",
		"xml", "xorg_conf", "xpp", "z80",
	}
	for _, lang := range langs {
		if mid == lang {
			// Change <%s> to <lang %s>
			return fmt.Sprintf("<lang %s>", lang)
		}

		if strings.HasPrefix(mid, "/") {
			if mid[len("/"):] == lang {
				// Change </%s> to </ lang>
				return "</"+"lang>"
			}
		}

		if strings.HasPrefix(mid, "code ") {
			if mid[len("code "):] == lang {
				// Change <code %s> to <lang %s>
				return fmt.Sprintf("<lang %s>", lang)
			}
		}
	}

	return in
}
