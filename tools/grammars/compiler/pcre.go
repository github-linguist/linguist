package compiler

import (
	"fmt"

	"github.com/github/linguist/tools/grammars/pcre"
)

type replacement struct {
	pos int
	len int
	val string
}

func fixRegex(re string) (string, bool) {
	var (
		replace     []replacement
		escape      = false
		hasBackRefs = false
	)

	for i, ch := range re {
		if escape {
			if ch == 'h' {
				replace = append(replace, replacement{i - 1, 2, "[[:xdigit:]]"})
			}
			if '0' <= ch && ch <= '9' {
				hasBackRefs = true
			}
		}
		escape = !escape && ch == '\\'
	}

	if len(replace) > 0 {
		reb := []byte(re)
		offset := 0
		for _, repl := range replace {
			reb = append(
				reb[:offset+repl.pos],
				append([]byte(repl.val), reb[offset+repl.pos+repl.len:]...)...)
			offset += len(repl.val) - repl.len
		}
		return string(reb), hasBackRefs
	}

	return re, hasBackRefs
}

func CheckPCRE(re string) (string, error) {
	hasBackRefs := false

	if re == "" {
		return "", nil
	}
	if len(re) > 32*1024 {
		return "", fmt.Errorf(
			"regex %s: definition too long (%d bytes)",
			pcre.RegexPP(re), len(re))
	}

	re, hasBackRefs = fixRegex(re)
	if !hasBackRefs {
		if err := pcre.CheckRegexp(re, pcre.DefaultFlags); err != nil {
			return "", err
		}
	}
	return re, nil
}
