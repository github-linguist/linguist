package compiler

import (
	"testing"
)

func Test_fixRegex(t *testing.T) {
	tests := []struct {
		re   string
		want string
	}{
		{"foobar", "foobar"},
		{`testing\h`, "testing[[:xdigit:]]"},
		{`\htest`, `[[:xdigit:]]test`},
		{`abc\hdef`, `abc[[:xdigit:]]def`},
		{`\\\htest`, `\\[[:xdigit:]]test`},
		{`\\htest`, `\\htest`},
		{`\h\h\h\h`, `[[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]]`},
		{`abc\hdef\hghi\h`, `abc[[:xdigit:]]def[[:xdigit:]]ghi[[:xdigit:]]`},
	}
	for _, tt := range tests {
		got, _ := fixRegex(tt.re)
		if got != tt.want {
			t.Errorf("fixRegex() got = %v, want %v", got, tt.want)
		}
	}
}
