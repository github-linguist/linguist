package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
	"unicode"
)

// line represents a single line in the configuration file.
type line struct {
	kind     lineKind
	option   string
	value    string
	disabled bool
}

// lineKind represents the different kinds of configuration line.
type lineKind int

const (
	_ lineKind = iota
	ignore
	parseError
	comment
	blank
	value
)

func (l line) String() string {
	switch l.kind {
	case ignore, parseError, comment, blank:
		return l.value
	case value:
		s := l.option
		if l.disabled {
			s = "; " + s
		}
		if l.value != "" {
			s += " " + l.value
		}
		return s
	}
	panic("unexpected line kind")
}

func removeDross(s string) string {
	return strings.Map(func(r rune) rune {
		if r < 32 || r > 0x7f || unicode.IsControl(r) {
			return -1
		}
		return r
	}, s)
}

func parseLine(s string) line {
	if s == "" {
		return line{kind: blank}
	}
	if s[0] == '#' {
		return line{kind: comment, value: s}
	}
	s = removeDross(s)
	fields := strings.Fields(s)
	if len(fields) == 0 {
		return line{kind: blank}
	}
	// Strip leading semicolons (but record that we found them)
	semi := false
	for len(fields[0]) > 0 && fields[0][0] == ';' {
		semi = true
		fields[0] = fields[0][1:]
	}
	// Lose the first field if it was all semicolons
	if fields[0] == "" {
		fields = fields[1:]
	}
	switch len(fields) {
	case 0:
		// This can only happen if the line starts
		// with a semicolon but has no other information
		return line{kind: ignore}
	case 1:
		return line{
			kind:     value,
			option:   strings.ToUpper(fields[0]),
			disabled: semi,
		}
	case 2:
		return line{
			kind:     value,
			option:   strings.ToUpper(fields[0]),
			value:    fields[1],
			disabled: semi,
		}
	}
	return line{kind: parseError, value: s}
}

// Config represents a "standard" configuration file.
type Config struct {
	options map[string]int		// index of each option in lines.
	lines   []line
}

// index returns the index of the given option in
// c.lines, or -1 if not found.
func (c *Config) index(option string) int {
	if i, ok := c.options[option]; ok {
		return i
	}
	return -1
}

// addLine adds a line to the config, ignoring
// duplicate options and "ignore" lines.
func (c *Config) addLine(l line) {
	switch l.kind {
	case ignore:
		return
	case value:
		if c.index(l.option) >= 0 {
			return
		}
		c.options[l.option] = len(c.lines)
		c.lines = append(c.lines, l)
	default:
		c.lines = append(c.lines, l)
	}
}

// ReadConfig reads a configuration file from path and returns it.
func ReadConfig(path string) (*Config, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	r := bufio.NewReader(f)
	c := &Config{options: make(map[string]int)}
	for {
		s, err := r.ReadString('\n')
		if s != "" {
			if err == nil {
				// strip newline unless we encountered an error without finding one.
				s = s[:len(s)-1]
			}
			c.addLine(parseLine(s))
		}
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
	}
	return c, nil
}

// Set sets an option to a value, adding the option if necessary.  If
// the option was previously disabled, it will be enabled.
func (c *Config) Set(option string, val string) {
	if i := c.index(option); i >= 0 {
		line := &c.lines[i]
		line.disabled = false
		line.value = val
		return
	}
	c.addLine(line{
		kind:   value,
		option: option,
		value:  val,
	})
}

// Enable sets the enabled status of an option. It is
// ignored if the option does not already exist.
func (c *Config) Enable(option string, enabled bool) {
	if i := c.index(option); i >= 0 {
		c.lines[i].disabled = !enabled
	}
}

// Write writes the configuration file to the writer.
func (c *Config) Write(w io.Writer) {
	for _, line := range c.lines {
		fmt.Println(line)
	}
}

func main() {
	c, err := ReadConfig("/tmp/cfg")
	if err != nil {
		log.Fatalln(err)
	}
	c.Enable("NEEDSPEELING", false)
	c.Set("SEEDSREMOVED", "")
	c.Set("NUMBEROFBANANAS", "1024")
	c.Set("NUMBEROFSTRAWBERRIES", "62000")
	c.Write(os.Stdout)
}
