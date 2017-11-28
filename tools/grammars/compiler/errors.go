package compiler

import "fmt"

type ConversionError struct {
	Conv   error
	Source string
}

func (err *ConversionError) Error() string {
	return fmt.Sprintf("conversion failed\n\tfile: %s\n\terror: %s", err.Source, err.Conv)
}

type MissingScopeError struct {
	Scope  string
	Source string
}

func (err *MissingScopeError) Error() string {
	return fmt.Sprintf("missing scope in grammar bundle: '%s' was expected\n\tfile: %s", err.Scope, err.Source)
}

type UnexpectedScopeError struct {
	Scope  string
	Source string
}

func (err *UnexpectedScopeError) Error() string {
	return fmt.Sprintf("unexpected scope in grammar bundle: '%s'\n\tfile: %s", err.Scope, err.Source)
}

type MissingIncludeError struct {
	Include string
	Source  string
}

func (err *MissingIncludeError) Error() string {
	return fmt.Sprintf("scope '%s' cannot be found in library\n\tincluded from grammar: %s", err.Include, err.Source)
}

type UnknownKeysError struct {
	Keys   []string
	Source string
}

func (err *UnknownKeysError) Error() string {
	return fmt.Sprintf("unknown keys in grammar\n\tfile: %s\n\tkeys: %+v", err.Source, err.Keys)
}

type InvalidRegexError struct {
	RegexError error
	Source     string
}

func (err *InvalidRegexError) Error() string {
	return fmt.Sprintf("invalid regex in grammar %s\n\t%s", err.Source, err.RegexError)
}
