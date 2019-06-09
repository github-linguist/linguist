package compiler

import "fmt"
import "strings"

type ConversionError struct {
	Path string
	Err  error
}

func (err *ConversionError) Error() string {
	return fmt.Sprintf(
		"Grammar conversion failed. File `%s` failed to parse: %s",
		err.Path, err.Err)
}

type DuplicateScopeError struct {
	Original  *Repository
	Duplicate string
}

func (err *DuplicateScopeError) Error() string {
	return fmt.Sprintf(
		"Duplicate scope in repository: scope `%s` was already defined in %s",
		err.Duplicate, err.Original)
}

type MissingScopeError struct {
	Scope string
}

func (err *MissingScopeError) Error() string {
	return fmt.Sprintf(
		"Missing scope in repository: `%s` is listed in grammars.yml but cannot be found",
		err.Scope)
}

type UnexpectedScopeError struct {
	File  *LoadedFile
	Scope string
}

func (err *UnexpectedScopeError) Error() string {
	return fmt.Sprintf(
		"Unexpected scope in repository: `%s` declared in %s was not listed in grammars.yml",
		err.Scope, err.File)
}

type MissingIncludeError struct {
	File    *LoadedFile
	Include string
}

func (err *MissingIncludeError) Error() string {
	return fmt.Sprintf(
		"Missing include in grammar: %s attempts to include `%s` but the scope cannot be found",
		err.File, err.Include)
}

type UnknownKeysError struct {
	File *LoadedFile
	Keys []string
}

func (err *UnknownKeysError) Error() string {
	var keys []string
	for _, k := range err.Keys {
		keys = append(keys, fmt.Sprintf("`%s`", k))
	}

	return fmt.Sprintf(
		"Unknown keys in grammar: %s contains invalid keys (%s)",
		err.File, strings.Join(keys, ", "))
}

type InvalidRegexError struct {
	File *LoadedFile
	Err  error
}

func (err *InvalidRegexError) Error() string {
	return fmt.Sprintf(
		"Invalid regex in grammar: %s contains a malformed regex (%s)",
		err.File, err.Err)
}

type UndeclaredScopeError struct {
	Path string
}

func (err *UndeclaredScopeError) Error() string {
	return fmt.Sprintf(
		"Undeclared scope in grammar: `%s` has no scope name",
		err.Path)
}
