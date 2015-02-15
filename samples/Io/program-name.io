#!/usr/bin/env io

main := method(
	program := System args at(0)

	("Program: " .. program) println
)

if (System args size > 0 and System args at(0) containsSeq("scriptname"), main)
