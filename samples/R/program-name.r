#!/usr/bin/env Rscript

getProgram <- function(args) {
	sub("--file=", "", args[grep("--file=", args)])
}

args <- commandArgs(trailingOnly = FALSE)
program <- getProgram(args)

cat("Program: ", program, "\n")

q("no")
