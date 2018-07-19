#!/usr/bin/env Rscript --default-packages=methods

args <- commandArgs(trailingOnly=TRUE)

if (length(args) == 0) {
  conn <- file("notes.txt", 'r')
  cat(readLines(conn), sep="\n")
} else {
  conn <- file("notes.txt", 'a')
  cat(file=conn, date(), "\n\t", paste(args, collapse=" "), "\n", sep="")
}
close(conn)
