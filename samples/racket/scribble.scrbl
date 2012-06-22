#lang scribble/manual
@(require scribble/bnf "utils.rkt")

@title{Scribble: The Racket Documentation Tool}

@author["Matthew Flatt" "Eli Barzilay"]

Scribble is a collection of tools for creating prose
documents---papers, books, library documentation, etc.---in HTML or
PDF (via Latex) form. More generally, Scribble helps you write
programs that are rich in textual content, whether the content is
prose to be typeset or any other form of text to be generated
programmatically.

This document is itself written using Scribble. You can see its source
at
@(let ([url "http://git.racket-lang.org/plt/tree/HEAD:/collects/scribblings/scribble"])
   (link url url)),
starting with the @filepath{scribble.scrbl} file.

@table-of-contents[]

@; ------------------------------------------------------------------------
@include-section["how-to-paper.scrbl"]
@include-section["reader.scrbl"]
@include-section["generic.scrbl"]
@include-section["plt.scrbl"]
@include-section["lp.scrbl"]
@include-section["text.scrbl"]
@include-section["internals.scrbl"]
@include-section["running.scrbl"]

@index-section[]
