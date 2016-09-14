module Linguist
  # A collection of simple heuristics that can be used to better analyze languages.
  class Heuristics
    # Public: Use heuristics to detect language of the blob.
    #
    # blob               - An object that quacks like a blob.
    # possible_languages - Array of Language objects
    #
    # Examples
    #
    #   Heuristics.call(FileBlob.new("path/to/file"), [
    #     Language["Ruby"], Language["Python"]
    #   ])
    #
    # Returns an Array of languages, or empty if none matched or were inconclusive.
    def self.call(blob, candidates)
      data = blob.data

      @heuristics.each do |heuristic|
        if heuristic.matches?(blob.name)
          languages = Array(heuristic.call(data))
          return languages if languages.any? || languages.all? { |l| candidates.include?(l) }
        end
      end

      [] # No heuristics matched
    end

    # Internal: Define a new heuristic.
    #
    # languages - String names of languages to disambiguate.
    # heuristic - Block which takes data as an argument and returns a Language or nil.
    #
    # Examples
    #
    #     disambiguate ".pm" do |data|
    #       if data.include?("use strict")
    #         Language["Perl"]
    #       elsif /^[^#]+:-/.match(data)
    #         Language["Prolog"]
    #       end
    #     end
    #
    def self.disambiguate(*extensions, &heuristic)
      @heuristics << new(extensions, &heuristic)
    end

    # Internal: Array of defined heuristics
    @heuristics = []

    # Internal
    def initialize(extensions, &heuristic)
      @extensions = extensions
      @heuristic = heuristic
    end

    # Internal: Check if this heuristic matches the candidate languages.
    def matches?(filename)
      filename = filename.downcase
      @extensions.any? { |ext| filename.end_with?(ext) }
    end

    # Internal: Perform the heuristic
    def call(data)
      @heuristic.call(data)
    end

    # Common heuristics
    ObjectiveCRegex = /^\s*(@(interface|class|protocol|property|end|synchronised|selector|implementation)\b|#import\s+.+\.h[">])/

    disambiguate ".asc" do |data|
      if /^(----[- ]BEGIN|ssh-(rsa|dss)) /.match(data)
        Language["Public Key"]
      elsif /^[=-]+(\s|\n)|{{[A-Za-z]/.match(data)
        Language["AsciiDoc"]
      elsif /^(\/\/.+|((import|export)\s+)?(function|int|float|char)\s+((room|repeatedly|on|game)_)?([A-Za-z]+[A-Za-z_0-9]+)\s*[;\(])/.match(data)
        Language["AGS Script"]
      end
    end

    disambiguate ".bb" do |data|
      if /^\s*; /.match(data) || data.include?("End Function")
        Language["BlitzBasic"]
      elsif /^\s*(# |include|require)\b/.match(data)
        Language["BitBake"]
      end
    end

    disambiguate ".builds" do |data|
      if /^(\s*)(<Project|<Import|<Property|<?xml|xmlns)/i.match(data)
        Language["XML"]
      else
        Language["Text"]
      end
    end

    disambiguate ".ch" do |data|
      if /^\s*#\s*(if|ifdef|ifndef|define|command|xcommand|translate|xtranslate|include|pragma|undef)\b/i.match(data)
        Language["xBase"]
      end
    end

    disambiguate ".cl" do |data|
      if /^\s*\((defun|in-package|defpackage) /i.match(data)
        Language["Common Lisp"]
      elsif /^class/x.match(data)
        Language["Cool"]
      elsif /\/\* |\/\/ |^\}/.match(data)
        Language["OpenCL"]
      end
    end

    disambiguate ".cs" do |data|
      if /![\w\s]+methodsFor: /.match(data)
        Language["Smalltalk"]
      elsif /^\s*namespace\s*[\w\.]+\s*{/.match(data) || /^\s*\/\//.match(data)
        Language["C#"]
      end
    end

    disambiguate ".d" do |data|
      if /^module /.match(data)
        Language["D"]
      elsif /^((dtrace:::)?BEGIN|provider |#pragma (D (option|attributes)|ident)\s)/.match(data)
        Language["DTrace"]
      elsif /(\/.*:( .* \\)$| : \\$|^ : |: \\$)/.match(data)
        Language["Makefile"]
      end
    end

    disambiguate ".ecl" do |data|
      if /^[^#]+:-/.match(data)
        Language["ECLiPSe"]
      elsif data.include?(":=")
        Language["ECL"]
      end
    end
    
    disambiguate ".es" do |data|
      if /^\s*(?:%%|main\s*\(.*?\)\s*->)/.match(data)
        Language["Erlang"]
      elsif /(?:\/\/|("|')use strict\1|export\s+default\s|\/\*.*?\*\/)/m.match(data)
        Language["JavaScript"]
      end
    end

    fortran_rx = /^([c*][^abd-z]|      (subroutine|program|end|data)\s|\s*!)/i

    disambiguate ".f" do |data|
      if /^: /.match(data)
        Language["Forth"]
      elsif data.include?("flowop")
        Language["Filebench WML"]
      elsif fortran_rx.match(data)
        Language["FORTRAN"]
      end
    end

    disambiguate ".for" do |data|
      if /^: /.match(data)
        Language["Forth"]
      elsif fortran_rx.match(data)
        Language["FORTRAN"]
      end
    end

    disambiguate ".fr" do |data|
      if /^(: |also |new-device|previous )/.match(data)
        Language["Forth"]
      elsif /^\s*(import|module|package|data|type) /.match(data)
        Language["Frege"]
      else
        Language["Text"]
      end
    end

    disambiguate ".fs" do |data|
      if /^(: |new-device)/.match(data)
        Language["Forth"]
      elsif /^\s*(#light|import|let|module|namespace|open|type)/.match(data)
        Language["F#"]
      elsif /^\s*(#version|precision|uniform|varying|vec[234])/.match(data)
        Language["GLSL"]
      elsif /#include|#pragma\s+(rs|version)|__attribute__/.match(data)
        Language["Filterscript"]
      end
    end

    disambiguate ".gs" do |data|
      Language["Gosu"] if /^uses java\./.match(data)
    end

    disambiguate ".h" do |data|
      if ObjectiveCRegex.match(data)
        Language["Objective-C"]
      elsif (/^\s*#\s*include <(cstdint|string|vector|map|list|array|bitset|queue|stack|forward_list|unordered_map|unordered_set|(i|o|io)stream)>/.match(data) ||
        /^\s*template\s*</.match(data) || /^[ \t]*try/.match(data) || /^[ \t]*catch\s*\(/.match(data) || /^[ \t]*(class|(using[ \t]+)?namespace)\s+\w+/.match(data) || /^[ \t]*(private|public|protected):$/.match(data) || /std::\w+/.match(data))
        Language["C++"]
      end
    end

    disambiguate ".inc" do |data|
      if /^<\?(?:php)?/.match(data)
        Language["PHP"]
      elsif /^\s*#(declare|local|macro|while)\s/.match(data)
        Language["POV-Ray SDL"]
      end
    end

    disambiguate ".l" do |data|
      if /\(def(un|macro)\s/.match(data)
        Language["Common Lisp"]
      elsif /^(%[%{}]xs|<.*>)/.match(data)
        Language["Lex"]
      elsif /^\.[a-z][a-z](\s|$)/i.match(data)
        Language["Groff"]
      elsif /^\((de|class|rel|code|data|must)\s/.match(data)
        Language["PicoLisp"]
      end
    end

    disambiguate ".ls" do |data|
      if /^\s*package\s*[\w\.\/\*\s]*\s*{/.match(data)
        Language["LoomScript"]
      else
        Language["LiveScript"]
      end
    end

    disambiguate ".lsp", ".lisp" do |data|
      if /^\s*\((defun|in-package|defpackage) /i.match(data)
        Language["Common Lisp"]
      elsif /^\s*\(define /.match(data)
        Language["NewLisp"]
      end
    end

    disambiguate ".m" do |data|
      if ObjectiveCRegex.match(data)
        Language["Objective-C"]
      elsif data.include?(":- module")
        Language["Mercury"]
      elsif /^: /.match(data)
        Language["MUF"]
      elsif /^\s*;/.match(data)
        Language["M"]
      elsif /^\s*\(\*/.match(data)
        Language["Mathematica"]
      elsif /^\s*%/.match(data)
        Language["Matlab"]
      elsif /^\w+\s*:\s*module\s*{/.match(data)
        Language["Limbo"]
      end
    end

    disambiguate ".md" do |data|
      if /^[-a-z0-9=#!\*\[|]/i.match(data)
        Language["Markdown"]
      elsif /^(;;|\(define_)/.match(data)
        Language["GCC machine description"]
      end
    end

    disambiguate ".ml" do |data|
      if /(^\s*module)|let rec |match\s+(\S+\s)+with/.match(data)
        Language["OCaml"]
      elsif /=> |case\s+(\S+\s)+of/.match(data)
        Language["Standard ML"]
      end
    end

    disambiguate ".mod" do |data|
      if data.include?('<!ENTITY ')
        Language["XML"]
      elsif /MODULE\s\w+\s*;/i.match(data) || /^\s*END \w+;$/i.match(data)
        Language["Modula-2"]
      else
        [Language["Linux Kernel Module"], Language["AMPL"]]
      end
    end

    disambiguate ".ms" do |data|
      if /^[.'][a-z][a-z](\s|$)/i.match(data)
        Language["Groff"]
      elsif /(?<!\S)\.(include|globa?l)\s/.match(data) || /(?<!\/\*)(\A|\n)\s*\.[A-Za-z]/.match(data.gsub(/"([^\\"]|\\.)*"|'([^\\']|\\.)*'|\\\s*(?:--.*)?\n/, ""))
        Language["GAS"]
      else
        Language["MAXScript"]
      end
    end

    disambiguate ".n" do |data|
      if /^[.']/.match(data)
        Language["Groff"]
      elsif /^(module|namespace|using)\s/.match(data)
        Language["Nemerle"]
      end
    end

    disambiguate ".ncl" do |data|
      if data.include?("THE_TITLE")
        Language["Text"]
      end
    end

    disambiguate ".nl" do |data|
      if /^(b|g)[0-9]+ /.match(data)
        Language["NL"]
      else
        Language["NewLisp"]
      end
    end

    disambiguate ".php" do |data|
      if data.include?("<?hh")
        Language["Hack"]
      elsif /<?[^h]/.match(data)
        Language["PHP"]
      end
    end

    disambiguate ".pl" do |data|
      if /^[^#]+:-/.match(data)
        Language["Prolog"]
      elsif /use strict|use\s+v?5\./.match(data)
        Language["Perl"]
      elsif /^(use v6|(my )?class|module)/.match(data)
        Language["Perl6"]
      end
    end

    disambiguate ".pm", ".t" do |data|
      if /use strict|use\s+v?5\./.match(data)
        Language["Perl"]
      elsif /^(use v6|(my )?class|module)/.match(data)
        Language["Perl6"]
      end
    end

    disambiguate ".pod" do |data|
      if /^=\w+$/.match(data)
        Language["Pod"]
      else
        Language["Perl"]
      end
    end

    disambiguate ".pro" do |data|
      if /^[^#]+:-/.match(data)
        Language["Prolog"]
      elsif data.include?("last_client=")
        Language["INI"]
      elsif data.include?("HEADERS") && data.include?("SOURCES")
        Language["QMake"]
      elsif /^\s*function[ \w,]+$/.match(data)
        Language["IDL"]
      end
    end

    disambiguate ".props" do |data|
      if /^(\s*)(<Project|<Import|<Property|<?xml|xmlns)/i.match(data)
        Language["XML"]
      elsif /\w+\s*=\s*/i.match(data)
        Language["INI"]
      end
    end

    disambiguate ".r" do |data|
      if /\bRebol\b/i.match(data)
        Language["Rebol"]
      elsif /<-|^\s*#/.match(data)
        Language["R"]
      end
    end

    disambiguate ".rno" do |data|
      if /^\.!|^\.end lit(?:eral)?\b/i.match(data)
        Language["RUNOFF"]
      elsif /^\.\\" /.match(data)
        Language["Groff"]
      end
    end

    disambiguate ".rpy" do |data|
      if /(^(import|from|class|def)\s)/m.match(data)
        Language["Python"]
      else
        Language["Ren'Py"]
      end
    end

    disambiguate ".rs" do |data|
      if /^(use |fn |mod |pub |macro_rules|impl|#!?\[)/.match(data)
        Language["Rust"]
      elsif /#include|#pragma\s+(rs|version)|__attribute__/.match(data)
        Language["RenderScript"]
      end
    end

    disambiguate ".sc" do |data|
      if /\^(this|super)\./.match(data) || /^\s*(\+|\*)\s*\w+\s*{/.match(data) || /^\s*~\w+\s*=\./.match(data)
        Language["SuperCollider"]
      elsif /^\s*import (scala|java)\./.match(data) || /^\s*val\s+\w+\s*=/.match(data) || /^\s*class\b/.match(data)
        Language["Scala"]
      end
    end

    disambiguate ".sql" do |data|
      if /^\\i\b|AS \$\$|LANGUAGE '?plpgsql'?/i.match(data) || /SECURITY (DEFINER|INVOKER)/i.match(data) || /BEGIN( WORK| TRANSACTION)?;/i.match(data)
        #Postgres
        Language["PLpgSQL"]
      elsif /(alter module)|(language sql)|(begin( NOT)+ atomic)/i.match(data)  || /signal SQLSTATE '[0-9]+'/i.match(data)
        #IBM db2
        Language["SQLPL"]
      elsif /\$\$PLSQL_|XMLTYPE|sysdate|systimestamp|\.nextval|connect by|AUTHID (DEFINER|CURRENT_USER)/i.match(data) || /constructor\W+function/i.match(data)
        #Oracle
        Language["PLSQL"]
      elsif ! /begin|boolean|package|exception/i.match(data)
        #Generic SQL
        Language["SQL"]
      end
    end
    
    disambiguate ".srt" do |data|
      if /^(\d{2}:\d{2}:\d{2},\d{3})\s*(-->)\s*(\d{2}:\d{2}:\d{2},\d{3})$/.match(data)
        Language["SubRip Text"]
      end
    end
    
    disambiguate ".t" do |data|
      if /^\s*%|^\s*var\s+\w+\s*:\s*\w+/.match(data)
        Language["Turing"]
      elsif /^\s*use\s+v6\s*;/.match(data)
        Language["Perl6"]
      end
    end
    
    disambiguate ".toc" do |data|
      if /^## |@no-lib-strip@/.match(data)
        Language["World of Warcraft Addon Data"]
      elsif /^\\(contentsline|defcounter|beamer|boolfalse)/.match(data)
        Language["TeX"]
      end
    end

    disambiguate ".ts" do |data|
      if data.include?("<TS")
        Language["XML"]
      else
        Language["TypeScript"]
      end
    end

    disambiguate ".tst" do |data|
      if (data.include?("gap> "))
        Language["GAP"]
      # Heads up - we don't usually write heuristics like this (with no regex match)
      else
        Language["Scilab"]
      end
    end
  end
end
