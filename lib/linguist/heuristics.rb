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
    def self.call(blob, languages)
      data = blob.data
      language_names = languages.map(&:name)
      result = nil

      @heuristics.each do |heuristic|
        result = heuristic.call(data) if heuristic.matches?(language_names)
        break if result
      end

      Array(result).map { |l| Language[l] }
    end

    # Internal: Define a new heuristic.
    #
    # languages - String names of languages to disambiguate.
    # heuristic - Block which takes data as an argument and returns a Language or nil.
    #
    # Examples
    #
    #     disambiguate "Perl", "Prolog" do |data|
    #       if data.include?("use strict")
    #         "Perl"
    #       elsif /^[^#]+:-/.match(data)
    #         "Prolog"
    #       end
    #     end
    #
    def self.disambiguate(*languages, &heuristic)
      @heuristics << new(languages, &heuristic)
    end

    # Internal: Array of defined heuristics
    @heuristics = []

    # Internal
    def initialize(languages, &heuristic)
      @languages = languages
      @heuristic = heuristic
    end

    # Internal: Check if this heuristic matches the candidate languages.
    #
    # A heuristic will match if it is a subset of the candidate languages.
    def matches?(candidates)
      @languages.all? { |l| candidates.include?(l) }
    end

    # Internal: Perform the heuristic
    def call(data)
      @heuristic.call(data)
    end

    disambiguate "BitBake", "BlitzBasic" do |data|
      if /^\s*; /.match(data) || data.include?("End Function")
        "BlitzBasic"
      elsif /^\s*(# |include|require)\b/.match(data)
        "BitBake"
      end
    end

    disambiguate "C#", "Smalltalk" do |data|
      if /![\w\s]+methodsFor: /.match(data)
        "Smalltalk"
      elsif /^\s*namespace\s*[\w\.]+\s*{/.match(data) || /^\s*\/\//.match(data)
        "C#"
      end
    end

    disambiguate "Objective-C" do |data|
      if /^[ \t]*@(interface|class|protocol|property|end|synchronised|selector|implementation)\b/.match(data)
        "Objective-C"
      end
    end

    disambiguate "C++", "C" do |data|
      if (/^\s*#\s*include <(cstdint|string|vector|map|list|array|bitset|queue|stack|forward_list|unordered_map|unordered_set|(i|o|io)stream)>/.match(data) ||
        /^\s*template\s*</.match(data) || /^[ \t]*try/.match(data) || /^[ \t]*catch\s*\(/.match(data) || /^[ \t]*(class|(using[ \t]+)?namespace)\s+\w+/.match(data) || /^[ \t]*(private|public|protected):$/.match(data) || /std::\w+/.match(data))
        "C++"
      end
    end

    disambiguate "Perl", "Perl6", "Prolog" do |data|
      if data.include?("use v6")
        "Perl6"
      elsif data.match(/use strict|use\s+v?5\./)
        "Perl"
      elsif /^[^#]+:-/.match(data)
        "Prolog"
      end
    end

    disambiguate "ECL", "Prolog" do |data|
      if /^[^#]+:-/.match(data)
        "Prolog"
      elsif data.include?(":=")
        "ECL"
      end
    end

    disambiguate "IDL", "Prolog", "INI", "QMake" do |data|
      if /^[^#]+:-/.match(data)
        "Prolog"
      elsif data.include?("last_client=")
        "INI"
      elsif data.include?("HEADERS") && data.include?("SOURCES")
        "QMake"
      elsif /^\s*function[ \w,]+$/.match(data)
        "IDL"
      end
    end

    disambiguate "GAP", "Scilab" do |data|
      if (data.include?("gap> "))
        "GAP"
      # Heads up - we don't usually write heuristics like this (with no regex match)
      else
        "Scilab"
      end
    end

    disambiguate "Common Lisp", "OpenCL", "Cool" do |data|
      if /^\s*\((defun|in-package|defpackage) /i.match(data)
        "Common Lisp"
      elsif /^class/x.match(data)
        "Cool"
      elsif /\/\* |\/\/ |^\}/.match(data)
        "OpenCL"
      end
    end

    disambiguate "Hack", "PHP" do |data|
      if data.include?("<?hh")
        "Hack"
      elsif /<?[^h]/.match(data)
        "PHP"
      end
    end

    disambiguate "Scala", "SuperCollider" do |data|
      if /\^(this|super)\./.match(data) || /^\s*(\+|\*)\s*\w+\s*{/.match(data) || /^\s*~\w+\s*=\./.match(data)
        "SuperCollider"
      elsif /^\s*import (scala|java)\./.match(data) || /^\s*val\s+\w+\s*=/.match(data) || /^\s*class\b/.match(data)
        "Scala"
      end
    end

    disambiguate "AsciiDoc", "AGS Script", "Public Key" do |data|
      if /^(----[- ]BEGIN|ssh-(rsa|dss)) /.match(data)
        "Public Key"
      elsif /^[=-]+(\s|\n)|{{[A-Za-z]/.match(data)
        "AsciiDoc"
      elsif /^(\/\/.+|((import|export)\s+)?(function|int|float|char)\s+((room|repeatedly|on|game)_)?([A-Za-z]+[A-Za-z_0-9]+)\s*[;\(])/.match(data)
        "AGS Script"
      end
    end

    disambiguate "FORTRAN", "Forth" do |data|
      if /^: /.match(data)
        "Forth"
      elsif /^([c*][^a-z]|      (subroutine|program)\s|\s*!)/i.match(data)
        "FORTRAN"
      end
    end

    disambiguate "F#", "Forth", "GLSL", "Filterscript" do |data|
      if /^(: |new-device)/.match(data)
        "Forth"
      elsif /^\s*(#light|import|let|module|namespace|open|type)/.match(data)
        "F#"
      elsif /^\s*(#version|precision|uniform|varying|vec[234])/.match(data)
        "GLSL"
      elsif /#include|#pragma\s+(rs|version)|__attribute__/.match(data)
        "Filterscript"
      end
    end

    disambiguate "Limbo", "M", "MUF", "Mathematica", "Matlab", "Mercury" do |data|
      if data.include?(":- module")
        "Mercury"
      elsif /^: /.match(data)
        "MUF"
      elsif /^\s*;/.match(data)
        "M"
      elsif /^\s*\(\*/.match(data)
        "Mathematica"
      elsif /^\s*%/.match(data)
        "Matlab"
      elsif /^\w+\s*:\s*module\s*{/.match(data)
        "Limbo"
      end
    end

    disambiguate "Gosu", "JavaScript" do |data|
      "Gosu" if /^uses java\./.match(data)
    end

    disambiguate "LoomScript", "LiveScript" do |data|
      if /^\s*package\s*[\w\.\/\*\s]*\s*{/.match(data)
        "LoomScript"
      else
        "LiveScript"
      end
    end

    disambiguate "Common Lisp", "NewLisp" do |data|
      if /^\s*\((defun|in-package|defpackage) /i.match(data)
        "Common Lisp"
      elsif /^\s*\(define /.match(data)
        "NewLisp"
      end
    end

    disambiguate "TypeScript", "XML" do |data|
      if data.include?("<TS ")
        "XML"
      else
        "TypeScript"
      end
    end

    disambiguate "Frege", "Forth", "Text" do |data|
      if /^(: |also |new-device|previous )/.match(data)
        "Forth"
      elsif /^\s*(import|module|package|data|type) /.match(data)
        "Frege"
      else
        "Text"
      end
    end

    disambiguate "PLSQL", "SQLPL", "PLpgSQL", "SQL" do |data|
      if /^\\i\b|AS \$\$|LANGUAGE '+plpgsql'+/i.match(data) || /SECURITY (DEFINER|INVOKER)/i.match(data) || /BEGIN( WORK| TRANSACTION)?;/i.match(data)
        #Postgres
        "PLpgSQL"
      elsif /(alter module)|(language sql)|(begin( NOT)+ atomic)/i.match(data)  || /signal SQLSTATE '[0-9]+'/i.match(data)
        #IBM db2
        "SQLPL"
      elsif /pragma|\$\$PLSQL_|XMLTYPE|sysdate|systimestamp|\.nextval|connect by|AUTHID (DEFINER|CURRENT_USER)/i.match(data) || /constructor\W+function/i.match(data)
        #Oracle
        "PLSQL"
      elsif ! /begin|boolean|package|exception/i.match(data)
        #Generic SQL
        "SQL"
      end
    end

    disambiguate "D", "DTrace", "Makefile" do |data|
      if /^module /.match(data)
        "D"
      elsif /^((dtrace:::)?BEGIN|provider |#pragma (D (option|attributes)|ident)\s)/.match(data)
        "DTrace"
      elsif /(\/.*:( .* \\)$| : \\$|^ : |: \\$)/.match(data)
        "Makefile"
      end
    end

    disambiguate "OCaml", "Standard ML" do |data|
      if /(^\s*module)|let rec |match\s+(\S+\s)+with/.match(data)
        "OCaml"
      elsif /=> |case\s+(\S+\s)+of/.match(data)
        "Standard ML"
      end
    end

    disambiguate "NL", "NewLisp" do |data|
      if /^(b|g)[0-9]+ /.match(data)
        "NL"
      else
        "NewLisp"
      end
    end

    disambiguate "Rust", "RenderScript" do |data|
      if data.include?("^(use |fn |mod |pub |macro_rules|impl|#!?\[)")
        "Rust"
      elsif /#include|#pragma\s+(rs|version)|__attribute__/.match(data)
        "RenderScript"
      end
    end

    disambiguate "Common Lisp", "Lex", "Groff" do |data|
      if data.include?("(def(un|macro)\s")
        Language["Common Lisp"]
      elsif /^(%[%{}]xs|<.*>)/.match(data)
        Language["Lex"]
      elsif /^\.[a-z][a-z](\s|$)/.match(data)
        Language["Groff"]
      end
    end
  end
end
