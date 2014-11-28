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
    # Returns an Array with one Language if a heuristic matched, or empty if
    # none matched or were inconclusive.
    def self.call(blob, languages)
      data = blob.data

      @heuristics.each do |heuristic|
        if heuristic.matches?(languages)
          language = heuristic.call(data)
          return [language] if language
        end
      end

      [] # No heuristics matched
    end

    @heuristics = []

    def self.create(*languages, &heuristic)
      @heuristics << new(languages, &heuristic)
    end

    def initialize(languages, &heuristic)
      @languages = languages
      @heuristic = heuristic
    end

    def matches?(candidates)
      candidates.all? { |l| @languages.include?(l.name) }
    end

    def call(data)
      @heuristic.call(data)
    end

    create "Perl", "Prolog" do |data|
      if data.include?("use strict")
        Language["Perl"]
      elsif data.include?(":-")
        Language["Prolog"]
      end
    end

    create "ECL", "Prolog" do |data|
      if data.include?(":-")
        Language["Prolog"]
      elsif data.include?(":=")
        Language["ECL"]
      end
    end

    create "IDL", "Prolog" do |data|
      if data.include?(":-")
        Language["Prolog"]
      else
        Language["IDL"]
      end
    end

    create "Common Lisp", "OpenCL" do |data|
      if data.include?("(defun ")
        Language["Common Lisp"]
      elsif /\/\* |\/\/ |^\}/.match(data)
        Language["OpenCL"]
      end
    end

    create "Hack", "PHP" do |data|
      if data.include?("<?hh")
        Language["Hack"]
      elsif /<?[^h]/.match(data)
        Language["PHP"]
      end
    end

    create "Scala", "SuperCollider" do |data|
      if /\^(this|super)\./.match(data) || /^\s*(\+|\*)\s*\w+\s*{/.match(data) || /^\s*~\w+\s*=\./.match(data)
        Language["SuperCollider"]
      elsif /^\s*import (scala|java)\./.match(data) || /^\s*val\s+\w+\s*=/.match(data) || /^\s*class\b/.match(data)
        Language["Scala"]
      end
    end

    create "AsciiDoc", "AGS Script" do |data|
      Language["AsciiDoc"] if /^=+(\s|\n)/.match(data)
    end

    create "FORTRAN", "Forth" do |data|
      if /^: /.match(data)
        Language["Forth"]
      elsif /^([c*][^a-z]|      subroutine\s)/i.match(data)
        Language["FORTRAN"]
      end
    end

    create "F#", "Forth", "GLSL" do |data|
      if /^(: |new-device)/.match(data)
        Language["Forth"]
      elsif /^(#light|import|let|module|namespace|open|type)/.match(data)
        Language["F#"]
      elsif /^(#include|#pragma|precision|uniform|varying|void)/.match(data)
        Language["GLSL"]
      end
    end

    # .h extensions are ambiguous between C, C++, and Objective-C.
    # We want to shortcut look for Objective-C _and_ now C++ too!
    #
    # Returns an array of Languages or []
    def self.disambiguate_c(data)
      matches = []
      if data.include?("@interface")
        matches << Language["Objective-C"]
      elsif data.include?("#include <cstdint>")
        matches << Language["C++"]
      end
      matches
    end

    def self.disambiguate_ts(data)
      matches = []
      if (data.include?("</translation>"))
        matches << Language["XML"]
      else
        matches << Language["TypeScript"]
      end
      matches
    end

    def self.disambiguate_r(data)
      matches = []
      matches << Language["Rebol"] if /\bRebol\b/i.match(data)
      matches << Language["R"] if data.include?("<-")
      matches
    end

  end
end
