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

    # Internal: Define a new heuristic.
    #
    # languages - String names of languages to disambiguate.
    # heuristic - Block which takes data as an argument and returns a Language or nil.
    #
    # Examples
    #
    #     disambiguate "Perl", "Prolog" do |data|
    #       if data.include?("use strict")
    #         Language["Perl"]
    #       elsif data.include?(":-")
    #         Language["Prolog"]
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
    def matches?(candidates)
      candidates.all? { |l| @languages.include?(l.name) }
    end

    # Internal: Perform the heuristic
    def call(data)
      @heuristic.call(data)
    end

    disambiguate "Perl", "Prolog" do |data|
      if data.include?("use strict")
        Language["Perl"]
      elsif data.include?(":-")
        Language["Prolog"]
      end
    end

    disambiguate "ECL", "Prolog" do |data|
      if data.include?(":-")
        Language["Prolog"]
      elsif data.include?(":=")
        Language["ECL"]
      end
    end

    disambiguate "IDL", "Prolog" do |data|
      if data.include?(":-")
        Language["Prolog"]
      else
        Language["IDL"]
      end
    end

    disambiguate "Common Lisp", "OpenCL" do |data|
      if data.include?("(defun ")
        Language["Common Lisp"]
      elsif /\/\* |\/\/ |^\}/.match(data)
        Language["OpenCL"]
      end
    end

    disambiguate "Hack", "PHP" do |data|
      if data.include?("<?hh")
        Language["Hack"]
      elsif /<?[^h]/.match(data)
        Language["PHP"]
      end
    end

    disambiguate "Scala", "SuperCollider" do |data|
      if /\^(this|super)\./.match(data) || /^\s*(\+|\*)\s*\w+\s*{/.match(data) || /^\s*~\w+\s*=\./.match(data)
        Language["SuperCollider"]
      elsif /^\s*import (scala|java)\./.match(data) || /^\s*val\s+\w+\s*=/.match(data) || /^\s*class\b/.match(data)
        Language["Scala"]
      end
    end

    disambiguate "AsciiDoc", "AGS Script" do |data|
      Language["AsciiDoc"] if /^=+(\s|\n)/.match(data)
    end

    disambiguate "FORTRAN", "Forth" do |data|
      if /^: /.match(data)
        Language["Forth"]
      elsif /^([c*][^a-z]|      subroutine\s)/i.match(data)
        Language["FORTRAN"]
      end
    end

    disambiguate "F#", "Forth", "GLSL" do |data|
      if /^(: |new-device)/.match(data)
        Language["Forth"]
      elsif /^(#light|import|let|module|namespace|open|type)/.match(data)
        Language["F#"]
      elsif /^(#include|#pragma|precision|uniform|varying|void)/.match(data)
        Language["GLSL"]
      end
    end
  end
end
