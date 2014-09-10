module Linguist
  # A collection of simple heuristics that can be used to better analyze languages.
  class Heuristics
    ACTIVE = true

    # Public: Given an array of String language names,
    # apply heuristics against the given data and return an array
    # of matching languages, or nil.
    #
    # data      - Array of tokens or String data to analyze.
    # languages - Array of language name Strings to restrict to.
    #
    # Returns an array of Languages or []
    def self.find_by_heuristics(data, languages)
      if active?
        if languages.all? { |l| ["Perl", "Prolog"].include?(l) }
          result = disambiguate_pl(data, languages)
        end
        if languages.all? { |l| ["ECL", "Prolog"].include?(l) }
          result = disambiguate_ecl(data, languages)
        end
        return result
      end
    end

    # .h extensions are ambigious between C, C++, and Objective-C.
    # We want to shortcut look for Objective-C _and_ now C++ too!
    #
    # Returns an array of Languages or []
    def self.disambiguate_c(data, languages)
      matches = []
      matches << Language["Objective-C"] if data.include?("@interface")
      matches << Language["C++"] if data.include?("#include <cstdint>")
      matches
    end

    def self.disambiguate_pl(data, languages)
      matches = []
      matches << Language["Prolog"] if data.include?(":-")
      matches << Language["Perl"] if data.include?("use strict")
      matches
    end

    def self.disambiguate_ecl(data, languages)
      matches = []
      matches << Language["Prolog"] if data.include?(":-")
      matches << Language["ECL"] if data.include?(":=")
      matches
    end

    def self.disambiguate_ts(data, languages)
      matches = []
      if (data.include?("</translation>"))
        matches << Language["XML"]
      else
        matches << Language["TypeScript"]
      end
      matches
    end

    def self.disambiguate_cl(data, languages)
      matches = []
      matches << Language["Common Lisp"] if data.include?("(defun ")
      matches << Language["OpenCL"] if /\/\* |\/\/ |^\}/.match(data)
      matches
    end

    def self.disambiguate_r(data, languages)
      matches = []
      matches << Language["Rebol"] if /\bRebol\b/i.match(data)
      matches << Language["R"] if data.include?("<-")
      matches
    end

    def self.active?
      !!ACTIVE
    end
  end
end
