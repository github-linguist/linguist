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
      matches = []
      if active?
        if languages.all? { |l| ["Objective-C", "C++", "C"].include?(l) }
          matches = disambiguate_c(data, languages)
        end
        if languages.all? { |l| ["Perl", "Prolog"].include?(l) }
          matches = disambiguate_pl(data, languages)
        end
        if languages.all? { |l| ["ECL", "Prolog"].include?(l) }
          matches = disambiguate_ecl(data, languages)
        end
        if languages.all? { |l| ["TypeScript", "XML"].include?(l) }
          matches = disambiguate_ts(data, languages)
        end
        if languages.all? { |l| ["Common Lisp", "OpenCL"].include?(l) }
          matches = disambiguate_cl(data, languages)
        end
      end
      matches
    end

    # .h extensions are ambigious between C, C++, and Objective-C.
    # We want to shortcut look for Objective-C _and_ now C++ too!
    #
    # Returns an array of Languages or []
    def self.disambiguate_c(data, languages)
      matches = []
      if (/@(interface|class|protocol|synchronised|selector|implementation)/.match(data))
        matches << Language["Objective-C"]
      end
      if (/^\s*#\s*include <(cstdint|string|vector|map|list|array|bitset|queue|stack|forward_list|unordered_map|unordered_set)>/.match(data) or
          /^\s*template\s*</.match(data) or /^[^@]class\s+\w+/.match(data) or /^[^@](private|public|protected):$/.match(data))
        matches << Language["C++"]
      end
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

    def self.active?
      !!ACTIVE
    end
  end
end
