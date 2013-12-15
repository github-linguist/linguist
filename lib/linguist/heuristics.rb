require 'linguist/tokenizer'

module Linguist
  # A collection of simple heuristics that can be used to better analysis languages.
  class Heuristics
    # Public: Given an array of String language names, a
    # apply all heuristics against the given data and return an array
    # of matching languages, or nil.
    # data      - Array of tokens or String data to analyze.
    # languages - Array of language name Strings to restrict to.

    # Returns an array of language name Strings, or []
    def self.find_by_heuristics(data, languages)
      if languages.all? { |l| ["pod", "perl"].include?(l) }
        disambiguate_pod(data, languages)
      elsif languages.all? { |l| ["objective-c", "c++"].include?(l) }
        disambiguate_h(data, languages)
      end
    end

    # Internal: Initialize a Heuristics class
    def initialize
    end

    # .pod extensions are ambigious between perl and pod.
    #
    # Returns an array of still-possible languages, or nil
    def self.disambiguate_pod(data, languages)
      matches = []
      matches << Language["Perl"] if data.includes?("my $")
      matches
    end

    # .h extensions are ambigious between C, C++, and Objective-C.
    # We want to look for Objective-C.
    def self.disambiguate_h(data, languages)
      matches = []
      matches << Language["Objective-C"] if data.includes?("NSData *") && data.includes?("@interface")
      matches
    end
  end
end

