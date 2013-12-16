module Linguist
  # A collection of simple heuristics that can be used to better analysis languages.
  class Heuristics
    # Public: Given an array of String language names,
    # apply heuristics against the given data and return an array
    # of matching languages, or nil.
    #
    # data      - Array of tokens or String data to analyze.
    # languages - Array of language name Strings to restrict to.
    #
    # Returns an array of Languages or []
    def self.find_by_heuristics(data, languages)
      if languages.all? { |l| ["Objective-C", "C++"].include?(l) }
        disambiguate_h(data, languages)
      end
    end

    # .h extensions are ambigious between C, C++, and Objective-C.
    # We want to shortcut look for Objective-C.
    #
    # Returns an array of Languages or []
    def self.disambiguate_h(data, languages)
      matches = []
      matches << Language["Objective-C"] if data.include?("@interface")
      matches
    end
  end
end
