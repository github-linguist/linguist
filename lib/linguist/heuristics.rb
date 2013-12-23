module Linguist
  # A collection of simple heuristics that can be used to better analyze languages.
  class Heuristics
    ACTIVE = false

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
        if languages.all? { |l| ["Objective-C", "C++"].include?(l) }
          disambiguate_h(data, languages)
        end
      end
    end

    # .h extensions are ambigious between C, C++, and Objective-C.
    # We want to shortcut look for Objective-C _and_ now C++ too!
    #
    # Returns an array of Languages or []
    # TODO rename this method as we're not strictly disambiguating between .h files here.
    def self.disambiguate_h(data, languages)
      matches = []
      matches << Language["Objective-C"] if data.include?("@interface")
      matches << Language["C++"] if data.include?("#include <cstdint>")
      matches
    end

    def self.active?
      !!ACTIVE
    end
  end
end
