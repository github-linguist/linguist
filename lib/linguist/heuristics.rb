require 'yaml'

module Linguist
  # A collection of simple heuristics that can be used to better analyze languages.
  class Heuristics
    HEURISTICS_CONSIDER_BYTES = 50 * 1024

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
      return [] if blob.symlink?
      self.load()

      data = blob.data[0...HEURISTICS_CONSIDER_BYTES]

      @heuristics.each do |heuristic|
        if heuristic.matches?(blob.name, candidates)
          return Array(heuristic.call(data))
        end
      end

      [] # No heuristics matched
    rescue Regexp::TimeoutError
      [] # Return nothing if we have a bad regexp which leads to a timeout enforced by Regexp.timeout in Ruby 3.2 or later
    end

    # Public: Get all heuristic definitions
    #
    # Returns an Array of heuristic objects.
    def self.all
      self.load()
      @heuristics
    end

    # Internal: Load heuristics from 'heuristics.yml'.
    def self.load()
      if @heuristics.any?
        return
      end

      data = self.load_config
      named_patterns = data['named_patterns'].map { |k,v| [k, self.to_regex(v)] }.to_h

      data['disambiguations'].each do |disambiguation|
        exts = disambiguation['extensions']
        rules = disambiguation['rules']
        rules.map! do |rule|
          rule['pattern'] = self.parse_rule(named_patterns, rule)
          rule
        end
        @heuristics << new(exts, rules)
      end
    end

    def self.load_config
      YAML.load_file(File.expand_path("../heuristics.yml", __FILE__))
    end

    def self.parse_rule(named_patterns, rule)
      if !rule['and'].nil?
        rules = rule['and'].map { |block| self.parse_rule(named_patterns, block) }
        return And.new(rules)
      elsif !rule['pattern'].nil?
        return self.to_regex(rule['pattern'])
      elsif !rule['negative_pattern'].nil?
        pat = self.to_regex(rule['negative_pattern'])
        return NegativePattern.new(pat)
      elsif !rule['named_pattern'].nil?
        return named_patterns[rule['named_pattern']]
      else
        return AlwaysMatch.new()
      end
    end

    # Internal: Converts a string or array of strings to regexp
    #
    # str: string or array of strings. If it is an array of strings,
    #      Regexp.union will be used.
    def self.to_regex(str)
      if str.kind_of?(Array)
        Regexp.union(str.map { |s| Regexp.new(s) })
      else
        Regexp.new(str)
      end
    end

    # Internal: Array of defined heuristics
    @heuristics = []

    # Internal
    def initialize(exts, rules)
      @exts = exts
      @rules = rules
    end

    # Internal: Return the heuristic's target extensions
    def extensions
      @exts
    end

    # Internal: Return the heuristic's candidate languages
    def languages
      @rules.map do |rule|
        [rule['language']].flatten(2).map { |name| Language[name] }
      end.flatten.uniq
    end

    # Internal: Check if this heuristic matches the candidate filenames or
    # languages.
    def matches?(filename, candidates)
      filename = filename.downcase
      candidates = candidates.compact.map(&:name)
      @exts.any? { |ext| filename.end_with?(ext) }
    end

    # Internal: Perform the heuristic
    def call(data)
      matched = @rules.find do |rule|
        rule['pattern'].match?(data)
      end
      if !matched.nil?
        languages = matched['language']
        if languages.is_a?(Array)
          languages.map{ |l| Language[l] }
        else
          Language[languages]
        end
      end
    end
  end

  class And

    def initialize(pats)
      @pats = pats
    end

    def match?(input)
      return @pats.all? { |pat| pat.match?(input) }
    end

  end

  class AlwaysMatch
    def match?(input)
      return true
    end
  end

  class NegativePattern

    def initialize(pat)
      @pat = pat
    end

    def match?(input)
      return !@pat.match?(input)
    end

  end
end
