require 'linguist/language'
require 'pry'
module Linguist
  class Overrides

    def self.ignored?(name)
      new(name).ignored_path?
    end

    def self.language_for?(name)
      new(name).language
    end

    def initialize(file_with_path)
      @name = file_with_path

      @override_definitions = File.new('.linguist')

      if File.exists?(@override_definitions)
        ignored_paths = []
        language_paths = Hash.new()

        File.open('.linguist', 'r').each do |line|
          regex, command = line.split(' ').map(&:strip)

          if command == 'ignore'
            ignored_paths << regex
          elsif Language[command] # is this a known language to Linguist?
            language_paths[command] = [] unless language_paths.has_key?(command)
            language_paths[command] << regex
          else
            # warn about this not be a recognised statement
          end
        end

        @ignore_regex = Regexp.new(ignored_paths.join('|'))
        @language_regexes = language_paths
      end
    end

    attr_reader :name
    attr_reader :ignore_regex
    attr_reader :language_regexes

    # Internal: Is this file ignored?
    #
    # Does this file match one of the ignore regular expressions?
    #
    # Returns true or false
    def ignored_path?
      name.match(self.ignore_regex) ? true : false
    end

    # Internal: Is there a language override for this file?
    #
    # Returns a Language
    def language
      override_language
    end

    def override_language
      language_candidates = []

      self.language_regexes.each do |lang, regexes|
        if name.match(Regexp.new(regexes.join('|')))
          language_candidates << Language[lang]
        end
      end

      if language_candidates.any?
        return language_candidates.first
      else
        return nil
      end
    end
  end
end
