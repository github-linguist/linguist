require 'linguist/tokenizer'

module Linguist
  # Language bayesian classifier.
  class Classifier
    # Internal: Path to persisted classifier db.
    PATH = File.expand_path('../classifier.yml', __FILE__)

    # Public: Check if persisted db exists on disk.
    #
    # Returns Boolean.
    def self.exist?
      File.exist?(PATH)
    end

    # Public: Get persisted Classifier instance.
    #
    # Returns Classifier.
    def self.instance
      @instance ||= YAML.load_file(PATH)
    end

    # Public: Initialize a Classifier.
    def initialize
      @tokens_total    = 0
      @languages_total = 0
      @tokens          = Hash.new { |h, k| h[k] = Hash.new(0) }
      @language_tokens = Hash.new(0)
      @languages       = Hash.new(0)
    end

    # Public: Compare Classifier objects.
    #
    # other - Classifier object to compare to.
    #
    # Returns Boolean.
    def eql?(other)
      # Lazy fast check counts only
      other.is_a?(self.class) &&
        @tokens_total == other.instance_variable_get(:@tokens_total) &&
        @languages_total == other.instance_variable_get(:@languages_total)
    end
    alias_method :==, :eql?

    # Public: Train classifier that data is a certain language.
    #
    # language - Language of data
    # data     - String contents of file
    #
    # Examples
    #
    #   train(Language['Ruby'], "def hello; end")
    #
    # Returns nothing.
    def train(language, data)
      language = language.name
      tokens   = Tokenizer.new(data).tokens

      tokens.each do |token|
        @tokens[language][token] += 1
        @language_tokens[language] += 1
        @tokens_total += 1
      end
      @languages[language] += 1
      @languages_total += 1

      nil
    end

    # Public: Prune infrequent tokens.
    #
    # Returns receiver Classifier instance.
    def gc
      @tokens.each do |language, tokens|
        if @language_tokens[language] > 20
          tokens.each do |name, count|
            if count == 1
              @tokens[language].delete(name)
              @language_tokens[language] -= 1
              @tokens_total -= 1
            end
          end
        end
      end
      self
    end

    # Public: Guess language of data.
    #
    # data      - Array of tokens or String data to analyze.
    # languages - Array of Languages to restrict to.
    #
    # Examples
    #
    #   classify("def hello; end")
    #   # => [ [Language['Ruby'], 0.90], [Language['Python'], 0.2], ... ]
    #
    # Returns sorted Array of result pairs. Each pair contains the
    # Language and a Float score.
    def classify(tokens, languages = @languages.keys)
      tokens = Tokenizer.new(tokens).tokens if tokens.is_a?(String)

      scores = {}
      languages.each do |language|
        language_name = language.is_a?(Language) ? language.name : language
        scores[language_name] = tokens_probability(tokens, language_name) *
                                  language_probability(language_name)
      end

      scores.sort { |a, b| b[1] <=> a[1] }.map { |score| [Language[score[0]], score[1]] }
    end

    # Internal: Probably of set of tokens in a language occuring - P(D | C)
    #
    # tokens   - Array of String tokens.
    # language - Language to check.
    #
    # Returns Float between 0.0 and 1.0.
    def tokens_probability(tokens, language)
      tokens.inject(1.0) do |sum, token|
        sum *= token_probability(token, language)
      end
    end

    # Internal: Probably of token in language occuring - P(F | C)
    #
    # token    - String token.
    # language - Language to check.
    #
    # Returns Float between 0.0 and 1.0.
    def token_probability(token, language)
      if @tokens[language][token] == 0
        1 / @tokens_total.to_f
      else
        @tokens[language][token].to_f / @languages[language].to_f
      end
    end

    # Internal: Probably of a language occuring - P(C)
    #
    # language - Language to check.
    #
    # Returns Float between 0.0 and 1.0.
    def language_probability(language)
      @languages[language].to_f / @languages_total.to_f
    end
  end

  # Eager load instance
  Classifier.instance if Classifier.exist?
end
