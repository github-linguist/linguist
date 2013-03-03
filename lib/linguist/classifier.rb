require 'linguist/tokenizer'

module Linguist
  # Language bayesian classifier.
  class Classifier
    # Public: Train classifier that data is a certain language.
    #
    # db       - Hash classifier database object
    # language - String language of data
    # data     - String contents of file
    #
    # Examples
    #
    #   Classifier.train(db, 'Ruby', "def hello; end")
    #
    # Returns nothing.
    def self.train!(db, language, data)
      tokens = Tokenizer.tokenize(data)

      db['tokens_total'] ||= 0
      db['languages_total'] ||= 0
      db['tokens'] ||= {}
      db['language_tokens'] ||= {}
      db['languages'] ||= {}

      tokens.each do |token|
        db['tokens'][language] ||= {}
        db['tokens'][language][token] ||= 0
        db['tokens'][language][token] += 1
        db['language_tokens'][language] ||= 0
        db['language_tokens'][language] += 1
        db['tokens_total'] += 1
      end
      db['languages'][language] ||= 0
      db['languages'][language] += 1
      db['languages_total'] += 1

      nil
    end

    # Public: Guess language of data.
    #
    # db        - Hash of classifier tokens database.
    # data      - Array of tokens or String data to analyze.
    # languages - Array of language name Strings to restrict to.
    #
    # Examples
    #
    #   Classifier.classify(db, "def hello; end")
    #   # => [ 'Ruby', 0.90], ['Python', 0.2], ... ]
    #
    # Returns sorted Array of result pairs. Each pair contains the
    # String language name and a Float score.
    def self.classify(db, tokens, languages = nil)
      languages ||= db['languages'].keys
      new(db).classify(tokens, languages)
    end

    # Internal: Initialize a Classifier.
    def initialize(db = {})
      @tokens_total    = db['tokens_total']
      @languages_total = db['languages_total']
      @tokens          = db['tokens']
      @language_tokens = db['language_tokens']
      @languages       = db['languages']
    end

    # Internal: Guess language of data
    #
    # data      - Array of tokens or String data to analyze.
    # languages - Array of language name Strings to restrict to.
    #
    # Returns sorted Array of result pairs. Each pair contains the
    # String language name and a Float score.
    def classify(tokens, languages)
      return [] if tokens.nil?
      tokens = Tokenizer.tokenize(tokens) if tokens.is_a?(String)

      scores = {}
      languages.each do |language|
        scores[language] = tokens_probability(tokens, language) +
                                   language_probability(language)
      end

      scores.sort { |a, b| b[1] <=> a[1] }.map { |score| [score[0], score[1]] }
    end

    # Internal: Probably of set of tokens in a language occurring - P(D | C)
    #
    # tokens   - Array of String tokens.
    # language - Language to check.
    #
    # Returns Float between 0.0 and 1.0.
    def tokens_probability(tokens, language)
      tokens.inject(0.0) do |sum, token|
        sum += Math.log(token_probability(token, language))
      end
    end

    # Internal: Probably of token in language occurring - P(F | C)
    #
    # token    - String token.
    # language - Language to check.
    #
    # Returns Float between 0.0 and 1.0.
    def token_probability(token, language)
      if @tokens[language][token].to_f == 0.0
        1 / @tokens_total.to_f
      else
        @tokens[language][token].to_f / @language_tokens[language].to_f
      end
    end

    # Internal: Probably of a language occurring - P(C)
    #
    # language - Language to check.
    #
    # Returns Float between 0.0 and 1.0.
    def language_probability(language)
      Math.log(@languages[language].to_f / @languages_total.to_f)
    end
  end
end
