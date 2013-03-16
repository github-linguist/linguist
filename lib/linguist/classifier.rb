require 'linguist/tokenizer'

module Linguist
  # Language bayesian classifier.
  class Classifier
    # Load the C extension
    require 'linguist/cclassifier'

    # Public: Train classifier that data is a certain language.
    #
    # db       - Instance of Classifier::TrainingDB
    # language - String language of data
    # data     - String contents of file
    #
    # Examples
    #
    #   Classifier.train(db, 'Ruby', "def hello; end")
    #
    # Returns nothing.
    def self.train!(db, language, data)
      db.train! language, Tokenizer.tokenize(data)
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
      languages ||= db.language_names
      new(db).classify(tokens, languages)
    end

    # Internal: Initialize a Classifier.
    def initialize(db = nil)
      @database = db || TrainingDB.new
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
      languages.each do |lang_name|
        if lang = @database[lang_name]
          scores[lang_name] = tokens_probability(tokens, lang) + language_probability(lang)
        end
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
      probability = language.token_probability(token)

      if probability == 0.0
        1.0 / @database.tokens_total
      else
        probability / language.token_count
      end
    end

    # Internal: Probably of a language occurring - P(C)
    #
    # language - Language to check.
    #
    # Returns Float between 0.0 and 1.0.
    def language_probability(language)
      Math.log(language.sample_count / @database.samples_total)
    end
  end
end
