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
    # language - String language of data
    # data     - String contents of file
    #
    # Examples
    #
    #   train('Ruby', "def hello; end")
    #
    # Returns nothing.
    def train(language, data)
      tokens = Tokenizer.tokenize(data)

      tokens.each do |token|
        @tokens[language][token] += 1
        @language_tokens[language] += 1
        @tokens_total += 1
      end
      @languages[language] += 1
      @languages_total += 1

      nil
    end

    # Public: Verify internal counts are consistent.
    #
    # Returns Boolean.
    def verify
      @languages.inject(0) { |n, (_, c)| n += c } == @languages_total &&
        @language_tokens.inject(0) { |n, (_, c)| n += c } == @tokens_total &&
        @tokens.inject(0) { |n, (_, ts)| n += ts.inject(0) { |m, (_, c)| m += c } } == @tokens_total
    end

    # Public: Prune infrequent tokens.
    #
    # Returns receiver Classifier instance.
    def gc
      self
    end

    # Public: Guess language of data.
    #
    # data      - Array of tokens or String data to analyze.
    # languages - Array of language name Strings to restrict to.
    #
    # Examples
    #
    #   classify("def hello; end")
    #   # => [ 'Ruby', 0.90], ['Python', 0.2], ... ]
    #
    # Returns sorted Array of result pairs. Each pair contains the
    # String language name and a Float score.
    def classify(tokens, languages = @languages.keys)
      return [] if tokens.nil?
      tokens = Tokenizer.tokenize(tokens) if tokens.is_a?(String)

      scores = {}
      languages.each do |language|
        scores[language] = tokens_probability(tokens, language) +
                                   language_probability(language)
      end

      scores.sort { |a, b| b[1] <=> a[1] }.map { |score| [score[0], score[1]] }
    end

    # Internal: Probably of set of tokens in a language occuring - P(D | C)
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

    # Internal: Probably of token in language occuring - P(F | C)
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

    # Internal: Probably of a language occuring - P(C)
    #
    # language - Language to check.
    #
    # Returns Float between 0.0 and 1.0.
    def language_probability(language)
      Math.log(@languages[language].to_f / @languages_total.to_f)
    end

    # Public: Serialize classifier to YAML.
    #
    # opts - Hash of YAML options.
    #
    # Returns nothing.
    def to_yaml(io)
      data = "--- !ruby/object:Linguist::Classifier\n"

      escape = lambda { |s| s.inspect.gsub(/\\#/, "\#") }

      data << "languages_total: #{@languages_total}\n"
      data << "tokens_total: #{@tokens_total}\n"

      data << "languages:\n"
      @languages.sort.each do |language, count|
        data << "  #{escape.call(language)}: #{count}\n"
      end

      data << "language_tokens:\n"
      @language_tokens.sort.each do |language, count|
        data << "  #{escape.call(language)}: #{count}\n"
      end

      data << "tokens:\n"
      @tokens.sort.each do |language, tokens|
        data << "  #{escape.call(language)}:\n"
        tokens.sort.each do |token, count|
          data << "    #{escape.call(token)}: #{count}\n"
        end
      end

      io.write data
      nil
    end
  end

  # Eager load instance
  Classifier.instance if Classifier.exist?
end
