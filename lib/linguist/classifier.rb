require 'linguist/tokenizer'

module Linguist
  # Language bayesian classifier.
  class Classifier
    CLASSIFIER_CONSIDER_BYTES = 50 * 1024

    # Public: Use the classifier to detect language of the blob.
    #
    # blob               - An object that quacks like a blob.
    # possible_languages - Array of Language objects
    #
    # Examples
    #
    #   Classifier.call(FileBlob.new("path/to/file"), [
    #     Language["Ruby"], Language["Python"]
    #   ])
    #
    # Returns an Array of Language objects, most probable first.
    def self.call(blob, possible_languages)
      language_names = possible_languages.map(&:name)
      classify(Samples.cache, blob.data[0...CLASSIFIER_CONSIDER_BYTES], language_names).map do |name, _|
        Language[name] # Return the actual Language objects
      end
    end

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
    #
    # Set LINGUIST_DEBUG=1 or =2 to see probabilities per-token or
    # per-language.  See also #dump_all_tokens, below.
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
      @unknown_logprob = Math.log(1 / db['tokens_total'].to_f)
    end

    # Internal: Guess language of data
    #
    # data      - Array of tokens or String data to analyze.
    # languages - Array of language name Strings to restrict to.
    #
    # Returns sorted Array of result pairs. Each pair contains the
    # String language name and a Float score.
    def classify(tokens, languages)
      return [] if tokens.nil? || languages.empty?
      tokens = Tokenizer.tokenize(tokens) if tokens.is_a?(String)
      scores = {}

      debug_dump_all_tokens(tokens, languages) if verbosity >= 2

      counts = Hash.new(0)
      tokens.each { |tok| counts[tok] += 1 }

      languages.each do |language|
        scores[language] = tokens_probability(counts, language) + language_probability(language)
        debug_dump_probabilities(counts, language, scores[language]) if verbosity >= 1
      end

      scores.sort { |a, b| b[1] <=> a[1] }.map { |score| [score[0], score[1]] }
    end

    # Internal: Probably of set of tokens in a language occurring - P(D | C)
    #
    # tokens   - Array of String tokens.
    # language - Language to check.
    #
    # Returns Float between 0.0 and 1.0.
    def tokens_probability(counts, language)
      sum = 0
      counts.each do |token, count|
        sum += count * token_probability(token, language)
      end
      sum
    end

    # Internal: Log-probability of token in language occurring - P(F | C)
    #
    # token    - String token.
    # language - Language to check.
    #
    # Returns Float.
    def token_probability(token, language)
      count = @tokens[language][token]
      if count.nil? || count == 0
        # This is usually the most common case, so we cache the result.
        @unknown_logprob
      else
        Math.log(count.to_f / @language_tokens[language].to_f)
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

    private
      def verbosity
        @verbosity ||= (ENV['LINGUIST_DEBUG'] || 0).to_i
      end

      def debug_dump_probabilities(tokens, language, score)
        printf("%10s = %10.3f + %7.3f = %10.3f\n",
            language, tokens_probability(tokens, language), language_probability(language), score)
      end

      # Internal: show a table of probabilities for each <token,language> pair.
      #
      # The number in each table entry is the number of "points" that each
      # token contributes toward the belief that the file under test is a
      # particular language.  Points are additive.
      #
      # Points are the number of times a token appears in the file, times
      # how much more likely (log of probability ratio) that token is to
      # appear in one language vs. the least-likely language.  Dashes
      # indicate the least-likely language (and zero points) for each token.
      def debug_dump_all_tokens(tokens, languages)
        maxlen = tokens.map { |tok| tok.size }.max

        printf "%#{maxlen}s", ""
        puts "    #" + languages.map { |lang| sprintf("%10s", lang) }.join

        token_map = Hash.new(0)
        tokens.each { |tok| token_map[tok] += 1 }

        token_map.sort.each { |tok, count|
          arr = languages.map { |lang| [lang, token_probability(tok, lang)] }
          min = arr.map { |a,b| b }.min
          if !arr.inject(true) { |result, n| result && n[1] == arr[0][1] }
            printf "%#{maxlen}s%5d", tok, count

            puts arr.map { |ent|
              ent[1] == min ? "         -" : sprintf("%10.3f", count * (ent[1] - min))
            }.join
          end
        }
      end
  end
end
