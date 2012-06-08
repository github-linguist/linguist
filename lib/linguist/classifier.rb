require 'linguist/tokenizer'

module Linguist
  # Language bayesian classifier.
  class Classifier
    PATH = File.expand_path('../classifier.yml', __FILE__)

    def self.exist?
      File.exist?(PATH)
    end

    def self.instance
      @instance ||= YAML.load_file(PATH)
    end

    def initialize
      @tokens_total    = 0
      @languages_total = 0
      @tokens          = Hash.new { |h, k| h[k] = Hash.new(0) }
      @language_tokens = Hash.new(0)
      @languages       = Hash.new(0)
    end

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
    end

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

    def classify(data)
      tokens = Tokenizer.new(data).tokens

      scores = {}
      @languages.keys.each do |language|
        scores[language] = tokens_probability(tokens, language) * language_probability(language)
      end

      scores.sort { |a, b| b[1] <=> a[1] }.map { |score| [Language[score[0]], score[1]] }
    end

    def tokens_probability(tokens, language)
      tokens.inject(1.0) do |sum, token|
        sum *= token_probability(token, language)
      end
    end

    def token_probability(token, language)
      if @tokens[language][token] == 0
        1 / @tokens_total.to_f
      else
        @tokens[language][token].to_f / @languages[language].to_f
      end
    end

    def language_probability(language)
      @languages[language].to_f / @languages_total.to_f
    end
  end

  # Eager load instance
  Classifier.instance if Classifier.exist?
end
