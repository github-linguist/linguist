require 'strscan'
require 'linguist/linguist'

module Linguist
  # Generic programming language tokenizer.
  #
  # Tokens are designed for use in the language bayes classifier.
  # It strips any data strings or comments and preserves significant
  # language symbols.
  class Tokenizer
    # Public: Extract tokens from data
    #
    # data - String to tokenize
    #
    # Returns Array of token Strings.
    def self.tokenize(data)
      new.extract_tokens(data)
    end
  end
end
