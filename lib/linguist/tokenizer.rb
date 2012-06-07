module Linguist
  # Generic programming language tokenizer.
  #
  # Tokens are designed for use in the language bayes classifier.
  # It strips any data strings or comments and preserves significant
  # language symbols.
  class Tokenizer
    def initialize(data)
      @data = data
    end

    # Get source data.
    #
    # Returns String.
    attr_reader :data

    # Extract tokens from data.
    #
    # Returns Array of Strings.
    def tokens
      s = StringScanner.new(data)

      tokens = []
      until s.eos?
        # Ruby single line comment
        if token = s.scan(/# /)
          tokens << "#"
          s.skip_until(/\n|\Z/)

        # C style single line comment
        elsif token = s.scan(/\/\/ /)
          tokens << "//"
          s.skip_until(/\n|\Z/)

        # C multiline comments
        elsif token = s.scan(/\/\*/)
          tokens << "/*"
          s.skip_until(/\*\//)
          tokens << "*/"

        # Haskell multiline comments
        elsif token = s.scan(/\{-/)
          tokens << "{-"
          s.skip_until(/-\}/)
          tokens << "-}"

        # XML multiline comments
        elsif token = s.scan(/<!--/)
          tokens << "<!--"
          s.skip_until(/-->/)
          tokens << "-->"

        elsif s.scan(/"/)
          s.skip_until(/[^\\]"/)
        elsif s.scan(/'/)
          s.skip_until(/[^\\]'/)

        # Common programming punctuation
        elsif token = s.scan(/;|\{|\}|\(|\)/)
          tokens << token

        # Regular token
        elsif token = s.scan(/[\w\.@#\/<>]+/)
            tokens << token

        else
          s.getch
        end
      end

      tokens
    end
  end
end
