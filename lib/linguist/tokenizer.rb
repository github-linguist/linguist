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
      extract_tokens(data)
    end

    def extract_tokens(data)
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

        # SGML style brackets
        elsif token = s.scan(/<[^>]+>/)
          extract_sgml_tokens(token).each { |t| tokens << t }

        # Common programming punctuation
        elsif token = s.scan(/;|\{|\}|\(|\)|<<?/)
          tokens << token

        # Regular token
        elsif token = s.scan(/[\w\.@#\/]+/)
            tokens << token

        else
          s.getch
        end
      end

      tokens
    end

    def extract_sgml_tokens(data)
      s = StringScanner.new(data)

      tokens = []

      until s.eos?
        if token = s.scan(/<\/?[^\s>]+/)
          tokens << "#{token}>"

        elsif token = s.scan(/\w+=/)
          tokens << token

          if s.scan(/"/)
            s.skip_until(/[^\\]"/)
          elsif s.scan(/'/)
            s.skip_until(/[^\\]'/)
          else
            s.skip_until(/\w+/)
          end

        elsif token = s.scan(/\w+/)
          tokens << token

        elsif s.scan(/>/)
          s.terminate

        else
          s.getch
        end
      end

      tokens
    end
  end
end
