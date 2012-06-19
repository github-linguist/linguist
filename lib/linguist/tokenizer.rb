module Linguist
  # Generic programming language tokenizer.
  #
  # Tokens are designed for use in the language bayes classifier.
  # It strips any data strings or comments and preserves significant
  # language symbols.
  class Tokenizer
    # Public: Initialize a Tokenizer.
    #
    # data - String data to scan.
    def initialize(data)
      @data = data
    end

    # Public: Get source data.
    #
    # Returns String.
    attr_reader :data

    # Public: Extract tokens from data.
    #
    # Returns Array of token Strings.
    def tokens
      extract_tokens(data)
    end

    # Internal: Extract generic tokens from data.
    #
    # data - String to scan.
    #
    # Examples
    #
    #   extract_tokens("printf('Hello')")
    #   # => ['printf', '(', ')']
    #
    # Returns Array of token Strings.
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

        # Skip single or double quoted strings
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

    # Internal: Extract tokens from inside SGML tag.
    #
    # data - SGML tag String.
    #
    # Examples
    #
    #   extract_sgml_tokens("<a href='' class=foo>")
    #   # => ["<a>", "href="]
    #
    # Returns Array of token Strings.
    def extract_sgml_tokens(data)
      s = StringScanner.new(data)

      tokens = []

      until s.eos?
        # Emit start token
        if token = s.scan(/<\/?[^\s>]+/)
          tokens << "#{token}>"

        # Emit attributes with trailing =
        elsif token = s.scan(/\w+=/)
          tokens << token

          # Then skip over attribute value
          if s.scan(/"/)
            s.skip_until(/[^\\]"/)
          elsif s.scan(/'/)
            s.skip_until(/[^\\]'/)
          else
            s.skip_until(/\w+/)
          end

        # Emit lone attributes
        elsif token = s.scan(/\w+/)
          tokens << token

        # Stop at the end of the tag
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
