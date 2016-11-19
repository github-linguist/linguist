module Linguist
  module Strategy
    class Modeline
      EMACS_MODELINE = /
        -\*-
        (?:
          # Short form: `-*- ruby -*-`
          \s* (?= [^:;\s]+ \s* -\*-)
          |
          # Longer form: `-*- foo:bar; mode: ruby; -*-`
          (?:
            .*?       # Preceding variables: `-*- foo:bar bar:baz;`
            [;\s]     # Which are delimited by spaces or semicolons
            |
            (?<=-\*-) # Not preceded by anything: `-*-mode:ruby-*-`
          )
          mode        # Major mode indicator
          \s*:\s*     # Allow whitespace around colon: `mode : ruby`
        )
        ([^:;\s]+)    # Name of mode

        # Ensure the mode is terminated correctly
        (?=
          # Followed by semicolon or whitespace
          [\s;]
          |
          # Touching the ending sequence: `ruby-*-`
          (?<![-*])   # Don't allow stuff like `ruby--*-` to match; it'll invalidate the mode
          -\*-        # Emacs has no problems reading `ruby --*-`, however.
        )
        .*?           # Anything between a cleanly-terminated mode and the ending -*-
        -\*-
      /xi

      VIM_MODELINE   = /

        # Start modeline. Could be `vim:`, `vi:` or `ex:`
        (?:
          (?:\s|^)
          vi
          (?:m[<=>]?\d+|m)? # Version-specific modeline
          |
          [\t\x20] # `ex:` requires whitespace, because "ex:" might be short for "example:"
          ex
        )

        # If the option-list begins with `set ` or `se `, it indicates an alternative
        # modeline syntax partly-compatible with older versions of Vi. Here, the colon
        # serves as a terminator for an option sequence, delimited by whitespace.
        (?=
          # So we have to ensure the modeline ends with a colon
          : (?=\s* set? \s [^\n:]+ :) |

          # Otherwise, it isn't valid syntax and should be ignored
          : (?!\s* set? \s)
        )

        # Possible (unrelated) `option=value` pairs to skip past
        (?:
          # Option separator. Vim uses whitespace or colons to separate options (except if
          # the alternate "vim: set " form is used, where only whitespace is used)
          (?:
            \s
            |
            \s* : \s* # Note that whitespace around colons is accepted too:
          )           # vim: noai :  ft=ruby:noexpandtab

          # Option's name. All recognised Vim options have an alphanumeric form.
          \w*

          # Possible value. Not every option takes an argument.
          (?:
            # Whitespace between name and value is allowed: `vim: ft   =ruby`
            \s*=

            # Option's value. Might be blank; `vim: ft= ` says "use no filetype".
            (?:
              [^\\\s] # Beware of escaped characters: titlestring=\ ft=ruby
              |       # will be read by Vim as { titlestring: " ft=ruby" }.
              \\.
            )*
          )?
        )*

        # The actual filetype declaration
        [\s:] (?:filetype|ft|syntax) \s*=

        # Language's name
        (\w+)

        # Ensure it's followed by a legal separator
        (?=\s|:|$)
      /xi

      MODELINES = [EMACS_MODELINE, VIM_MODELINE]

      # Scope of the search for modelines
      # Number of lines to check at the beginning and at the end of the file
      SEARCH_SCOPE = 5

      # Public: Detects language based on Vim and Emacs modelines
      #
      # blob               - An object that quacks like a blob.
      #
      # Examples
      #
      #   Modeline.call(FileBlob.new("path/to/file"))
      #
      # Returns an Array with one Language if the blob has a Vim or Emacs modeline
      # that matches a Language name or alias. Returns an empty array if no match.
      def self.call(blob, _ = nil)
        header = blob.lines.first(SEARCH_SCOPE).join("\n")
        footer = blob.lines.last(SEARCH_SCOPE).join("\n")
        Array(Language.find_by_alias(modeline(header + footer)))
      end

      # Public: Get the modeline from the first n-lines of the file
      #
      # Returns a String or nil
      def self.modeline(data)
        match = MODELINES.map { |regex| data.match(regex) }.reject(&:nil?).first
        match[1] if match
      end
    end
  end
end
