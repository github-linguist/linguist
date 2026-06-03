module Linguist
  module Strategy
    class Modeline
      EMACS_MODELINE = %r[
        (?-m)

        # Opening delimiter
        -\*-

        (?:
          # Short form: `-*- ruby -*-`
          [ \t]*
          (?=
            [^:;\s]+  # Name of mode
            [ \t]*    # Optional whitespace
            -\*-      # Closing delimiter
          )
          |

          # Longer form: `-*- foo:bar; mode: ruby; -*-`
          (?:
            .*?[ \t;] # Preceding variables: `-*- foo:bar bar:baz;`
            |
            (?<=-\*-) # Not preceded by anything: `-*-mode:ruby-*-`
          )

          # Explicitly-named variable: `mode: ruby` or `mode  : ruby`
          [ \t]* mode [ \t]* : [ \t]*
        )

        # Name of major-mode, which corresponds to syntax or filetype
        ([^:;\s]+)

        # Ensure the name is terminated correctly
        (?=
          # Followed by semicolon or whitespace
          [ \t;]
          |
          # Touching the ending sequence: `ruby-*-`
          (?<![-*])   # Don't allow stuff like `ruby--*-` to match; it'll invalidate the mode
          -\*-        # Emacs has no problems reading `ruby --*-`, however.
        )

        # If we've gotten this far, it means the modeline is valid.
        # We gleefully skip past everything up until reaching "-*-"
        .*?

        # Closing delimiter
        -\*-
      ]xi

      # NOTE: When changing this regex, be sure to keep the Vim Help heuristic updated too (#5347)
      VIM_MODELINE = %r[
        (?-m)

        # Start of modeline (syntax documented in E520)
        (?:
          # `vi:`, `vim:` or `Vim:`
          (?:^|[ \t]) (?:vi|Vi(?=m))

          # Check if specific Vim version(s) are requested (won't work in vi/ex)
          (?:
            # Versioned modeline. `vim<700:` targets Vim versions older than 7.0
            m
            [<=>]?    # If comparison operator is omitted, *only* this version is targeted
            [0-9]+    # Version argument = (MINOR_VERSION_NUMBER * 100) + MINOR_VERSION_NUMBER
            |

            # Unversioned modeline. `vim:` targets any version of Vim.
            m
          )?
          |

          # `ex:`, which requires leading whitespace to avoid matching stuff like "lex:"
          [ \t] ex
        )

        # If the option-list begins with `set ` or `se `, it indicates an alternative
        # modeline syntax partly-compatible with older versions of Vi. Here, the colon
        # serves as a terminator for an option sequence, delimited by whitespace.
        (?=
          # So we have to ensure the modeline ends with a colon
          : (?=[ \t]* set? [ \t] [^\r\n:]+ :) |

          # Otherwise, it isn't valid syntax and should be ignored
          : (?![ \t]* set? [ \t])
        )

        # Possible (unrelated) `option=value` pairs to skip past
        (?:
          # Option separator, either
          (?:
            # 1. A colon (possibly surrounded by whitespace)
            [ \t]* : [ \t]*     # vim: noai :  ft=sh:noexpandtab
            |

            # 2. At least one (horizontal) whitespace character
            [ \t]               # vim: noai ft=sh noexpandtab
          )

          # Option's name. All recognised Vim options have an alphanumeric form.
          \w*

          # Possible value. Not every option takes an argument.
          (?:
            # Whitespace between name and value is allowed: `vim: ft   =sh`
            [ \t]*=

            # Option's value. Might be blank; `vim: ft= ` means "use no filetype".
            (?:
              [^\\\s]    # Beware of escaped characters: titlestring=\ ft=sh
              |          # will be read by Vim as { titlestring: " ft=sh" }.
              \\.
            )*
          )?
        )*

        # The actual filetype declaration
        [ \t:] (?:filetype|ft|syntax) [ \t]*=

        # Language's name
        (\w+)

        # Ensure it's followed by a legal separator (including EOL)
        (?=$|\s|:)
      ]x

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
        return [] if blob.symlink?

        header = blob.first_lines(SEARCH_SCOPE).join("\n")
        # Return early for Vimball files as their modeline will not reflect their filetype.
        return [] if header.include?("UseVimball")

        footer = blob.last_lines(SEARCH_SCOPE).join("\n")
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
