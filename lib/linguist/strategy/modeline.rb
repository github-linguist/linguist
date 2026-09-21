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
        # Start of modeline (syntax documented in E520)
        (?:
          # `vi:`, `vim:`, `Vim:`, or a versioned marker such as `vim<700:`
          (?:^|[ \t]) (?:vi|[Vv]im(?:[<=>]?[0-9]+)?)
          |

          # `ex:`, which requires leading whitespace to avoid matching stuff like "lex:"
          [ \t] ex
        ) : [ \t]*

        (?:
          # `se`/`set` forms have whitespace-delimited options and a terminating colon.
          set? [ \t]+
          (?: \w* (?:[ \t]*=(?:[^\\\s:]|\\.)*)? [ \t]+ )*
          (?:filetype|ft|syntax) [ \t]*= (\w+)
          (?: [ \t][^\r\n:]*: | : )
          |

          # Ordinary forms have whitespace- or colon-delimited options.
          (?:
            (?:
              # Exclude exactly `se` and `set` before whitespace, without lookahead.
              (?: [^\Ws]\w* | s[^\We]\w* | se[^\Wt]\w* | set\w+ | s )
              (?:[ \t]*=(?:[^\\\s]|\\.)*)?
              (?:[ \t]*:[ \t]*|[ \t])
              |
              # Immediately followed by `=` or `:`, `se`/`set` are not commands.
              set? = (?:[^\\\s]|\\.)* (?:[ \t]*:[ \t]*|[ \t])
              |
              (?:set?)? : [ \t]*
            )
            # Escaped spaces belong to values, not to the next option.
            (?: \w* (?:[ \t]*=(?:[^\\\s]|\\.)*)? (?:[ \t]*:[ \t]*|[ \t]) )*
          )?
          (?:filetype|ft|syntax) [ \t]*= (\w+)
          (?:$|\s|:)
        )
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
        match.captures.compact.first if match
      end
    end
  end
end
