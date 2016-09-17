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

      # First form vim modeline
      # [text]{white}{vi:|vim:|ex:}[white]{options}
      # ex: 'vim: syntax=ruby'
      VIM_MODELINE_1 = /(?:vim|vi|ex):\s*(?:ft|filetype|syntax)=(\w+)\s?/i

      # Second form vim modeline (compatible with some versions of Vi)
      # [text]{white}{vi:|vim:|Vim:|ex:}[white]se[t] {options}:[text]
      # ex: 'vim set syntax=ruby:'
      VIM_MODELINE_2 = /(?:vim|vi|Vim|ex):\s*se(?:t)?.*\s(?:ft|filetype|syntax)=(\w+)\s?.*:/i

      MODELINES = [EMACS_MODELINE, VIM_MODELINE_1, VIM_MODELINE_2]

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
