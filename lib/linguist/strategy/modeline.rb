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

      VIM_MODELINE_START = '(?:(?:^|[ \t])(?:vi|(?:vim|Vim)(?:[<=>]?[0-9]+)?):|[ \t]ex:)'
      VIM_MODELINE_VALUE = '(?:[^\\\\\s]|\\\\.)*'
      VIM_MODELINE_OPTION = "[A-Za-z0-9_]*(?:[ \\t]*=#{VIM_MODELINE_VALUE})?"
      VIM_MODELINE_SEPARATOR = '(?:[ \t]*:[ \t]*|[ \t])'

      # The ordinary form must not begin with `se` or `set`, which select the
      # alternative form whose whitespace-delimited option list ends in a colon.
      VIM_MODELINE_NON_SET_NAME = '(?:[A-RT-Za-rt-z0-9_][A-Za-z0-9_]*|s(?:[A-Za-df-z0-9_][A-Za-z0-9_]*|e(?:[A-SU-Za-su-z0-9_][A-Za-z0-9_]*|t[A-Za-z0-9_]+))?)'
      VIM_MODELINE_NON_SET_OPTION = "#{VIM_MODELINE_NON_SET_NAME}(?:[ \\t]*=#{VIM_MODELINE_VALUE})?"
      VIM_MODELINE_LANGUAGE = 'VIM_MODELINE_LANGUAGE'
      VIM_MODELINE_TARGET = "(?:filetype|ft|syntax)[ \\t]*=#{VIM_MODELINE_LANGUAGE}"
      VIM_MODELINE_TAIL = "(?:#{VIM_MODELINE_OPTION}#{VIM_MODELINE_SEPARATOR})*#{VIM_MODELINE_TARGET}"
      VIM_MODELINE_NORMAL = "#{VIM_MODELINE_START}[ \\t]*(?:#{VIM_MODELINE_TARGET}|#{VIM_MODELINE_NON_SET_OPTION}#{VIM_MODELINE_SEPARATOR}#{VIM_MODELINE_TAIL}|:[ \\t]*#{VIM_MODELINE_TAIL})(?:$|[\\s:])"
      VIM_MODELINE_SET = "#{VIM_MODELINE_START}[ \\t]*set?[ \\t]+(?:#{VIM_MODELINE_OPTION}[ \\t]+)*#{VIM_MODELINE_TARGET}(?:[ \\t][^\\r\\n:]*:|:)"

      # The Vim Help heuristic specializes this portable grammar by replacing
      # VIM_MODELINE_LANGUAGE with `help`.
      VIM_MODELINE_PATTERN = "(?:#{VIM_MODELINE_NORMAL}|#{VIM_MODELINE_SET})"
      VIM_MODELINE = Regexp.new(VIM_MODELINE_PATTERN.gsub(VIM_MODELINE_LANGUAGE, '([A-Za-z0-9_]+)'))

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
        match&.captures&.compact&.first
      end
    end
  end
end
