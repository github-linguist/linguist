module Linguist
  module Strategy
    # Detect a well-formed man(7) or mdoc(7) manpage
    class Manpage
      # Number of lines to search at the beginning of the file
      SEARCH_SCOPE = 100

      # RegExp for matching conventional manpage extensions
      MANPAGE_EXTS = /\.(?:[1-9](?![1-9])[a-z_0-9]*|0p|n|man|mdoc)(?:\.in)?$/i

      # RegExp for matching man(7) and mdoc(7) title macros 
      TITLE_MACRO = /
        ^ [.']
        [ \t]* (?<macro> TH|Dt)
        [ \t]+ (?<name>  "[^\n"]+"|[^ \s"]+)
        [ \t]+ (?<sect>  "[^\n"]+"|[^ \s"]+)
      /x

      # Public: Detect a Roff manpage based on its content and file extension.
      #
      # blob       - An object that quacks like a blob.
      # candidates - A list of candidate languages
      #
      # Returns the candidates array if it wasn't empty, [Language["Roff"]]
      # if the strategy matched, and an empty array if the strategy didn't. 
      def self.call(blob, candidates = [])
        return candidates if candidates.any?
        valid_manpage?(blob) ? [Language["Roff"]] : []
      end

      # Wrapper method which handles the actual checking routines.
      def self.valid_manpage?(blob)
        if MANPAGE_EXTS.match?(blob.name)
          header = blob.first_lines(SEARCH_SCOPE).join("\n")
          if match = TITLE_MACRO.match(header)
            title_matches_filename?(match, blob) ||
            is_header_conventional?(match, header)
          end
        end
      end

      # Internal: First test we make, which should match most manpages.
      #
      # match - A #<MatchData> object returned from the extension match
      # blob  - An object which quacks like a blob
      #
      # Returns a Boolean.
      def self.title_matches_filename?(match, blob)
        file_name  = blob.name.downcase.gsub(/\.[^.]+$/, "")
        file_sect  = blob.extension.downcase.gsub(/^\./, "")
        param_name = match[:name].downcase.gsub(/^"|"$/, "")
        param_sect = match[:sect].downcase.gsub(/^"|"$/, "")

        # NB: The /^@.+@$/ matches are for autoconf variables
        (file_name == param_name || /^@.+@$/.match?(param_name)) &&
        (file_sect == param_sect || /^@.+@$/.match?(param_sect))
      end

      # Internal: Second test that's run if the first happened to fail. 
      #
      # match - A #<MatchData> object returned from the extension match
      # blob  - An object which quacks like a blob
      #
      # Returns a Boolean.
      def self.is_header_conventional?(match, header)
        if "Dt" == match[:macro]
          # mdoc: Well-formed prologues will contain the following:
          /^[.'][ \t]*Dd[ \t]+\S/.match?(header) && # Document date
          /^[.'][ \t]*Os(?=\s|$)/.match?(header) && # System identifier
          /^[.'][ \t]*Sh[ \t]+(NAME\b|"NAME")/.match?(header) # A "NAME" section
        else
          # man: Few expectations apply, so just match two common sections:
          /^[.'][ \t]*SH[ \t]+(NAME\b|"NAME")/.match?(header) &&      # "NAME"
          /^[.'][ \t]*SH[ \t]+(SYNOPSIS\b|"SYNOPSIS")/.match?(header) # "SYNOPSIS"
        end
      end
    end
  end
end
