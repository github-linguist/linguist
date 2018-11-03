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
        ^[.']     [ \t]*
        (?:TH|Dt) [ \t]+
        (?<name> "[^\n"]+"|[^ \s"]+) [ \t]+
        (?<sect> "[^\n"]+"|[^ \s"]+)
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
          lines = blob.first_lines(SEARCH_SCOPE).join("\n")

          if match = TITLE_MACRO.match(lines)
            file_name = blob.name.downcase.gsub(/\.[^.]+$/, "")
            file_sect = blob.extension.downcase.gsub(/^\./, "")
            param_name = match[:name].downcase.gsub(/^"|"$/, "")
            param_sect = match[:sect].downcase.gsub(/^"|"$/, "")

            # Be lenient towards autoconf variables like "@NAME@"
            autoconf_name = /^@.+@$/.match?(param_name)
            autoconf_sect = /^@.+@$/.match?(param_sect)
        
            return (autoconf_name || file_name == param_name) &&
                   (autoconf_sect || file_sect == param_sect)
          end
        end
      end
    end
  end
end
