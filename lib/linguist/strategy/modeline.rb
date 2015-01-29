module Linguist
  module Strategy
    class Modeline
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
        if language = Language.find_by_alias(modeline(blob.data))
          return [language]
        else
          return []
        end
      end

      # Public: Get the modeline from the first n-lines of the file
      #
      # Returns a String or nil
      def self.modeline(data)
        regex =
          /(?:
              (-\*- \s* (?:mode:)? \s*) |                  # $1: Emacs
              (\/\* \s* vim: \s* set \s* (?:ft|filetype)=) # $2: Vim
            )
            (\w+)                                          # $3: language
            (?:
              (?(1)                                        # If $1 matched...
                ;?\s* -\*- |                               # then close Emacs syntax
                : \s* \*\/                                 # otherwise close Vim syntax
              )
            )/x

        data.lines.first(5).any? { |l| l.match(regex) }
        lang = $3
      end
    end
  end
end
