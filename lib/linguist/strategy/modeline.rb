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
      # Returns an Array with one Language if the blob has a shebang with a valid
      # interpreter, or empty if there is no shebang.
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
        data.lines.first(5).any? { |l| l.match(/\W(?:filetype|ft)=\s*(\w+)/) }
        lang = $1
      end
    end
  end
end
