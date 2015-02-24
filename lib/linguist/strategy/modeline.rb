module Linguist
  module Strategy
    class Modeline
      EmacsModeline = /-\*-\s*mode:\s*(\w+);?\s*-\*-/i
      VimModeline = /\/\*\s*vim:\s*set\s*(?:ft|filetype)=(\w+):\s*\*\//i

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
        Array(Language.find_by_alias(modeline(blob.data)))
      end

      # Public: Get the modeline from the first n-lines of the file
      #
      # Returns a String or nil
      def self.modeline(data)
        match = data.match(EmacsModeline) || data.match(VimModeline)
        match[1] if match
      end
    end
  end
end
