module Linguist
  module Strategy
    # Detects XML files based on root tag.
    class XML
      # Scope of the search for the root tag
      # Number of lines to check at the beginning of the file
      SEARCH_SCOPE = 2

      # Public: Use the root tag to detect the XML blobs, only if no other
      # candidates were previously identified.
      #
      # blob               - An object that quacks like a blob.
      # candidates         - A list of candidate languages.
      #
      # Examples
      #
      #   XML.call(FileBlob.new("path/to/file"))
      #
      # Returns the list of candidates if it wasn't empty, an array with the
      # XML language as sole item if the root tag is detected, and an empty
      # Array otherwise.
      def self.call(blob, candidates = [])
        return candidates if candidates.any?

        header = blob.first_lines(SEARCH_SCOPE).join("\n")
        /<?xml version=/.match(header) ? [Language["XML"]] : []
      end
    end
  end
end
