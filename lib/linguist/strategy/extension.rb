module Linguist
  module Strategy
    # Extensions considered too common for simple extension lookup.
    # Languages which use such an extension must instead satisfy a
    # heuristic (or some other strategy) to be matched on extension.
    GENERIC = %w[.sol]

    # Detects language based on extension
    class Extension
      # Public: Use the file extension to detect the blob's language.
      #
      # blob               - An object that quacks like a blob.
      # candidates         - A list of candidate languages.
      #
      # Examples
      #
      #   Extension.call(FileBlob.new("path/to/file"))
      #
      # Returns an array of languages associated with a blob's file extension.
      # Selected languages must be in the candidate list, except if it's empty,
      # in which case any language is a valid candidate.
      def self.call(blob, candidates)
        return candidates if GENERIC.any? { |ext| blob.name.downcase.end_with? ext }
        languages = Language.find_by_extension(blob.name.to_s)
        candidates.any? ? candidates & languages : languages
      end
    end
  end
end
