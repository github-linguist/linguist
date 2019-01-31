module Linguist
  module Strategy
    # Detects language based on File Path Pattern
    class FilePathPattern
      # Public: Use the filename to detect the blob's language.
      #
      # blob               - An object that quacks like a blob.
      # candidates         - A list of candidate languages.
      #
      # Examples
      #
      #   FilePathPattern.call(FileBlob.new("path/to/file"))
      #
      # Returns an array of languages with a associated blob's file Path pattern.
      # Selected languages must be in the candidate list, except if it's empty,
      # in which case any language is a valid candidate.
      def self.call(blob, candidates)
        path = blob.path.to_s
        languages = Language.find_by_filepath_pattern(path)
        candidates.any? ? candidates & languages : languages
      end
    end
  end
end
