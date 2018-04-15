module Linguist
  module Strategy
    # Detects language based on filename
    class Filename
      # Public: Use the filename to detect the blob's language.
      #
      # blob               - An object that quacks like a blob.
      # candidates         - A list of candidate languages.
      #
      # Examples
      #
      #   Filename.call(FileBlob.new("path/to/file"))
      #
      # Returns an Array of Languages associated blob's filename. Selected
      # Languages must be in the candidate list, except if it's empty, in which
      # case any language is a valid candidate.
      def self.call(blob, candidates)
        name = blob.name.to_s
        languages = Language.find_by_filename(name)
        if candidates.any? then candidates & languages else languages end
      end
    end
  end
end
