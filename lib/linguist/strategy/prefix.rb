module Linguist
  module Strategy
    class Prefix
      # Public: Detect languages based on common filename prefixes.
      #
      # blob               - An object that quacks like a blob.
      # candidates         - A list of candidate languages.
      #
      # Examples
      #
      #   Prefix.call FileBlob.new("Makefile.amd64")
      #   Prefix.call FileBlob.new("Dockerfile.production")
      #
      # Returns an array of languages.
      def self.call(blob, candidates)
        languages = Language.find_by_prefix(blob.name.to_s)
        candidates.any? ? candidates & languages : languages
      end
    end
  end
end
