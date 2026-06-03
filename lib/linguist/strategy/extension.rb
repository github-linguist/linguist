require 'yaml'

module Linguist
  module Strategy

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
        return candidates if generic? blob.name.to_s
        languages = Language.find_by_extension(blob.name.to_s)
        candidates.any? ? candidates & languages : languages
      end

      # Public: Return true if filename uses a generic extension.
      def self.generic?(filename)
        self.load
        @generic.any? { |ext| filename.downcase.end_with? ext }
      end

      @generic = []

      # Internal: Load the contents of `generic.yml`
      def self.load()
        return if @generic.any?
        data = YAML.load_file(File.expand_path("../../generic.yml", __FILE__))
        @generic = data['extensions']
      end
    end
  end
end
