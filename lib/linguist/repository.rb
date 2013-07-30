require 'linguist/file_blob'

module Linguist
  # A Repository is an abstraction of a Grit::Repo or a basic file
  # system tree. It holds a list of paths pointing to Blobish objects.
  #
  # Its primary purpose is for gathering language statistics across
  # the entire project.
  class Repository
    # Public: Initialize a new Repository from a File directory
    #
    # base_path - A path String
    #
    # Returns a Repository
    def self.from_directory(base_path)
      new Dir["#{base_path}/**/*"].
        select { |f| File.file?(f) }.
        map { |path| FileBlob.new(path, base_path) }
    end

    # Public: Initialize a new Repository
    #
    # enum - Enumerator that responds to `each` and
    #        yields Blob objects
    #
    # Returns a Repository
    def initialize(enum)
      @enum = enum
      @computed_stats = false
      @language = @size = nil
      @sizes = Hash.new { 0 }
    end

    # Public: Returns a breakdown of language stats.
    #
    # Examples
    #
    #   # => { Language['Ruby'] => 46319,
    #          Language['JavaScript'] => 258 }
    #
    # Returns a Hash of Language keys and Integer size values.
    def languages
      compute_stats
      @sizes
    end

    # Public: Get primary Language of repository.
    #
    # Returns a Language
    def language
      compute_stats
      @language
    end

    # Public: Get the total size of the repository.
    #
    # Returns a byte size Integer
    def size
      compute_stats
      @size
    end

    # Internal: Compute language breakdown for each blob in the Repository.
    #
    # Returns nothing
    def compute_stats
      return if @computed_stats

      @enum.each do |blob|
        # Skip files that are likely binary
        next if blob.likely_binary?

        # Skip vendored or generated blobs
        next if blob.vendored? || blob.generated? || blob.language.nil?

        # Only include programming languages and acceptable markup languages
        if blob.language.type == :programming || Language.detectable_markup.include?(blob.language.name)
          @sizes[blob.language.group] += blob.size
        end
      end

      # Compute total size
      @size = @sizes.inject(0) { |s,(_,v)| s + v }

      # Get primary language
      if primary = @sizes.max_by { |(_, size)| size }
        @language = primary[0]
      end

      @computed_stats = true

      nil
    end
  end
end
