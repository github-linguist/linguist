require 'linguist/file_blob'

module Linguist
  # A Repository is an abstraction of a Grit::Repo or a basic file
  # system tree. It holds a list of paths pointing to Blobish objects.
  #
  # Its primary purpose is for gathering langauge statistics across
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
    # paths - An Array of Blob objects or a Hash of String path keys
    #         and Blob values.
    #
    # Returns a Repository
    def initialize(paths)
      case paths
      when Array
        @paths = paths.inject({}) do |h, blob|
          h[blob.name] = blob
          h
        end
      when Hash
        @paths = paths
      else
        raise ArgumentError, "#{paths.class} is not an Array or Hash"
      end

      @computed_stats = false
      @language = @size = nil
      @sizes = Hash.new { 0 }
    end

    # Public: Lookup blob for path.
    #
    # Returns a Blob
    def [](path)
      @paths[path]
    end

    # Public: Returns a breakdown of langauge stats.
    #
    # Examples
    #
    #   # => { Langauge['Ruby'] => 46319,
    #          Langauge['JavaScript'] => 258 }
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

      @paths.each do |path, blob|
        # Skip vendored or generated blobs
        next if blob.vendored? || blob.generated?

        language = blob.language

        # Only include common langauges
        if language && language.common?
          @sizes[language] += blob.size
        end
      end

      # Compute total size
      @size = @sizes.inject(0) { |s,(k,v)| s + v }

      # Get primary language
      if primary = @sizes.max_by { |(_, size)| size }
        @language = primary[0]
      end

      @computed_stats = true

      nil
    end
  end
end
