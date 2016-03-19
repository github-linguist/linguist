require 'linguist/lazy_blob'
require 'rugged'

module Linguist
  # A Repository is an abstraction of a Grit::Repo or a basic file
  # system tree. It holds a list of paths pointing to Blobish objects.
  #
  # Its primary purpose is for gathering language statistics across
  # the entire project.
  class Repository
    attr_reader :repository

    # Public: Create a new Repository based on the stats of
    # an existing one
    def self.incremental(repo, commit_oid, old_commit_oid, old_stats)
      repo = self.new(repo, commit_oid)
      repo.load_existing_stats(old_commit_oid, old_stats)
      repo
    end

    # Public: Initialize a new Repository to be analyzed for language
    # data
    #
    # repo - a Rugged::Repository object
    # commit_oid - the sha1 of the commit that will be analyzed;
    #              this is usually the master branch
    #
    # Returns a Repository
    def initialize(repo, commit_oid)
      @repository = repo
      @commit_oid = commit_oid

      @old_commit_oid = nil
      @old_stats = nil

      raise TypeError, 'commit_oid must be a commit SHA1' unless commit_oid.is_a?(String)
    end

    # Public: Load the results of a previous analysis on this repository
    # to speed up the new scan.
    #
    # The new analysis will be performed incrementally as to only take
    # into account the file changes since the last time the repository
    # was scanned
    #
    # old_commit_oid - the sha1 of the commit that was previously analyzed
    # old_stats - the result of the previous analysis, obtained by calling
    #             Repository#cache on the old repository
    #
    # Returns nothing
    def load_existing_stats(old_commit_oid, old_stats)
      @old_commit_oid = old_commit_oid
      @old_stats = old_stats
      nil
    end

    # Public: Returns a breakdown of language stats.
    #
    # Examples
    #
    #   # => { 'Ruby' => 46319,
    #          'JavaScript' => 258 }
    #
    # Returns a Hash of language names and Integer size values.
    def languages
      @sizes ||= begin
        sizes = Hash.new { 0 }
        cache.each do |_, (language, size)|
          sizes[language] += size
        end
        sizes
      end
    end

    # Public: Get primary Language of repository.
    #
    # Returns a language name
    def language
      @language ||= begin
        primary = languages.max_by { |(_, size)| size }
        primary && primary[0]
      end
    end

    # Public: Get the total size of the repository.
    #
    # Returns a byte size Integer
    def size
      @size ||= languages.inject(0) { |s,(_,v)| s + v }
    end

    # Public: Return the language breakdown of this repository by file
    #
    # Returns a map of language names => [filenames...]
    def breakdown_by_file
      @file_breakdown ||= begin
        breakdown = Hash.new { |h,k| h[k] = Array.new }
        cache.each do |filename, (language, _)|
          breakdown[language] << filename
        end
        breakdown
      end
    end

    # Public: Return the cached results of the analysis
    #
    # This is a per-file breakdown that can be passed to other instances
    # of Linguist::Repository to perform incremental scans
    #
    # Returns a map of filename => [language, size]
    def cache
      @cache ||= begin
        if @old_commit_oid == @commit_oid
          @old_stats
        else
          compute_stats(@old_commit_oid, @old_stats)
        end
      end
    end

    def read_index
      attr_index = Rugged::Index.new
      attr_index.read_tree(current_tree)
      repository.index = attr_index
    end

    def current_tree
      @tree ||= Rugged::Commit.lookup(repository, @commit_oid).tree
    end

    protected
    MAX_TREE_SIZE = 100_000

    def compute_stats(old_commit_oid, cache = nil)
      return {} if current_tree.count_recursive(MAX_TREE_SIZE) >= MAX_TREE_SIZE

      old_tree = old_commit_oid && Rugged::Commit.lookup(repository, old_commit_oid).tree
      read_index
      diff = Rugged::Tree.diff(repository, old_tree, current_tree)

      # Clear file map and fetch full diff if any .gitattributes files are changed
      if cache && diff.each_delta.any? { |delta| File.basename(delta.new_file[:path]) == ".gitattributes" }
        diff = Rugged::Tree.diff(repository, old_tree = nil, current_tree)
        file_map = {}
      else
        file_map = cache ? cache.dup : {}
      end

      diff.each_delta do |delta|
        old = delta.old_file[:path]
        new = delta.new_file[:path]

        file_map.delete(old)
        next if delta.binary

        if [:added, :modified].include? delta.status
          # Skip submodules and symlinks
          mode = delta.new_file[:mode]
          mode_format = (mode & 0170000)
          next if mode_format == 0120000 || mode_format == 040000 || mode_format == 0160000

          blob = Linguist::LazyBlob.new(repository, delta.new_file[:oid], new, mode.to_s(8))

          if blob.include_in_language_stats?
            file_map[new] = [blob.language.group.name, blob.size]
          end

          blob.cleanup!
        end
      end

      file_map
    end
  end
end
