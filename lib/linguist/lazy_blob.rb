require 'linguist/blob_helper'
require 'linguist/language'
require 'linguist/source/repository'
require 'linguist/source/rugged'

module Linguist
  class LazyBlob
    GIT_ATTR = ['linguist-documentation',
                'linguist-language',
                'linguist-vendored',
                'linguist-generated',
                'linguist-detectable']

    # DEPRECATED: use Linguist::Source::RuggedRepository::GIT_ATTR_OPTS instead
    GIT_ATTR_OPTS = Linguist::Source::RuggedRepository::GIT_ATTR_OPTS

    # DEPRECATED: use Linguist::Source::RuggedRepository::GIT_ATTR_FLAGS instead
    GIT_ATTR_FLAGS = Linguist::Source::RuggedRepository::GIT_ATTR_FLAGS

    include BlobHelper

    MAX_SIZE = 128 * 1024

    attr_reader :repository
    attr_reader :oid
    attr_reader :path
    attr_reader :mode

    alias :name :path

    def initialize(repo, oid, path, mode = nil)
      @repository = if repo.is_a? Linguist::Source::Repository
        repo
      else
        # Allow this for backward-compatibility purposes
        Linguist::Source::RuggedRepository.new(repo)
      end
      @oid = oid
      @path = path
      @mode = mode
      @data = nil
    end

    def git_attributes
      @git_attributes ||= repository.load_attributes_for_path(name, GIT_ATTR)
    end

    def documentation?
      if not git_attributes['linguist-documentation'].nil?
        boolean_attribute(git_attributes['linguist-documentation'])
      else
        super
      end
    end

    def generated?
      if not git_attributes['linguist-generated'].nil?
        boolean_attribute(git_attributes['linguist-generated'])
      else
        super
      end
    end

    def vendored?
      if not git_attributes['linguist-vendored'].nil?
        boolean_attribute(git_attributes['linguist-vendored'])
      else
        super
      end
    end

    def language
      return @language if defined?(@language)

      @language = if lang = git_attributes['linguist-language']
        Language.find_by_alias(lang)
      else
        super
      end
    end

    def detectable?
      if not git_attributes['linguist-detectable'].nil?
        boolean_attribute(git_attributes['linguist-detectable'])
      else
        nil
      end
    end

    def data
      load_blob!
      @data
    end

    def size
      load_blob!
      @size
    end

    def symlink?
      # We don't create LazyBlobs for symlinks.
      false
    end

    def cleanup!
      @data.clear if @data
    end

    protected

    # Returns true if the attribute is present and not the string "false" and not the false boolean.
    def boolean_attribute(attribute)
      attribute != "false" && attribute != false
    end

    def load_blob!
      @data, @size = repository.load_blob(oid, MAX_SIZE) if @data.nil?
    end
  end
end
