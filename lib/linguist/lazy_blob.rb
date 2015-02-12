require 'linguist/blob_helper'
require 'linguist/language'
require 'rugged'

module Linguist
  class LazyBlob
    GIT_ATTR = ['linguist-documentation', 'linguist-language', 'linguist-vendored']
    GIT_ATTR_OPTS = { :priority => [:index], :skip_system => true }
    GIT_ATTR_FLAGS = Rugged::Repository::Attributes.parse_opts(GIT_ATTR_OPTS)

    include BlobHelper

    MAX_SIZE = 128 * 1024

    attr_reader :repository
    attr_reader :oid
    attr_reader :name
    attr_reader :mode

    def initialize(repo, oid, name, mode = nil)
      @repository = repo
      @oid = oid
      @name = name
      @mode = mode
    end

    def git_attributes
      @git_attributes ||= repository.fetch_attributes(
        name, GIT_ATTR, GIT_ATTR_FLAGS)
    end

    def vendored?
      if attr = git_attributes['linguist-vendored']
        return boolean_attribute(attr)
      else
        return super
      end
    end

    def documentation?
      if attr = git_attributes['linguist-documentation']
        boolean_attribute(attr)
      else
        super
      end
    end

    def language
      return @language if defined?(@language)

      @language = if lang = git_attributes['linguist-language']
        Language.find_by_name(lang)
      else
        super
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

    protected

    # Returns true if the attribute is present and not the string "false".
    def boolean_attribute(attr)
      attr != "false"
    end

    def load_blob!
      @data, @size = Rugged::Blob.to_buffer(repository, oid, MAX_SIZE) if @data.nil?
    end
  end
end
