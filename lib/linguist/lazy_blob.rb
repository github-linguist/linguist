require 'linguist/blob_helper'
require 'linguist/language'
require 'rugged'

module Linguist
  class LazyBlob
    GIT_ATTR = ['linguist-documentation',
                'linguist-language',
                'linguist-vendored',
                'linguist-generated']

    GIT_ATTR_OPTS = { :priority => [:index], :skip_system => true }
    GIT_ATTR_FLAGS = Rugged::Repository::Attributes.parse_opts(GIT_ATTR_OPTS)

    include BlobHelper

    MAX_SIZE = 128 * 1024

    attr_reader :repository
    attr_reader :oid
    attr_reader :path
    attr_reader :mode

    alias :name :path

    def initialize(repo, oid, path, mode = nil)
      @repository = repo
      @oid = oid
      @path = path
      @mode = mode
      @data = nil
    end

    def git_attributes
      @git_attributes ||= repository.fetch_attributes(
        name, GIT_ATTR, GIT_ATTR_FLAGS)
    end

    def documentation?
      if attr = git_attributes['linguist-documentation']
        boolean_attribute(attr)
      else
        super
      end
    end

    def generated?
      if attr = git_attributes['linguist-generated']
        boolean_attribute(attr)
      else
        super
      end
    end

    def vendored?
      if attr = git_attributes['linguist-vendored']
        return boolean_attribute(attr)
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

    def data
      load_blob!
      @data
    end

    def size
      load_blob!
      @size
    end

    def cleanup!
      @data.clear if @data
    end

    protected

    # Returns true if the attribute is present and not the string "false".
    def boolean_attribute(attribute)
      attribute != "false"
    end

    def load_blob!
      @data, @size = Rugged::Blob.to_buffer(repository, oid, MAX_SIZE) if @data.nil?
    end
  end
end
