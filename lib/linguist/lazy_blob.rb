require 'linguist/blob_helper'
require 'linguist/language'
require 'rugged'
require 'pry'
module Linguist
  class LazyBlob
    GIT_ATTR = ['linguist-language', 'linguist-vendored', 'linguist-generated']
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

    def linguist_vendored?
      if git_attributes['linguist-vendored']
        return result_for_key('linguist-vendored')
      else
        return vendored?
      end
    end

    def linguist_generated?
      if git_attributes['linguist-generated']
        return result_for_key('linguist-generated')
      else
        return generated?
      end
    end

    def result_for_key(keyname)
      key = git_attributes[keyname]
      if key == "false" || key.nil?
        return false
      else
        return true
      end
    end

    def overriden_language
      if lang = git_attributes['linguist-language']
        Language.find_by_name(lang)
      end
    end

    def language
      @language ||= (overriden_language || Language.detect(self))
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
    def load_blob!
      @data, @size = Rugged::Blob.to_buffer(repository, oid, MAX_SIZE) if @data.nil?
    end
  end
end
