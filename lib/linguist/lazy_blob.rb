require 'linguist/blob_helper'
require 'rugged'

module Linguist
  class LazyBlob
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
