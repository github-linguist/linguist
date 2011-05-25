require 'linguist/blob_helper'

module Linguist
  class FileBlob
    include BlobHelper

    def initialize(path, name = path)
      @path, @name = path, name
    end

    attr_reader :name

    def data
      File.read(@path)
    end

    def size
      File.size(@path)
    end
  end
end
