require 'linguist/blob_helper'

module Linguist
  class FileBlob
    include BlobHelper

    def initialize(path, base_path = nil)
      @path = path
      @name = base_path ? path.sub("#{base_path}/", '') : path
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
