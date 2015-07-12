require 'linguist/blob_helper'
require 'linguist/memory_blob'

module Linguist
  # A FileBlob is a wrapper around a File object to make it quack
  # like a Grit::Blob. It provides the basic interface: `name`,
  # `data`, `path` and `size`.
  class FileBlob < MemoryBlob
    include BlobHelper

    # Public: Initialize a new FileBlob from a path
    #
    # path      - A path String that exists on the file system.
    # base_path - Optional base to relativize the path
    #
    # Returns a FileBlob.
    def initialize(path, base_path = nil)
      @fullpath = path
      @path = base_path ? path.sub("#{base_path}/", '') : path
    end

    # Public: Read file permissions
    #
    # Returns a String like '100644'
    def mode
      File.stat(@fullpath).mode.to_s(8)
    end

    # Public: Read file contents.
    #
    # Returns a String.
    def data
      File.read(@fullpath)
    end

    # Public: Get byte size
    #
    # Returns an Integer.
    def size
      File.size(@fullpath)
    end
  end
end
