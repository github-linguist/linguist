require 'linguist/blob_helper'
require 'linguist/blob'

module Linguist
  # A FileBlob is a wrapper around a File object to make it quack
  # like a Grit::Blob. It provides the basic interface: `name`,
  # `data`, `path` and `size`.
  class FileBlob < Blob
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
      @mode ||= File.stat(@fullpath).mode.to_s(8)
    end

    def symlink?
      return @symlink if defined? @symlink
      @symlink = (File.symlink?(@fullpath) rescue false)
    end

    # Public: Read file contents.
    #
    # Returns a String.
    def data
      @data ||= File.read(@fullpath, :encoding => "ASCII-8BIT")
    end

    # Public: Get byte size
    #
    # Returns an Integer.
    def size
      @size ||= File.size(@fullpath)
    end
  end
end
