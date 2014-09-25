require 'linguist/blob_helper'

module Linguist
  # A FileBlob is a wrapper around a File object to make it quack
  # like a Grit::Blob. It provides the basic interface: `name`,
  # `data`, and `size`.
  class FileBlob
    include BlobHelper

    # Public: Initialize a new FileBlob from a path
    #
    # path      - A path String that exists on the file system.
    # base_path - Optional base to relativize the path
    #
    # Returns a FileBlob.
    def initialize(path, base_path = nil)
      @path = path
      @name = base_path ? path.sub("#{base_path}/", '') : path
    end

    # Public: Filename
    #
    # Examples
    #
    #   FileBlob.new("/path/to/linguist/lib/linguist.rb").name
    #   # =>  "/path/to/linguist/lib/linguist.rb"
    #
    #   FileBlob.new("/path/to/linguist/lib/linguist.rb",
    #                "/path/to/linguist").name
    #   # =>  "lib/linguist.rb"
    #
    # Returns a String
    attr_reader :name

    # Public: Read file permissions
    #
    # Returns a String like '100644'
    def mode
      File.stat(@path).mode.to_s(8)
    end

    # Public: Read file contents.
    #
    # Returns a String.
    def data
      File.read(@path)
    end

    # Public: Get byte size
    #
    # Returns an Integer.
    def size
      File.size(@path)
    end

    # Public: Get file extension.
    #
    # Returns a String.
    def extension
      # File.extname returns nil if the filename is an extension.
      extension = File.extname(name)
      basename = File.basename(name)
      # Checks if the filename is an extension.
      if extension.empty? && basename[0] == "."
        basename
      else
        extension
      end
    end
  end
end
