require 'linguist/blob_helper'

module Linguist
  # A FileBlob is a wrapper around a File object to make it quack
  # like a Grit::Blob. It provides the basic interface: `name`,
  # `data`, `path` and `size`.
  class FileBlob
    include BlobHelper

    # Public: Initialize a new FileBlob from a path
    #
    # path      - A path String that exists on the file system.
    # base_path - Optional base to relativize the path
    #
    # Returns a FileBlob.
    def initialize(path, base_path = nil)
      @path = base_path ? path.sub("#{base_path}/", '') : path
    end

    # Public: Filename
    #
    # Examples
    #
    #   FileBlob.new("/path/to/linguist/lib/linguist.rb").path
    #   # =>  "/path/to/linguist/lib/linguist.rb"
    #
    #   FileBlob.new("/path/to/linguist/lib/linguist.rb",
    #                "/path/to/linguist").path
    #   # =>  "lib/linguist.rb"
    #
    # Returns a String
    attr_reader :path

    # Public: Read file permissions
    #
    # Returns a String like '100644'
    def mode
      File.stat(@path).mode.to_s(8)
    end

    # Public: File name
    #
    # Returns a String
    def name
      File.basename(path)
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
      extensions.last || ""
    end

    # Public: Return an array of the file extensions
    #
    #     >> Linguist::FileBlob.new("app/views/things/index.html.erb").extensions
    #     => [".html.erb", ".erb"]
    #
    # Returns an Array
    def extensions
      basename, *segments = File.basename(path).split(".")

      segments.map.with_index do |segment, index|
        "." + segments[index..-1].join(".")
      end
    end
  end
end
