require 'linguist/blob_helper'

module Linguist
  # A Blob is a wrapper around the content of a file to make it quack
  # like a Grit::Blob. It provides the basic interface: `name`,
  # `data`, `path` and `size`.
  class Blob
    include BlobHelper

    # Public: Initialize a new Blob.
    #
    # path    - A path String (does not necessarily exists on the file system).
    # content - Content of the file.
    #
    # Returns a Blob.
    def initialize(path, content)
      @path = path
      @content = content
    end

    # Public: Filename
    #
    # Examples
    #
    #   Blob.new("/path/to/linguist/lib/linguist.rb", "").path
    #   # =>  "/path/to/linguist/lib/linguist.rb"
    #
    # Returns a String
    attr_reader :path

    # Public: File name
    #
    # Returns a String
    def name
      File.basename(@path)
    end

    # Public: File contents.
    #
    # Returns a String.
    def data
      @content
    end

    # Public: Get byte size
    #
    # Returns an Integer.
    def size
      @content.bytesize
    end

    # Public: Get file extension.
    #
    # Returns a String.
    def extension
      extensions.last || ""
    end

    # Public: Return an array of the file extensions
    #
    #     >> Linguist::Blob.new("app/views/things/index.html.erb").extensions
    #     => [".html.erb", ".erb"]
    #
    # Returns an Array
    def extensions
      _, *segments = name.downcase.split(".", -1)

      segments.map.with_index do |segment, index|
        "." + segments[index..-1].join(".")
      end
    end
  end
end
