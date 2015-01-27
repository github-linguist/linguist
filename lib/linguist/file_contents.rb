require 'linguist/blob_helper'

module Linguist
  # A FileContents mimics Linguist::FileBlob to provide language detection
  # based on initialized raw file contents and file name.
  class FileContents
    include BlobHelper

    # Public: Initialize a new FileContents from a file name and its contents.
    #
    # filename  - A String name of the file.
    # contents  - A String contents of the file.
    #
    # Returns a FileContents.
    def initialize(filename, contents)
      @name = filename
      @data = contents
    end

    attr_reader :name
    attr_reader :data

    def mode
      raise NotImplementedError
    end

    def size
      raise NotImplementedError
    end

    # Public: Get file extension.
    #
    # Returns a String.
    def extension
      extensions.last || ""
    end

    # Public: Return an array of the file extensions
    #
    #     >> Linguist::FileContents.new("index.html.erb").extensions
    #     => [".html.erb", ".erb"]
    #
    # Returns an Array
    def extensions
      basename, *segments = File.basename(name).split(".")

      segments.map.with_index do |segment, index|
        "." + segments[index..-1].join(".")
      end
    end
  end
end
