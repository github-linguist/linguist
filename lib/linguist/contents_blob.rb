require 'linguist/blob_helper'
require 'digest/md5'

module Linguist
  # A ContentsBlog is a wrapper around a String object to make it quack
  # like a Grit::Blob. It provides the basic interface: `name`,
  # `data`, and `size`.
  class ContentsBlob
    include BlobHelper

    # Public: Initialize a new ContentsBlob from a path
    #
    # contents - A String containing the text to be analyzed
    #
    # Returns a FileBlob.
    def initialize(contents)
      @name     = Digest::MD5.hexdigest(contents)
      @contents = contents
    end

    # Public: Fake Filename
    #
    # Returns a String
    attr_reader :name

    # Public: Read file permissions
    #
    # Returns a String like '100644'
    def mode
      "100644"
    end

    # Public: Read file contents.
    #
    # Returns a String.
    def data
      @contents
    end

    # Public: Get byte size
    #
    # Returns an Integer.
    def size
      @contents.bytesize
    end
  end
end
