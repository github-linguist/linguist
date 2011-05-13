require 'linguist/blob_helper'

module Linguist
  class Blob
    include BlobHelper

    def initialize(blob)
      @blob = blob
    end

    def name
      @blob.name
    end

    def data
      @blob.data
    end

    def size
      @blob.size
    end
  end
end
