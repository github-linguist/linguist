require 'linguist/mime'
require 'linguist/pathname'

require 'escape_utils'

module Linguist
  class Blob
    def initialize(blob)
      @blob = blob
      @name = Pathname.new(blob.name || "")
    end

    attr_reader :name

    def mime_type
      Mime.lookup(name.extname)
    end

    def disposition
      case mime_type
      when 'application/octet-stream', 'application/java-archive'
        "attachment; filename=#{EscapeUtils.escape_url(name.basename)}"
      else
        'inline'
      end
    end

    def data
      @blob.data
    end

    def lines
      @lines ||= data ? data.split("\n", -1) : []
    end

    def size
      @blob.size
    end

    def loc
      lines.size
    end

    def sloc
      lines.grep(/\S/).size
    end

    def submodule?
      defined?(Grit::Submodule) && @blob.kind_of?(Grit::Submodule)
    end

    def binary?
      mime_type == 'octet-stream' || !(text? || image?)
    end

    def file?
      image? || binary?
    end

    def text?
      return false if submodule?

      name.media_type == 'text' ||
        name.mime_type == 'application/json'
    end

    def image?
      ['.png', '.jpg', '.jpeg', '.gif'].include?(name.extname)
    end

    MEGABYTE = 1024 * 1024

    def large?
      size.to_i > MEGABYTE
    end

    def viewable?
      !file? && !large?
    end
  end
end
