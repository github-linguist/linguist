require 'linguist/language'
require 'linguist/mime'

module Linguist
  class Pathname
    def initialize(path)
      @path = path
    end

    def basename
      File.basename(@path)
    end

    def extname
      if basename[0] == ?.
        basename
      elsif basename.include?('.')
        File.extname(basename)
      else
        basename
      end
    end

    def language
      Language.find_by_extension(extname) || Language['Text']
    end

    def lexer
      language.lexer
    end

    def mime_type
      @mime_type ||= Mime.mime_for(extname)
    end

    def media_type
      mime_type.split('/')[0]
    end

    def sub_type
      mime_type.split('/')[1]
    end

    def content_type
      @content_type ||= Mime.content_type_for(extname)
    end

    def to_s
      @path.dup
    end

    def eql?(other)
      other.is_a?(self.class) && @path == other.to_s
    end
    alias_method :==, :eql?
  end
end
