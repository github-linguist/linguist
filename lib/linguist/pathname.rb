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
      @mime_type ||= Mime.lookup(extname)
    end

    def to_s
      @path.dup
    end
  end
end
