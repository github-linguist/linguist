require 'linguist/language'

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
      Language.find_by_extension(extname)
    end

    def lexer
      if language
        language.lexer
      else
        'plain'
      end
    end

    def lexer_name
      if language
        language.name
      else
        'Text'
      end
    end

    def to_s
      @path.dup
    end
  end
end
