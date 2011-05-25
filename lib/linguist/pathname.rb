require 'linguist/language'
require 'linguist/mime'

module Linguist
  # Similar to ::Pathname, Linguist::Pathname wraps a path string and
  # provides helpful query methods. Its useful when you only have a
  # filename but not a blob and need to figure out the langauge of the file.
  class Pathname
    # Public: Initialize a Pathname
    #
    # path - A filename String. The file may or maybe actually exist.
    #
    # Returns a Pathname.
    def initialize(path)
      @path = path
    end

    # Public: Get the basename of the path
    #
    # Examples
    #
    #   Pathname.new('sub/dir/file.rb').basename
    #   # => 'file.rb'
    #
    # Returns a String.
    def basename
      File.basename(@path)
    end

    # Public: Get the extname of the path
    #
    # Examples
    #
    #   Pathname.new('.rb').extname
    #   # => '.rb'
    #
    #   Pathname.new('file.rb').extname
    #   # => '.rb'
    #
    #   Pathname.new('Rakefile').extname
    #   # => 'Rakefile'
    #
    # Returns a String.
    def extname
      if basename[0] == ?.
        basename
      elsif basename.include?('.')
        File.extname(basename)
      else
        basename
      end
    end

    # Public: Get the language of the path
    #
    # The path extension name is the only heuristic used to detect the
    # language name.
    #
    # Examples
    #
    #   Pathname.new('file.rb').language
    #   # => Language['Ruby']
    #
    # Returns a Langauge.
    def language
      Language.find_by_extension(extname) || Language['Text']
    end

    # Deprecated: Get the lexer of the path
    #
    # Returns a Lexer.
    def lexer
      language.lexer
    end

    # Public: Get the mime type
    #
    # Examples
    #
    #   Pathname.new('index.html').mime_type
    #   # => 'text/html'
    #
    # Returns a mime type String.
    def mime_type
      @mime_type ||= Mime.mime_for(extname)
    end

    # Public: Get the mime media type
    #
    # Examples
    #
    #   Pathname.new('index.html').media_type
    #   # => 'text'
    #
    # Returns a media type String.
    def media_type
      mime_type.split('/')[0]
    end

    # Public: Get the mime sub type
    #
    # Examples
    #
    #   Pathname.new('index.html').sub_type
    #   # => 'html'
    #
    # Returns a media type String.
    def sub_type
      mime_type.split('/')[1]
    end

    # Public: Get the Content-Type header
    #
    # This value is used when serving raw blobs.
    #
    # Examples
    #
    #   Pathname.new('file.txt').content_type
    #   # => 'text/plain; charset=utf-8'
    #
    # Returns a content type String.
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
