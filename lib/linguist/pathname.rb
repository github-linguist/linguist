require 'linguist/language'
require 'linguist/mime'
require 'pygments'

module Linguist
  # Similar to ::Pathname, Linguist::Pathname wraps a path string and
  # provides helpful query methods. Its useful when you only have a
  # filename but not a blob and need to figure out the language of the file.
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
    # Returns a String.
    def extname
      File.extname(@path)
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
    # Returns a Language or nil if none was found.
    def language
      @language ||= Language.find_by_filename(@path)
    end

    # Internal: Get the lexer of the path
    #
    # Returns a Lexer.
    def lexer
      language ? language.lexer : Pygments::Lexer.find_by_name('Text only')
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

    # Public: Return self as String
    #
    # Returns a String
    def to_s
      @path.dup
    end

    def eql?(other)
      other.is_a?(self.class) && @path == other.to_s
    end
    alias_method :==, :eql?
  end
end
