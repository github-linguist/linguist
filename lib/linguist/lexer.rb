require 'albino'
require 'yaml'

module Linguist
  class Lexer < Struct.new(:name, :aliases, :filenames, :mimetypes)
    @name_index  = {}
    @alias_index = {}

    def self.find_by_name(name)
      @name_index[name.downcase]
    end

    def self.find_by_alias(name)
      @alias_index[name]
    end

    def self.[](name)
      find_by_name(name) || find_by_alias(name)
    end

    def to_s
      aliases.first
    end

    def colorize(text)
      Albino.colorize(text, to_s)
    end

    def ==(other)
      eql?(other)
    end

    def eql?(other)
      equal?(other)
    end

    YAML.load_file(File.expand_path("../lexers.yml", __FILE__)).each do |lexer|
      @name_index[lexer.name.downcase] = lexer
      lexer.aliases.each do |name|
        @alias_index[name] = lexer
      end
    end
  end
end
