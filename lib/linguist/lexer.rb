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
      Albino.colorize(text, self)
    end

    def colorize_without_wrapper(text)
      if text = colorize(text)
        text[%r{<div class="highlight"><pre>(.*?)</pre>\s*</div>}m, 1]
      else
        ''
      end
    end

    def ==(other)
      eql?(other)
    end

    def eql?(other)
      equal?(other)
    end

    YAML.load_file(File.expand_path("../lexers.yml", __FILE__)).each do |lexer|
      if @name_index.key?(lexer.name.downcase)
        warn "Duplicate lexer name: #{lexer.name}"
      end

      @name_index[lexer.name.downcase] = lexer

      lexer.aliases.each do |name|
        if @alias_index.key?(name)
          warn "Duplicate alias: #{name}"
        end

        @alias_index[name] = lexer
      end
    end
  end
end
