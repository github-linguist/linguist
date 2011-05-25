require 'albino'
require 'yaml'

module Linguist
  # Mirror of Pygments Lexer structure.
  class Lexer < Struct.new(:name, :aliases, :filenames, :mimetypes)
    @name_index  = {}
    @alias_index = {}

    # Public: Look up Lexer by its proper name.
    #
    # name - The case-insensitive String name of the Lexer
    #
    # Examples
    #
    #   Lexer.find_by_name('Ruby')
    #   # => #<Lexer name="Ruby">
    #
    # Returns the Lexer or nil if none was found.
    def self.find_by_name(name)
      @name_index[name.downcase]
    end

    # Public: Look up Lexer by one of its aliases.
    #
    # name - A case-sensitive String alias of the Lexer
    #
    # Examples
    #
    #   Lexer.find_by_alias('rb')
    #   # => #<Lexer name="Ruby">
    #
    # Returns the Lexer or nil if none was found.
    def self.find_by_alias(name)
      @alias_index[name]
    end

    # Public: Look up Lexer by name or alias.
    #
    # name - A case-insensitive String name or alias
    #
    #   Lexer['Ruby']
    #   => #<Lexer name="Ruby">
    #
    # Returns the Lexer or nil if none was found.
    def self.[](name)
      find_by_name(name) || find_by_alias(name)
    end

    # Public: Return a alias of the Lexer to pass to Pygments.
    #
    # The alias we choose is arbitrary.
    #
    # Returns the alias String
    def to_s
      aliases.first
    end

    # Public: Highlight syntax of text
    #
    # text - String of code to be highlighted
    #
    # Returns html String
    def colorize(text)
      Albino.colorize(text, self)
    end

    # Public: Highlight syntax of text without the outer highlight div
    # wrapper.
    #
    # text - String of code to be highlighted
    #
    # Returns html String
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

    # Load lexers from lexers.yml
    #
    # `bin/pygments-lexers` dumps a YAML list of all the available
    # Pygments lexers.
    YAML.load_file(File.expand_path("../lexers.yml", __FILE__)).each do |lexer|
      # All Lexer names should be unique. Warn if there is a duplicate.
      if @name_index.key?(lexer.name.downcase)
        warn "Duplicate lexer name: #{lexer.name}"
      end

      @name_index[lexer.name.downcase] = lexer

      lexer.aliases.each do |name|
        # All Lexer aliases should be unique. Warn if there is a duplicate.
        if @alias_index.key?(name)
          warn "Duplicate alias: #{name}"
        end

        @alias_index[name] = lexer
      end
    end
  end
end
