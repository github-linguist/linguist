require 'albino'
require 'yaml'

module Linguist
  # Mirror of Pygments Lexer structure.
  #
  # name      - Proper lexer name (JavaScript, Ruby, Python)
  # aliases   - Aliases for lookup (js, javascript)
  # filenames - Filename globs (*.js)
  # mimetypes - Mime types (application/javascript)
  class Lexer < Struct.new(:name, :aliases, :filenames, :mimetypes)
    @lexers          = []
    @index           = {}
    @name_index      = {}
    @alias_index     = {}
    @mimetypes_index = {}

    # Internal: Create a new Lexer object
    #
    # name  - Name of Lexer
    # attrs - A hash of attributes
    #
    # Returns a Lexer object
    def self.create(name, attrs)
      name      = name
      aliases   = attrs['aliases']   || []
      filenames = attrs['filenames'] || []
      mimetypes = attrs['mimetypes'] || []

      @lexers << lexer = new(name, aliases, filenames, mimetypes)

      # All Lexer names should be unique. Warn if there is a duplicate.
      if @name_index.key?(lexer.name)
        warn "Duplicate lexer name: #{lexer.name}"
      end

      @index[lexer.name] = @name_index[lexer.name] = lexer

      lexer.aliases.each do |name|
        # All Lexer aliases should be unique. Warn if there is a duplicate.
        if @alias_index.key?(name)
          warn "Duplicate alias: #{name}"
        end

        @index[name] = @alias_index[name] = lexer
      end

      lexer.mimetypes.each do |type|
        # All Lexer mimetypes should be unique. Warn if there is a duplicate.
        if @mimetypes_index.key?(name)
          warn "Duplicate mimetype: #{name}"
        end

        @mimetypes_index[type] = lexer
      end
    end

    # Internal: Test if system has Pygments
    #
    # Only used in tests to disable tests that require Pygments.
    #
    # Returns true if `pygmentize` in is PATH otherwise false.
    def self.has_pygments?
      `which #{Albino.bin}`
      $?.success?
    end

    # Public: Get all Lexers
    #
    # Returns an Array of Lexers
    def self.all
      @lexers
    end

    # Public: Look up Lexer by name or alias.
    #
    # name - A String name or alias
    #
    #   Lexer['Ruby']
    #   => #<Lexer name="Ruby">
    #
    # Returns the Lexer or nil if none was found.
    def self.[](name)
      @index[name]
    end

    # Public: Look up Lexer by its proper name.
    #
    # name - The String name of the Lexer
    #
    # Examples
    #
    #   Lexer.find_by_name('Ruby')
    #   # => #<Lexer name="Ruby">
    #
    # Returns the Lexer or nil if none was found.
    def self.find_by_name(name)
      @name_index[name]
    end

    # Public: Look up Lexer by one of its aliases.
    #
    # name - A String alias of the Lexer
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

    # Public: Look up Lexer by one of it's mime types.
    #
    # type - A mime type String.
    #
    # Examples
    #
    #  Lexer.find_by_mimetype('application/x-ruby')
    #  # => #<Lexer name="Ruby">
    #
    # Returns the Lexer or nil if none was found.
    def self.find_by_mimetype(type)
      @mimetypes_index[type]
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
      Albino.new(text, self).colorize(:O => 'stripnl=false')
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
    YAML.load_file(File.expand_path("../lexers.yml", __FILE__)).each do |name, attrs|
      Lexer.create(name, attrs)
    end
  end
end
