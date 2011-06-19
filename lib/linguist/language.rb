require 'linguist/lexer'

require 'yaml'

module Linguist
  # Langage names that are recognizable by GitHub. Defined languages
  # can be highlighted, searched and listed under the Top Languages page.
  #
  # Langages are defined in `lib/linguist/langages.yml`.
  class Language
    @languages       = []
    @index           = {}
    @name_index      = {}
    @alias_index     = {}
    @extension_index = {}
    @filename_index  = {}

    # Internal: Create a new Language object
    #
    # attributes - A hash of attributes
    #
    # Returns a Language object
    def self.create(attributes = {})
      language = new(attributes)

      @languages << language

      # All Language names should be unique. Warn if there is a duplicate.
      if @name_index.key?(language.name)
        warn "Duplicate language name: #{language.name}"
      end

      # Language name index
      @index[language.name] = @name_index[language.name] = language

      language.aliases.each do |name|
        # All Language aliases should be unique. Warn if there is a duplicate.
        if @alias_index.key?(name)
          warn "Duplicate alias: #{name}"
        end

        @index[name] = @alias_index[name] = language
      end

      language.extensions.each do |extension|
        if extension !~ /^\./
          warn "Extension is missing a '.': #{extension.inspect}"
        end

        # All Language extensions should be unique. Warn if there is a
        # duplicate.
        if @extension_index.key?(extension)
          warn "Duplicate extension: #{extension}"
        end

        # Index the extension with a leading ".": ".rb"
        @extension_index[extension] = language

        # Index the extension without a leading ".": "rb"
        @extension_index[extension.sub(/^\./, '')] = language
      end

      language.filenames.each do |filename|
        @filename_index[filename] = language
      end

      language
    end

    # Public: Get all Languages
    #
    # Returns an Array of Languages
    def self.all
      @languages
    end

    # Public: Look up Language by its proper name.
    #
    # name - The String name of the Language
    #
    # Examples
    #
    #   Language.find_by_name('Ruby')
    #   # => #<Language name="Ruby">
    #
    # Returns the Language or nil if none was found.
    def self.find_by_name(name)
      @name_index[name]
    end

    # Public: Look up Language by one of its aliases.
    #
    # name - A String alias of the Language
    #
    # Examples
    #
    #   Language.find_by_alias('cpp')
    #   # => #<Language name="C++">
    #
    # Returns the Lexer or nil if none was found.
    def self.find_by_alias(name)
      @alias_index[name]
    end

    # Public: Look up Language by extension.
    #
    # extension - The extension String. May include leading "."
    #
    # Examples
    #
    #   Language.find_by_extension('.rb')
    #   # => #<Language name="Ruby">
    #
    # Returns the Language or nil if none was found.
    def self.find_by_extension(extension)
      @extension_index[extension]
    end

    # Public: Look up Language by filename.
    #
    # filename - The path String.
    #
    # Examples
    #
    #   Language.find_by_filename('foo.rb')
    #   # => #<Language name="Ruby">
    #
    # Returns the Language or nil if none was found.
    def self.find_by_filename(filename)
      basename, extname = File.basename(filename), File.extname(filename)
      @filename_index[basename] || @extension_index[extname]
    end

    # Public: Look up Language by its name or lexer.
    #
    # name - The String name of the Language
    #
    # TODO: Consider returning nil instead of Text
    #
    # Examples
    #
    #   Language['Ruby']
    #   # => #<Language name="Ruby">
    #
    #   Language['ruby']
    #   # => #<Language name="Ruby">
    #
    # Returns the Language or Text if none was found.
    def self.[](name)
      @index[name] || self['Text']
    end

    # Public: A List of popular languages
    #
    # Popular languages are sorted to the top of language chooser
    # dropdowns.
    #
    # This list is configured in "popular.yml".
    #
    # Returns an Array of Lexers.
    def self.popular
      @popular ||= all.select(&:popular?).sort_by { |lang| lang.name.downcase }
    end

    # Public: A List of non-popular languages
    #
    # Unpopular languages appear below popular ones in language
    # chooser dropdowns.
    #
    # This list is created from all the languages not listed in "popular.yml".
    #
    # Returns an Array of Lexers.
    def self.unpopular
      @unpopular ||= all.select(&:unpopular?).sort_by { |lang| lang.name.downcase }
    end

    # Internal: Initialize a new Language
    #
    # attributes - A hash of attributes
    def initialize(attributes = {})
      # @name is required
      @name = attributes[:name] || raise(ArgumentError, "missing name")

      # Set aliases
      @aliases = [default_alias_name] + (attributes[:aliases] || [])

      # Lookup Lexer object
      @lexer = Lexer.find_by_name(attributes[:lexer] || name) ||
        raise(ArgumentError, "#{@name} is missing lexer")

      # Set legacy search term
      @search_term = attributes[:search_term] || default_alias_name

      # Set extensions or default to [].
      @extensions = attributes[:extensions] || []
      @filenames  = attributes[:filenames]  || []

      # Set popular, common, and searchable flags
      @popular    = attributes.key?(:popular)    ? attributes[:popular]    : false
      @common     = attributes.key?(:common)     ? attributes[:common]     : false
      @searchable = attributes.key?(:searchable) ? attributes[:searchable] : true
    end

    # Public: Get proper name
    #
    # Examples
    #
    #   # => "Ruby"
    #   # => "Python"
    #   # => "Perl"
    #
    # Returns the name String
    attr_reader :name

    # Public: Get aliases
    #
    # Examples
    #
    #   Language['C++'].aliases
    #   # => ["cpp"]
    #
    # Returns an Array of String names
    attr_reader :aliases

    # Deprecated: Get code search term
    #
    # Examples
    #
    #   # => "ruby"
    #   # => "python"
    #   # => "perl"
    #
    # Returns the name String
    attr_reader :search_term

    # Public: Get Lexer
    #
    # Returns the Lexer
    attr_reader :lexer

    # Public: Get extensions
    #
    # Examples
    #
    #   # => ['.rb', '.rake', ...]
    #
    # Returns the extensions Array
    attr_reader :extensions

    # Public: Get filenames
    #
    # Examples
    #
    #   # => ['Rakefile', ...]
    #
    # Returns the extensions Array
    attr_reader :filenames

    # Internal: Get default alias name
    #
    # Returns the alias name String
    def default_alias_name
      name.downcase.gsub(/\s/, '-')
    end

    # Public: Is it popular?
    #
    # Returns true or false
    def popular?
      @popular
    end

    # Public: Is it not popular?
    #
    # Returns true or false
    def unpopular?
      !popular?
    end

    # Public: Is it common?
    #
    # Returns true or false
    def common?
      @common
    end

    # Public: Is it searchable?
    #
    # Unsearchable languages won't by indexed by solr and won't show
    # up in the code search dropdown.
    #
    # Returns true or false
    def searchable?
      @searchable
    end

    # Public: Highlight syntax of text
    #
    # text - String of code to be highlighted
    #
    # Returns html String
    def colorize(text)
      lexer.colorize(text)
    end

    # Public: Highlight syntax of text without the outer highlight div
    # wrapper.
    #
    # text - String of code to be highlighted
    #
    # Returns html String
    def colorize_without_wrapper(text)
      lexer.colorize_without_wrapper(text)
    end

    # Public: Return name as String representation
    def to_s
      name
    end

    def ==(other)
      eql?(other)
    end

    def eql?(other)
      equal?(other)
    end

    def hash
      name.hash
    end
  end

  popular = YAML.load_file(File.expand_path("../popular.yml", __FILE__))
  common  = YAML.load_file(File.expand_path("../common.yml", __FILE__))

  YAML.load_file(File.expand_path("../languages.yml", __FILE__)).each do |name, options|
    Language.create(
      :name        => name,
      :aliases     => options['aliases'],
      :lexer       => options['lexer'],
      :searchable  => options.key?('searchable') ? options['searchable'] : true,
      :search_term => options['search_term'],
      :extensions  => options['extensions'],
      :filenames   => options['filenames'],
      :popular     => popular.include?(name),
      :common      => common.include?(name)
    )
  end
end
