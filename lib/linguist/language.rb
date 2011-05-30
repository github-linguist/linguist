require 'linguist/lexer'

require 'yaml'

module Linguist
  class Language
    @languages       = []
    @name_index      = {}
    @alias_index     = {}
    @lexer_index     = {}
    @extension_index = {}

    # Internal: Create a new Language object
    #
    # attributes - A hash of attributes
    #
    # Returns a Language object
    def self.create(attributes = {})
      language = new(attributes)

      @languages << language

      # All Language names should be unique. Warn if there is a duplicate.
      if @name_index.key?(language.name.downcase)
        warn "Duplicate language name: #{language.name}"
      end

      # Case-insensitive language name index
      @name_index[language.name.downcase] = language

      language.aliases.each do |name|
        # All Language aliases should be unique. Warn if there is a duplicate.
        if @alias_index.key?(name)
          warn "Duplicate alias: #{name}"
        end

        @alias_index[name] = language
      end


      # Set langauge as the default for reverse lexer lookup if
      # :default_lexer is set or the language is the same name as its
      # lexer.
      if attributes[:default_lexer] || language.default_lexer?
        @lexer_index[language.lexer_name.downcase] = language
      end

      # Case-insensitive lexer name index
      @lexer_index[language.lexer_name.downcase] ||= language

      language.extensions.each do |extension|
        # All Language extensions should be unique. Warn if there is a
        # duplicate.
        if @extension_index.key?(extension)
          warn "Duplicate extension: #{extension}"
        end

        # Index the extension with a leading ".": ".rb"
        @extension_index[extension] = language

        # Index the extension without a leading ".": "rb"
        @extension_index[extension.sub(/^./, '')] = language
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
    # name - The case-insensitive String name of the Language
    #
    # Examples
    #
    #   Language.find_by_name('Ruby')
    #   # => #<Language name="Ruby">
    #
    # Returns the Language or nil if none was found.
    def self.find_by_name(name)
      @name_index[name.downcase]
    end

    # Public: Look up Language by one of its aliases.
    #
    # name - A case-sensitive String alias of the Language
    #
    # Examples
    #
    #   Language.find_by_alias('cpp')
    #   # => #<Language name="C++">
    #
    # Returns the Lexer or nil if none was found.
    def self.find_by_alias(name)
      @alias_index[name.downcase]
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

    # Public: Look up Language by its name or lexer.
    #
    # name - The case-insensitive String name of the Language
    #
    # Examples
    #
    #   Language['Ruby']
    #   # => #<Language name="Ruby">
    #
    #   Language['ruby']
    #   # => #<Language name="Ruby">
    #
    # Returns the Language or nil if none was found.
    def self.[](name)
      find_by_name(name) || find_by_alias(name) || self['Text']
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

      # Use :lexer_name or fallback to `@name.downcase`
      @lexer_name = attributes[:lexer_name] || default_alias_name

      # Lookup Lexer object
      @lexer = Lexer.find_by_alias(@lexer_name)

      # Set extensions or default to [].
      # Consider using `@lexer.extensions`
      @extensions = attributes[:extensions] || []

      # Set popular or common flags
      @popular = attributes[:popular] || false
      @common  = attributes[:common] || false
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

    # Deprecated: Get lexer name
    #
    # Examples
    #
    #   # => "ruby"
    #   # => "python"
    #   # => "perl"
    #
    # Returns the name String
    attr_reader :lexer_name

    # Public: Get Lexer
    #
    # Returns the Lexer
    attr_reader :lexer

    # Public: Get extensions
    #
    # Examples
    #
    #   # => ['.rb', '.rake', 'Rakefile', ...]
    #
    # Returns the extensions Array
    attr_reader :extensions

    # Internal: Get default alias name
    #
    # Returns the alias name String
    def default_alias_name
      name.downcase.gsub(/\s/, '-')
    end

    # Internal: Is the language using the default lexer
    #
    # Returns true or false
    def default_lexer?
      lexer_name == default_alias_name
    end

    def search_term
      lexer_name
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
      :name    => name,
      :aliases => options[:aliases],
      :lexer_name => options[:lexer],
      :default_lexer => options[:default_lexer],
      :extensions => options[:ext],
      :popular    => popular.include?(name),
      :common     => common.include?(name)
    )
  end
end
