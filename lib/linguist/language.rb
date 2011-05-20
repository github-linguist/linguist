require 'linguist/lexer'

require 'yaml'

module Linguist
  class Language
    @name_index      = {}
    @lexer_index     = {}
    @extension_index = {}

    def self.create(attributes = {})
      language = new(attributes)

      @name_index[language.name.downcase] = language

      if attributes[:default_lexer] || language.default_lexer?
        @lexer_index[language.lexer_name.downcase] = language
      end

      @lexer_index[language.lexer_name.downcase] ||= language

      language.extensions.each do |extension|
        @extension_index[extension] = language
        @extension_index[extension.sub(/^./, '')] = language
      end

      language
    end

    def self.[](name)
      find_by_name(name) || find_by_lexer(name)
    end

    def self.find_by_name(name)
      @name_index[name.downcase]
    end

    def self.find_by_extension(extension)
      @extension_index[extension]
    end

    def self.find_by_lexer(lexer)
      @lexer_index[lexer.downcase] || self['Text']
    end

    def self.popular
      @name_index.values.select(&:popular?).sort_by { |lang| lang.name.downcase }
    end

    def self.unpopular
      @name_index.values.select(&:unpopular?).sort_by { |lang| lang.name.downcase }
    end

    def initialize(attributes = {})
      @name       = attributes[:name] || raise(ArgumentError, "missing name")
      @lexer_name = attributes[:lexer_name] || default_lexer_name
      @lexer      = Lexer.find_by_alias(@lexer_name)
      @extensions = attributes[:extensions] || []
      @popular    = attributes[:popular] || false
    end

    attr_reader :name, :lexer_name, :lexer, :extensions

    def default_lexer_name
      name.downcase.gsub(/\s/, '-')
    end

    def default_lexer?
      lexer_name == default_lexer_name
    end

    def popular?
      @popular
    end

    def unpopular?
      !popular?
    end

    def ==(other)
      eql?(other)
    end

    def eql?(other)
      equal?(other)
    end
  end

  popular = YAML.load_file(File.expand_path("../popular.yml", __FILE__))

  YAML.load_file(File.expand_path("../extensions.yml", __FILE__)).each do |name, options|
    Language.create(
      :name => name,
      :lexer_name => options[:lexer],
      :default_lexer => options[:default_lexer],
      :extensions => options[:ext],
      :popular => popular.include?(name)
    )
  end
end
