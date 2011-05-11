require 'yaml'

module Linguist
  class Language
    @name_index      = {}
    @lexer_index     = {}
    @extension_index = {}

    def self.create(attributes = {})
      language = new(attributes)

      @name_index[language.name.downcase] = language

      if language.default_lexer? || !@lexer_index.key?(language.lexer)
        @lexer_index[language.lexer] = language
      end

      language.extensions.each do |extension|
        @extension_index[extension] = language
        @extension_index[extension.sub(/^./, '')] = language
      end

      language
    end

    def self.[](name)
      find_by_name(name)
    end

    def self.find_by_name(name)
      @name_index[name.downcase]
    end

    def self.find_by_extension(extension)
      @extension_index[extension]
    end

    def self.find_by_lexer(lexer)
      @lexer_index[lexer]
    end

    def self.lexers
      @lexer_index.to_a
    end

    def self.lexer2name(lexer)
      if language = find_by_lexer(lexer)
        language.name
      end
    end

    def self.name2lexer(name)
      if language = find_by_name(name)
        language.lexer
      end
    end

    def initialize(attributes = {})
      @name       = attributes[:name] || raise(ArgumentError, "missing name")
      @lexer      = attributes[:lexer] || default_lexer
      @extensions = attributes[:extensions] || []
    end

    attr_reader :name, :lexer, :extensions

    def default_lexer
      name.downcase.gsub(/\s/, '-')
    end

    def default_lexer?
      lexer == default_lexer
    end

    def ==(other)
      eql?(other)
    end

    def eql?(other)
      equal?(other)
    end
  end

  YAML.load_file(File.expand_path("../extensions.yml", __FILE__)).each do |name, options|
    Language.create(:name => name, :lexer => options[:lexer], :extensions => options[:ext])
  end
end
