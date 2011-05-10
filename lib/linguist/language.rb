require 'yaml'

module Linguist
  class Language
    @name_index      = {}
    @extension_index = {}

    def self.create(attributes = {})
      language = new(attributes)

      @name_index[language.name.downcase] = language

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

    def self.find_by_filename(filename)
      basename = File.basename(filename)

      if basename[0] == ?.
        ext = basename
      elsif basename.include?('.')
        ext = File.extname(basename)
      else
        ext = basename
      end

      find_by_extension(ext)
    end

    attr_reader :name, :extensions

    def initialize(attributes = {})
      @name       = attributes[:name] || raise(ArgumentError, "missing name")
      @extensions = attributes[:extensions] || []
    end
  end

  YAML.load_file(File.expand_path("../extensions.yml", __FILE__)).each do |name, options|
    Language.create(:name => name, :extensions => options[:ext])
  end
end
