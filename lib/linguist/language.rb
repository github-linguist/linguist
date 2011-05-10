require 'yaml'

module Linguist
  class Language
    @name_index = {}

    def self.create(attributes = {})
      language = new(attributes)

      @name_index[language.name.downcase] = language

      language
    end

    def self.find_by_name(name)
      @name_index[name.downcase]
    end

    def self.[](name)
      find_by_name(name)
    end

    attr_reader :name, :extensions

    def initialize(attributes = {})
      @name       = attributes[:name]
      @extensions = attributes[:extensions] || []
    end
  end

  YAML.load_file(File.expand_path("../extensions.yml", __FILE__)).each do |name, options|
    Language.create(:name => name, :extensions => options[:ext])
  end
end
