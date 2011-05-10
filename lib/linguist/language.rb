require 'yaml'

module Linguist
  class Language
    @name_index = {}

    def self.create(name)
      language = new(name)

      @name_index[language.name] = language

      language
    end

    def self.find_by_name(name)
      @name_index[name]
    end

    def self.[](name)
      find_by_name(name)
    end

    attr_reader :name

    def initialize(name)
      @name = name
    end
  end

  YAML.load_file(File.expand_path("../extensions.yml", __FILE__)).each do |name, options|
    Language.create(name)
  end
end
