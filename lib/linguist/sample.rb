require 'linguist/classifier'
require 'linguist/language'

module Linguist
  class Sample
    # Samples live in test/ for now, we'll eventually move them out
    PATH = File.expand_path("../../../test/fixtures", __FILE__)

    def self.each(&block)
      Dir.entries(PATH).each do |category|
        next if category == '.' || category == '..'

        # Skip text and binary for now
        next if category == 'text' || category == 'binary'

        language = Linguist::Language.find_by_alias(category)
        raise "No language for #{category.inspect}" unless language

        dirname = File.join(PATH, category)
        Dir.entries(dirname).each do |filename|
          next if filename == '.' || filename == '..'
          yield new(File.join(dirname, filename), language)
        end
      end

      nil
    end

    def self.classifier
      classifier = Classifier.new
      each { |sample| classifier.train(sample.language, sample.data) }
      classifier.gc
    end

    def initialize(path, language)
      @path     = path
      @language = language
    end

    def data
      File.read(path)
    end

    attr_reader :path, :language
  end
end
