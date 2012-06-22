require 'linguist/classifier'
require 'linguist/language'

module Linguist
  # Model for accessing classifier training data.
  class Sample
    # Samples live in test/ for now, we'll eventually move them out
    PATH = File.expand_path("../../../samples", __FILE__)

    # Public: Iterate over each Sample.
    #
    # &block - Yields Sample to block
    #
    # Returns nothing.
    def self.each(&block)
      Dir.entries(PATH).each do |category|
        next if category == '.' || category == '..'

        # Skip text and binary for now
        # Possibly reconsider this later
        next if category == 'text' || category == 'binary'

        # Map directory name to a Language alias
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

    # Public: Build Classifier from all samples.
    #
    # Returns trained Classifier.
    def self.classifier
      classifier = Classifier.new
      each { |sample| classifier.train(sample.language, sample.data) }
      classifier.gc
    end

    # Internal: Initialize Sample.
    #
    # Samples should be initialized by Sample.each.
    #
    # path     - String full path to file.
    # language - Language of sample.
    def initialize(path, language)
      @path     = path
      @language = language
    end

    # Public: Get full path to file.
    #
    # Returns String.
    attr_reader :path

    # Public: Get sample language.
    #
    # Returns Language.
    attr_reader :language

    # Public: Read file contents.
    #
    # Returns String.
    def data
      File.read(path)
    end
  end
end
