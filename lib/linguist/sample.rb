module Linguist
  # Model for accessing classifier training data.
  module Sample
    # Samples live in test/ for now, we'll eventually move them out
    PATH = File.expand_path("../../../samples", __FILE__)

    # Public: Iterate over each sample.
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

        dirname = File.join(PATH, category)
        Dir.entries(dirname).each do |filename|
          next if filename == '.' || filename == '..'
          yield({ :path => File.join(dirname, filename), :language => category })
        end
      end

      nil
    end

    # Public: Build Classifier from all samples.
    #
    # Returns trained Classifier.
    def self.classifier
      require 'linguist/classifier'
      require 'linguist/language'

      classifier = Classifier.new
      each { |sample|
        language = Language.find_by_alias(sample[:language])
        data     = File.read(sample[:path])
        classifier.train(language.name, data)
      }
      classifier.gc
    end
  end
end
