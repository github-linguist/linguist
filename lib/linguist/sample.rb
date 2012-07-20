require 'set'

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

          if filename == 'filenames'
            Dir.entries(File.join(dirname, filename)).each do |subfilename|
              next if subfilename == '.' || subfilename == '..'

              yield({
                :path    => File.join(dirname, filename, subfilename),
                :language => category,
                :filename => subfilename
              })
            end
          else
            yield({
              :path     => File.join(dirname, filename),
              :language => category,
              :extname  => File.extname(filename)
            })
          end
        end
      end

      nil
    end

    # Get all extensions listed in samples/
    #
    # Returns Hash of sample language keys with a Set of extension
    # Strings.
    def self.extensions
      extensions = {}
      each do |sample|
        # TODO: For now skip empty extnames
        next if sample[:extname].nil? || sample[:extname] == ""
        extensions[sample[:language]] ||= Set.new
        extensions[sample[:language]] << sample[:extname]
      end
      extensions
    end

    # Get all filenames listed in samples/
    #
    # Returns Hash of sample language keys with a Set of filename
    # Strings.
    def self.filenames
      filenames = {}
      each do |sample|
        # TODO: For now skip empty extnames
        next if sample[:filename].nil?
        filenames[sample[:language]] ||= Set.new
        filenames[sample[:language]] << sample[:filename]
      end
      filenames
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
