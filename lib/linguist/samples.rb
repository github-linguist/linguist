require 'set'
require 'yaml'
require 'linguist/md5'

module Linguist
  # Model for accessing classifier training data.
  module Samples
    # Path to samples root directory
    ROOT = File.expand_path("../../../samples", __FILE__)

    # Path for serialized samples db
    PATH = File.expand_path('../samples.yml', __FILE__)

    # Hash of serialized samples object
    if File.exist?(PATH)
      DATA = YAML.load_file(PATH)
    else
      DATA = nil
    end

    # Check if serialized db is out of sync from db directory.
    #
    # Returns Boolean.
    def self.outdated?
      MD5.hexdigest(DATA) != MD5.hexdigest(classifier.to_hash)
    end

    # Public: Iterate over each sample.
    #
    # &block - Yields Sample to block
    #
    # Returns nothing.
    def self.each(&block)
      Dir.entries(ROOT).each do |category|
        next if category == '.' || category == '..'

        # Skip text and binary for now
        # Possibly reconsider this later
        next if category == 'text' || category == 'binary'

        dirname = File.join(ROOT, category)
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
      classifier
    end
    
    # Public: Serialize samples data to YAML.
    #
    # data - Hash
    # io   - IO object to write to
    #
    # Returns nothing.
    def self.serialize_to_yaml(data, io)
      data = ""
      escape = lambda { |s| s.inspect.gsub(/\\#/, "\#") }

      data << "languages_total: #{data['languages_total']}\n"
      data << "tokens_total: #{data['tokens_total']}\n"

      data << "languages:\n"
      data['languages'].sort.each do |language, count|
        data << "  #{escape.call(language)}: #{count}\n"
      end

      data << "language_tokens:\n"
      data['language_tokens'].sort.each do |language, count|
        data << "  #{escape.call(language)}: #{count}\n"
      end

      data << "tokens:\n"
      data['tokens'].sort.each do |language, tokens|
        data << "  #{escape.call(language)}:\n"
        tokens.sort.each do |token, count|
          data << "    #{escape.call(token)}: #{count}\n"
        end
      end

      io.write data
      nil
    end
  end
end
