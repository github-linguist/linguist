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

    # Public: Build Classifier from all samples.
    #
    # Returns trained Classifier.
    def self.data
      require 'linguist/classifier'
      require 'linguist/language'

      db = {}
      db['extnames'] = {}
      db['filenames'] = {}

      each do |sample|
        language = Language.find_by_alias(sample[:language])

        # TODO: For now skip empty extnames
        if sample[:extname] && sample[:extname] != ""
          db['extnames'][language.name] ||= []
          if !db['extnames'][language.name].include?(sample[:extname])
            db['extnames'][language.name] << sample[:extname]
          end
        end

        # TODO: For now skip empty extnames
        if fn = sample[:filename]
          db['filenames'][language.name] ||= []
          db['filenames'][language.name] << fn
        end

        data = File.read(sample[:path])
        Classifier.train!(db, language.name, data)
      end

      db['md5'] = Linguist::MD5.hexdigest(db)

      db
    end
  end
end
