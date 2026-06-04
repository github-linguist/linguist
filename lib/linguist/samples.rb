begin
  require 'yajl'
rescue LoadError
  require 'json'
end

require 'linguist/sha256'
require 'linguist/classifier'
require 'linguist/shebang'

module Linguist
  # Model for accessing classifier training data.
  module Samples
    # Path to samples root directory
    ROOT = File.expand_path("../../../samples", __FILE__)

    # Path for serialized samples db
    PATH = File.expand_path('../samples.json', __FILE__)

    # Hash of serialized samples object, cached in memory
    def self.cache
      @cache ||= load_samples
    end

    # Hash of serialized samples object, uncached
    def self.load_samples
      serializer = defined?(Yajl) ? Yajl : JSON
      data = serializer.load(File.read(PATH, encoding: 'utf-8'))
      # JSON serialization does not allow integer keys, we fix them here
      for lang in data['centroids'].keys
        fixed = data['centroids'][lang].to_a.map { |k,v| [k.to_i, v] }
        data['centroids'][lang] = Hash[fixed]
      end

      data
    end

    # Public: Iterate over each sample.
    #
    # &block - Yields Sample to block
    #
    # Returns nothing.
    def self.each(&block)
      Dir.entries(ROOT).sort!.each do |category|
        next if category == '.' || category == '..'

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
            path = File.join(dirname, filename)
            extname = File.extname(filename)

            yield({
              :path     => path,
              :language => category,
              :interpreter => Shebang.interpreter(File.read(path)),
              :extname  => extname.empty? ? nil : extname
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
      db = {}
      db['extnames'] = {}
      db['interpreters'] = {}
      db['filenames'] = {}

      each do |sample|
        language_name = sample[:language]

        if sample[:extname]
          db['extnames'][language_name] ||= []
          if !db['extnames'][language_name].include?(sample[:extname])
            db['extnames'][language_name] << sample[:extname]
            db['extnames'][language_name].sort!
          end
        end

        if sample[:interpreter]
          db['interpreters'][language_name] ||= []
          if !db['interpreters'][language_name].include?(sample[:interpreter])
            db['interpreters'][language_name] << sample[:interpreter]
            db['interpreters'][language_name].sort!
          end
        end

        if sample[:filename]
          db['filenames'][language_name] ||= []
          db['filenames'][language_name] << sample[:filename]
          db['filenames'][language_name].sort!
        end

        data = File.read(sample[:path])
        Classifier.train!(db, language_name, data)
      end

      Classifier.finalize_train! db
      db['sha256'] = Linguist::SHA256.hexdigest(db)

      db
    end
  end
end
