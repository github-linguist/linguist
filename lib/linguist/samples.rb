begin
  require 'yajl'
rescue LoadError
  require 'yaml'
end

require 'linguist/md5'
require 'linguist/classifier'
require 'linguist/shebang'

module Linguist
  # Model for accessing classifier training data.
  module Samples
    # Path to samples root directory
    ROOT = File.expand_path("../../../samples", __FILE__)

    # Path for serialized samples db
    PATH = File.expand_path('../samples.json', __FILE__)

    # Hash of serialized samples object
    def self.cache
      @cache ||= begin
        serializer = defined?(Yajl) ? Yajl : YAML
        serializer.load(File.read(PATH, encoding: 'utf-8'))
      end
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
        data.encode!("UTF-8", "binary", :invalid => :replace, :undef => :replace, :replace => " ")
        data = data.force_encoding("UTF-8")
        Classifier.train!(db, language_name, data)
      end

      db['md5'] = Linguist::MD5.hexdigest(db)

      db
    end
  end
end
