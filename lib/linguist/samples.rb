begin
  require 'json'
rescue LoadError
  require 'yaml'
end

require 'linguist/md5'
require 'linguist/classifier'

module Linguist
  # Model for accessing classifier training data.
  module Samples
    # Path to samples root directory
    ROOT = File.expand_path("../../../samples", __FILE__)

    # Path for serialized samples db
    PATH = File.expand_path('../samples.json', __FILE__)

    # Hash of serialized samples object
    if File.exist?(PATH)
      serializer = defined?(JSON) ? JSON : YAML
      DATA = serializer.load(File.read(PATH))
    end

    # Public: Iterate over each sample.
    #
    # &block - Yields Sample to block
    #
    # Returns nothing.
    def self.each(&block)
      Dir.entries(ROOT).sort!.each do |category|
        next if category == '.' || category == '..'

        # Skip text and binary for now
        # Possibly reconsider this later
        next if category == 'Text' || category == 'Binary'

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
            if File.extname(filename) == ""
              raise "#{File.join(dirname, filename)} is missing an extension, maybe it belongs in filenames/ subdir"
            end

            yield({
              :path     => File.join(dirname, filename),
              :language => category,
              :interpreter => File.exist?(filename) ? Linguist.interpreter_from_shebang(File.read(filename)) : nil,
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

      db['md5'] = Linguist::MD5.hexdigest(db)

      db
    end
  end

  # Used to retrieve the interpreter from the shebang line of a file's
  # data.
  def self.interpreter_from_shebang(data)
    lines = data.lines.to_a

    if lines.any? && (match = lines[0].match(/(.+)\n?/)) && (bang = match[0]) =~ /^#!/
      bang.sub!(/^#! /, '#!')
      tokens = bang.split(' ')
      pieces = tokens.first.split('/')

      if pieces.size > 1
        script = pieces.last
      else
        script = pieces.first.sub('#!', '')
      end

      script = script == 'env' ? tokens[1] : script

      # "python2.6" -> "python"
      if script =~ /((?:\d+\.?)+)/
        script.sub! $1, ''
      end

      # Check for multiline shebang hacks that call `exec`
      if script == 'sh' &&
        lines[0...5].any? { |l| l.match(/exec (\w+).+\$0.+\$@/) }
        script = $1
      end

      script
    else
      nil
    end
  end

end
