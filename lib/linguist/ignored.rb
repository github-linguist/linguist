require 'yaml'

module Linguist
  class Ignored
    # Public: Is the blob  ignored
    #
    # name - String filename
    # data - String blob data. A block also maybe passed in for lazy
    #        loading.
    #
    # Return true or false
    def self.ignored?(name, data, base_path )
      new(name, data, base_path).ignored?
    end

    # Internal: Initialize Ignored instance
    #
    # name - String filename
    # data - String blob data
    def initialize(name, data, base_path )
      @name = name
      @_data = data
      @base_path = base_path

      @ignorefile = File.expand_path(".linguistignore.yml", base_path)

      if File.exists?(@ignorefile)
        ignored_paths = YAML.load_file(@ignorefile)
        @IgnoredRegexp = Regexp.new(ignored_paths.join('|')) if ignored_paths
      end
    end

    # Lazy load blob data if block was passed in.
    #
    # Returns String data.
    def data
      @data ||= @_data.respond_to?(:call) ? @_data.call() : @_data
    end

    attr_reader :name
    attr_reader :base_path


    # Internal: Is the blob an ignored file?
    #
    # Ignored files are ignored by language statistics.
    #
    # Return true or false
    def ignored?
      ignored_filename? 
    end

    # Internal: Does the filename indicate an ignore file?
    #
    # See ".linguistignore.yml" for a list of vendored conventions that match
    # this pattern.
    #
    # Return true or false
    def ignored_filename?
      name =~ @IgnoredRegexp ? true : false
    end

  end
end
