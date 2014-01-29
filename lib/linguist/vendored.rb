require 'yaml'

module Linguist
  class Vendored
    # Public: Is the blob a vendor file?
    #
    # name - String filename
    # data - String blob data. A block also maybe passed in for lazy
    #        loading.
    #
    # Return true or false
    def self.vendored?(name, data)
      new(name, data).vendored?
    end

    # Internal: Initialize Vendored instance
    #
    # name - String filename
    # data - String blob data
    def initialize(name, data)
      @name = name
      @_data = data
    end

    # Lazy load blob data if block was passed in.
    #
    # Returns String data.
    def data
      @data ||= @_data.respond_to?(:call) ? @_data.call() : @_data
    end

    attr_reader :name

    vendored_paths = YAML.load_file(File.expand_path("../vendor.yml", __FILE__))
    VendoredRegexp = Regexp.new(vendored_paths.join('|'))
    vendored_complex = YAML.load_file(File.expand_path("../vendor_complex.yml", __FILE__))
    VendoredComplex = vendored_complex.map{ |item|
      item['name'] = Regexp.new(item['name']) if item.include?('name')
      item
    }

    # Internal: Is the blob a vendored file?
    #
    # Vendored files are ignored by language statistics.
    #
    # Return true or false
    def vendored?
      vendored_filename? ||
        vendored_file?
    end

    # Internal: Does the filename indicate a vendored file?
    #
    # See "vendor.yml" for a list of vendored conventions that match
    # this pattern.
    #
    # Return true or false
    def vendored_filename?
      name =~ VendoredRegexp ? true : false
    end

    # Internal: Does the blob a vendored file?
    #
    # This method runs a more complex checking which examines not only
    # the filename but also the content of a file.
    #
    # See "vendor_complex.yml" for a list of vendored conventions that
    # match this pattern.
    def vendored_file?
      VendoredComplex.any? { |item|
        (!item.include?('name') || name =~ item['name']) &&
          (!item.include?('contain') || data[0..1024].include?(item['contain']))
      }
    end
  end
end
