module Linguist
  # BlobHelper is a mixin for Blobish classes that respond to "name",
  # "data" and "size" such as Grit::Blob.
  VENDORED_PATHS = YAML.load_file(File.expand_path("../vendor.yml", __FILE__))
  VENDORED_REGEXP = Regexp.new(VENDORED_PATHS.join('|'))

  DOCUMENTATION_PATHS = YAML.load_file(File.expand_path("../documentation.yml", __FILE__))
  DOCUMENTATION_REGEXP = Regexp.new(DOCUMENTATION_PATHS.join('|'))

  class BlobCounter

    GIT_ATTR = ['linguist-documentation',
      'linguist-language',
      'linguist-vendored',
      'linguist-generated',
      'linguist-detectable']

    GIT_ATTR_OPTS = { :priority => [:index], :skip_system => true }
    GIT_ATTR_FLAGS = Rugged::Repository::Attributes.parse_opts(GIT_ATTR_OPTS)

    attr_reader :repository

    def initialize(repository)
      @repository = repository
    end

    def count
      counter = 0
      repository.index.each do | entry |
        return counter if counter >= 100000
        counter += 1 if countable?(entry[:path])
      end
      counter
    end

    def git_attributes(path)
      repository.fetch_attributes(path, nil, GIT_ATTR_FLAGS)
    end

    def countable?(path)
      git_attributes = git_attributes(path)

      !vendored?(git_attributes, path) &&
      !documentation?(git_attributes, path) &&
      !generated?(git_attributes, path)
      # TODO does it worth to check for detectable language?
      # language && ( defined?(detectable?) && !detectable?.nil? ?
      #   detectable? :
      #   DETECTABLE_TYPES.include?(language.type)
      # )
    end

    def documentation?(git_attributes, path)
      if not git_attributes['linguist-documentation'].nil?
        boolean_attribute(git_attributes['linguist-documentation'])
      end

      DOCUMENTATION_REGEXP.match?(path)
    end

    def generated?(git_attributes, path)
      if not git_attributes['linguist-generated'].nil?
        boolean_attribute(git_attributes['linguist-generated'])
      end

      #TODO isolate all generated file path rules
      false
    end

    def vendored?(git_attributes, path)
      if not git_attributes['linguist-vendored'].nil?
        boolean_attribute(git_attributes['linguist-vendored'])
      end

      VENDORED_REGEXP.match?(path)
    end

    private

    # Returns true if the attribute is present and not the string "false" and not the false boolean.
    def boolean_attribute(attribute)
      attribute != "false" && attribute != false
    end
  end
end