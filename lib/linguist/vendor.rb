module Linguist
  # Keeps track of files that should not be analyzed by linguist.
  #
  # Consults the static "lib/linguist/vendor.yml" for some common patterns for
  # vendored files and also "linguist_vendor.yml" in the repo for repo-specific
  # ones.
  class Vendor
    def initialize(blobs)
      @blobs = blobs
    end

    # Public: Is the given blob in a vendored directory?
    #
    # Vendored files are ignored by language statistics.
    #
    # See "vendor.yml" for a list of vendored conventions that match
    # this pattern.
    #
    # If the blobs the object was initialized with contain a
    # "linguist_vendor.yml" file, that one as an additional "vendor.yml" file
    #
    # Return true or false
    def vendored?(blob)
      blob.name =~ vendored_regexp ? true : false
    end

    private

    def paths
      return @paths if @paths

      @paths = YAML.load_file(File.expand_path("../vendor.yml", __FILE__))

      repo_config = @blobs.find { |blob| blob.name == "linguist_vendor.yml" }
      if repo_config
        data = YAML.load(repo_config.data)
        @paths += data if data.is_a? Array
      end

      @paths
    end

    def vendored_regexp
      @vendored_regexp ||= Regexp.new(paths.join('|'))
    end
  end
end
