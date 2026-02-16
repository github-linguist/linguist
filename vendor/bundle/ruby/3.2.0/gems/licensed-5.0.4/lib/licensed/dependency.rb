# frozen_string_literal: true
require "licensee"

module Licensed
  class Dependency < Licensee::Projects::FSProject
    LEGAL_FILES_PATTERN = /#{File::SEPARATOR}(AUTHORS|NOTICE|LEGAL)(?:\..*)?\z/i

    attr_reader :name
    attr_reader :version
    attr_reader :errors
    attr_reader :path
    attr_reader :additional_terms

    # Create a new project dependency
    #
    # name        - unique dependency name
    # version     - dependency version
    # path        - absolute file path to the dependency, to find license contents
    # search_root - (optional) the root location to search for dependency license contents
    # metadata    - (optional) additional dependency data to cache
    # errors      - (optional) errors encountered when evaluating dependency
    #
    # Returns a new dependency object.  Dependency metadata and license contents
    # are available if no errors are set on the dependency.
    def initialize(name:, version:, path:, search_root: nil, metadata: {}, errors: [])
      @name = name
      @version = version
      @metadata = metadata
      @errors = errors
      path = path.to_s
      @path = path
      @additional_terms = []

      # enforcing absolute paths makes life much easier when determining
      # an absolute file path in #notices
      if File.exist?(path) && !Pathname.new(path).absolute?
        # this is an internal error related to source implementation and
        # should be raised, not stored to be handled by reporters
        raise ArgumentError, "dependency path #{path} must be absolute"
      end

      super(path, search_root: search_root, detect_readme: true, detect_packages: true)
    end

    # Returns whether the dependency exists locally
    def exist?
      # some types of dependencies won't necessarily have a path that exists,
      # but they can still find license contents between the given path and
      # the search root
      # @root is defined
      File.exist?(path) || File.exist?(@root)
    end

    # Returns true if the dependency has any errors, false otherwise
    def errors?
      errors.any?
    end

    # Returns a record for this dependency including metadata and legal contents
    def record
      @record ||= DependencyRecord.new(
        metadata: license_metadata,
        licenses: license_contents,
        notices: notice_contents
      )
    end

    # Returns a string representing the dependencys license
    def license_key
      return "none" unless license
      license.key
    end

    # Returns the license text content from all matched sources
    # except the package file, which doesn't contain license text.
    def license_contents
      files = matched_files.reject { |f| f == package_file }
                           .group_by(&:content)
                           .map { |content, sources| { "sources" => license_content_sources(sources), "text" => content } }

      files << generated_license_contents if files.empty?
      files.compact
    end


    # Override the behavior of Licensee::Projects::FSProject#project_files to include
    # additional license terms
    def project_files
      super + additional_license_terms_files
    end

    # Returns legal notices found at the dependency path
    def notice_contents
      Dir.glob(dir_path.join("*"))
         .grep(LEGAL_FILES_PATTERN)
         .select { |path| File.file?(path) }
         .sort # sorted by the path
         .map { |path| { "sources" => normalize_source_path(path), "text" => read_file_with_encoding_check(path) } }
         .select { |notice| notice["text"].length > 0 } # files with content only
    end

    # Returns a hash of basic metadata about the dependency - name, version, type, etc
    def metadata
      {
        # can be overriden by values in @metadata
        "name" => name,
        "version" => version
      }.merge(
        @metadata
      )
    end

    private

    def read_file_with_encoding_check(file_path)
      File.read(file_path).encode("UTF-16", invalid: :replace, replace: "?").encode("UTF-8").rstrip
    end

    # Returns the sources for a group of license file contents
    #
    # Sources are returned as a single string with sources separated by ", "
    def license_content_sources(files)
      paths = Array(files).map do |file|
        next file[:uri] if file[:uri]
        next file[:source] if file[:source]

        path = dir_path.join(file[:dir], file[:name])
        normalize_source_path(path)
      end

      paths.join(", ")
    end

    def normalize_source_path(path)
      path = Pathname.new(path) unless path.is_a?(Pathname)
      if path.fnmatch?(dir_path.join("**").to_path)
        # files under the dependency path return the relative path to the file
        path.relative_path_from(dir_path).to_path
      else
        # otherwise return the source_path as the immediate parent folder name
        # joined with the file name
        path.dirname.basename.join(path.basename).to_path
      end
    end

    # Returns the metadata that represents this dependency.  This metadata
    # is written to YAML in the dependencys cached text file
    def license_metadata
      metadata.merge({
        # overrides all metadata values
        "license" => license_key
      })
    end

    # Returns a generated license content source and text for the dependency's
    # license if it exists and is not "other"
    def generated_license_contents
      return unless license
      return if license.key == "other"
      return if license.text.nil?

      # strip copyright clauses and any extra newlines
      # many package managers don't provide enough information to
      # autogenerate a copyright clause
      text = license.text.lines
                         .reject { |l| l =~ Licensee::Matchers::Copyright::REGEX }
                         .join
                         .gsub(/\n\n\n/, "\n\n")

      {
        "sources" => "Auto-generated #{license.spdx_id} license text",
        "text" => text
      }
    end

    # Returns an array of Licensee::ProjectFiles::LicenseFile created from
    # this dependency's additional license terms
    def additional_license_terms_files
      @additional_license_terms_files ||= begin
        files = additional_terms.map do |path|
          next unless File.file?(path)

          metadata = { dir: File.dirname(path), name: File.basename(path) }
          Licensee::ProjectFiles::LicenseFile.new(
            load_file(metadata),
            { source: "License terms loaded from #{metadata[:name]}" }
          )
        end
        files.compact
      end
    end
  end
end
