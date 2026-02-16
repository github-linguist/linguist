# frozen_string_literal: true
require "pathname/common_prefix"
require "licensed/sources/helpers/content_versioning"

module Licensed
  module Sources
    class Manifest < Source
      include Licensed::Sources::ContentVersioning

      def enabled?
        File.exist?(manifest_path) || generate_manifest?
      end

      def enumerate_dependencies
        Parallel.map(packages) do |package_name, sources|
          Licensed::Sources::Manifest::Dependency.new(
            name: package_name,
            version: contents_version(*sources),
            path: configured_license_path(package_name) || sources_license_path(sources),
            sources: sources,
            metadata: {
              "type"     => Manifest.type,
              "name"     => package_name
            }
          )
        end
      end

      # Returns the license path for a package specified in the configuration.
      def configured_license_path(package_name)
        license_path = config.dig("manifest", "licenses", package_name)
        return unless license_path

        license_path = config.root.join(license_path)
        return unless license_path.exist?
        license_path
      end

      # Returns the top-most directory that is common to all paths in `sources`
      def sources_license_path(sources)
        # if there is more than one source, try to find a directory common to
        # all sources
        if sources.size > 1
          common_prefix = Pathname.common_prefix(*sources).to_path

          # don't allow the workspace root to be used as common prefix
          # the project this is run for should be excluded from the manifest,
          # or ignored in the config.  any license in the root should be ignored.
          return common_prefix if common_prefix != config.root
        end

        # use the first (or only) sources directory to find license information
        source = sources.first
        return File.dirname(source) if File.file?(source)
        source
      end

      # Returns a map of package names -> array of full source paths found
      # in the app manifest
      def packages
        manifest.each_with_object({}) do |(src, package_name), hsh|
          next if src.nil? || src.empty?
          hsh[package_name] ||= []
          hsh[package_name] << File.absolute_path(src, config.root)
        end
      end

      # Returns parsed or generated manifest data for the app
      def manifest
        return generate_manifest if generate_manifest?

        case manifest_path.extname.downcase.delete "."
        when "json"
          JSON.parse(File.read(manifest_path))
        when "yml", "yaml"
          YAML.load_file(manifest_path)
        end
      end

      # Returns the manifest location for the app
      def manifest_path
        path = config.dig("manifest", "path")
        return config.root.join(path) if path

        config.cache_path.join("manifest.json")
      end

      # Returns whether a manifest should be generated automatically
      def generate_manifest?
        !File.exist?(manifest_path) && !config.dig("manifest", "dependencies").nil?
      end

      # Returns a manifest of files generated automatically based on patterns
      # set in the licensed configuration file
      def generate_manifest
        verify_configured_dependencies!
        configured_dependencies.each_with_object({}) do |(name, files), hsh|
          files.each { |f| hsh[f] = name }
        end
      end

      # Verify that the licensed configuration file is valid for the current project.
      # Raises errors for issues found with configuration
      def verify_configured_dependencies!
        # verify that dependencies are configured
        if configured_dependencies.empty?
          raise Source::Error.new("The manifest \"dependencies\" cannot be empty!")
        end

        # verify all included files match a single configured dependency
        errors = included_files.map do |file|
          matches = configured_dependencies.select { |name, files| files.include?(file) }
                                           .map { |name, files| name }
          case matches.size
          when 0
            "#{file} did not match a configured dependency"
          when 1
            nil
          else
            "#{file} matched multiple configured dependencies: #{matches.join(", ")}"
          end
        end

        errors.compact!
        raise Source::Error.new(errors.join($/)) if errors.any?
      end

      # Returns the project dependencies specified from the licensed configuration
      def configured_dependencies
        @configured_dependencies ||= begin
          dependencies = config.dig("manifest", "dependencies")&.dup || {}

          dependencies.each_with_object({}) do |(name, patterns), hsh|
            # map glob pattern(s) listed for the dependency to a listing
            # of files that match the patterns and are not excluded
            hsh[name] = files_from_pattern_list(patterns) & included_files
          end
        end
      end

      # Returns the set of project files that are included in dependency evaluation
      def included_files
        @included_files ||= tracked_files - files_from_pattern_list(config.dig("manifest", "exclude"))
      end

      # Finds and returns all files in the project that match
      # the glob pattern arguments.
      def files_from_pattern_list(patterns)
        return Set.new if patterns.nil? || patterns.empty?

        # evaluate all patterns from the project root
        Array(patterns).each_with_object(Set.new) do |pattern, files|
          if pattern.start_with?("!")
            # if the pattern is an exclusion, remove all matching files
            # from the result
            files.subtract(Dir.glob(pattern[1..-1], File::FNM_DOTMATCH, base: config.root))
          else
            # if the pattern is an inclusion, add all matching files
            # to the result
            files.merge(Dir.glob(pattern, File::FNM_DOTMATCH, base: config.root))
          end
        end
      end

      # Returns all tracked files in the project as the intersection of what git tracks and the files in the project
      def tracked_files
        @tracked_files ||= Set.new(Array(Licensed::Git.files)) &
                           Set.new(Dir.glob("**/*", File::FNM_DOTMATCH, base: config.root))
      end

      class Dependency < Licensed::Dependency
        ANY_EXCEPT_COMMENT_CLOSE_REGEX = /(\*(?!\/)|[^\*])*/m.freeze
        HEADER_LICENSE_REGEX = /
          (
            \/\*
            #{ANY_EXCEPT_COMMENT_CLOSE_REGEX}#{Licensee::Matchers::Copyright::COPYRIGHT_SYMBOLS}#{ANY_EXCEPT_COMMENT_CLOSE_REGEX}
            \*\/
          )
        /imx.freeze

        def initialize(name:, version:, path:, sources:, metadata: {})
          @sources = sources
          super(name: name, version: version, path: path, metadata: metadata)
        end

        def project_files
          files = super
          files.concat(source_files) if files.empty?
          files
        end

        # Returns an enumeration of Licensee::ProjectFiles::LicenseFile
        # representing licenses found in source header comments
        def source_files
          @source_files ||= begin
            @sources
              .select { |file| File.file?(file) }
              .flat_map { |file| source_file_comments(file) }
              .map do |comment, file|
                Licensee::ProjectFiles::LicenseFile.new(comment, file)
              end
          end
        end

        private

        # Returns all source header comments for a file
        def source_file_comments(file)
          file_parts = { dir: File.dirname(file), name: File.basename(file) }
          content = File.read(file)
          matches = content.scan(HEADER_LICENSE_REGEX)
          matches.map { |match| [source_comment_text(match[0]), file_parts] }
        end

        # Returns the comment text with leading * and whitespace stripped
        def source_comment_text(comment)
          indent = nil
          comment.lines.map do |line|
            # find the length of the indent as the number of characters
            # until the first word character
            indent ||= line[/\A([^\w]*)\w/, 1]&.size

            # insert newline for each line until a word character is found
            next "\n" unless indent

            line[/([^\w\r\n]{0,#{indent}})(.*)/m, 2]
          end.join
        end
      end
    end
  end
end
