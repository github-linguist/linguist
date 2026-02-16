# frozen_string_literal: true
require "English"

module Licensed
  module Sources
    class Cabal < Source
      DEPENDENCY_REGEX = /\s*.+?\s*/.freeze
      DEFAULT_TARGETS = %w{executable library}.freeze

      def enabled?
        cabal_file_dependencies.any? && ghc?
      end

      def enumerate_dependencies
        packages.map do |package|
          path, search_root = package_docs_dirs(package)
          Dependency.new(
            name: package["name"],
            version: package["version"],
            path: path,
            search_root: search_root,
            errors: Array(package["error"]),
            metadata: {
              "type"     => Cabal.type,
              "summary"  => package["synopsis"],
              "homepage" => safe_homepage(package["homepage"])
            }
          )
        end
      end

      # Returns a list of all detected packages
      def packages
        package_ids = Set.new
        cabal_file_dependencies.each do |target|
          name = target.split(/\s/)[0]
          package_id = cabal_package_id(name)
          if package_id.nil?
            package_ids << target
          else
            recursive_dependencies([package_id], package_ids)
          end
        end

        Parallel.map(package_ids) { |id| package_info(id) }
      end

      # Returns the packages document directory and search root directory
      # as an array
      def package_docs_dirs(package)
        return [nil, nil] if package.nil? || package.empty?

        unless package["haddock-html"]
          # default to a local vendor directory if haddock-html property
          # isn't available
          return [File.join(config.pwd, "vendor", package["name"]), nil]
        end

        html_dir = package["haddock-html"]
        data_dir = package["data-dir"]
        return [html_dir, nil] unless data_dir

        # only allow data directories that are ancestors of the html directory
        unless Pathname.new(html_dir).fnmatch?(File.join(data_dir, "**"))
          data_dir = nil
        end

        [html_dir, data_dir]
      end

      # Returns a homepage url that enforces https and removes url fragments
      def safe_homepage(homepage)
        return unless homepage
        # Ensure there's no denial of service issue with a long homepage
        # 1000 characters is likely enough for any real project homepage
        # See https://github.com/github/licensed/security/code-scanning/1
        if homepage.length > 1000
          raise ArgumentError, "Input too long"
        end
        # use https and remove url fragment
        homepage.gsub(/http:/, "https:")
                .gsub(/#[^?]*\z/, "")
      end

      # Recursively finds the dependencies for each cabal package.
      # Returns a `Set` containing the package names for all dependencies
      def recursive_dependencies(package_names, results = Set.new)
        return results if package_names.nil? || package_names.empty?

        new_packages = Set.new(package_names) - results
        return results if new_packages.empty?

        results.merge new_packages

        dependencies = Parallel.map(new_packages, &method(:package_dependencies)).flatten

        recursive_dependencies(dependencies, results)
        results
      end

      # Returns an array of dependency package names for the cabal package
      # given by `id`
      def package_dependencies(id)
        package_dependencies_command(id).gsub("depends:", "").split.map(&:strip)
      end

      # Returns the output of running `ghc-pkg field depends` for a package id
      # Optionally allows for interpreting the given id as an
      # installed package id (`--ipid`)
      def package_dependencies_command(id)
        fields = %w(depends)
        ghc_pkg_field_command(id, fields, "--ipid")
      end

      # Returns package information as a hash for the given id
      def package_info(id)
        info = package_info_command(id).strip
        return missing_package(id) if info.empty?

        info.lines.each_with_object({}) do |line, hsh|
          key, value = line.split(":", 2).map(&:strip)
          next unless key && value

          hsh[key] = value
        end
      end

      # Returns the output of running `ghc-pkg field` to obtain package information
      def package_info_command(id)
        fields = %w(name version synopsis homepage haddock-html data-dir)
        ghc_pkg_field_command(id, fields, "--ipid")
      end

      # Runs a `ghc-pkg field` command for a given set of fields and arguments
      # Automatically includes ghc package DB locations in the command
      def ghc_pkg_field_command(id, fields, *args)
        Licensed::Shell.execute("ghc-pkg", "field", id, fields.join(","), *args, *package_db_args, allow_failure: true)
      end

      # Returns an array of ghc package DB locations as specified in the app
      # configuration
      def package_db_args
        @package_db_args ||= Array(config.dig("cabal", "ghc_package_db")).map do |path|
          next "--#{path}" if %w(global user).include?(path)
          path = realized_ghc_package_path(path)
          path = File.expand_path(path, config.root)

          next unless File.exist?(path)
          "--package-db=#{path}"
        end.compact
      end

      # Returns a ghc package path with template markers replaced by live
      # data
      def realized_ghc_package_path(path)
        path.gsub("<ghc_version>", ghc_version)
      end

      # Returns a set of the top-level dependencies found in cabal files
      def cabal_file_dependencies
        @cabal_file_dependencies ||= cabal_files.each_with_object(Set.new) do |cabal_file, targets|
          content = File.read(cabal_file)
          next if content.nil? || content.empty?

          # add any dependencies for matched targets from the cabal file.
          # by default this will find executable and library dependencies
          content.scan(cabal_file_regex).each do |match|
            # match[1] is a string of "," separated dependencies.
            # dependency packages might have a version specifier, remove them
            # to get the full id specifier for each package
            dependencies = match[1].split(",").map(&:strip)
            targets.merge(dependencies)
          end
        end
      end

      # Returns an installed package id for the package.
      def cabal_package_id(package_name)
        # using the first returned id assumes that package resolvers
        # order returned package information in the same order that it would
        # be used during build
        field = ghc_pkg_field_command(package_name, ["id"]).lines.first
        return unless field

        id = field.split(":", 2)[1]
        id.strip if id
      end

      # Find `build-depends` lists from specified targets in a cabal file
      def cabal_file_regex
        # this will match 0 or more occurences of
        # match[0] - specifier, e.g. executable, library, etc
        # match[1] - full list of matched dependencies
        # match[2] - first matched dependency (required)
        # match[3] - remainder of matched dependencies (not required)
        @cabal_file_regex ||= /
          # match a specifier, e.g. library or executable
          ^(#{cabal_file_targets.join("|")})
            .*? # stuff

            # match a list of 1 or more dependencies
            build-depends:(#{DEPENDENCY_REGEX}(,#{DEPENDENCY_REGEX})*)\n
        /xmi
      end

      # Returns the targets to search for `build-depends` in a cabal file
      def cabal_file_targets
        targets = Array(config.dig("cabal", "cabal_file_targets"))
        targets.push(*DEFAULT_TARGETS) if targets.empty?
        targets
      end

      # Returns an array of the local directory cabal package files
      def cabal_files
        @cabal_files ||= Dir.glob(File.join(config.pwd, "*.cabal"))
      end

      # Returns the ghc cli tool version
      def ghc_version
        return unless ghc?
        @version ||= Licensed::Shell.execute("ghc", "--numeric-version")
      end

      # Returns whether the ghc cli tool is available
      def ghc?
        @ghc ||= Licensed::Shell.tool_available?("ghc")
      end

      # Returns a package info structure with an error set
      def missing_package(id)
        name, version = package_id_name_version(id)
        { "name" => name, "version" => version, "error" => "package not found" }
      end

      # Parses the name and version pieces from an id or package requirement string
      def package_id_name_version(id)
        name, version = id.split(" ", 2)
        return [name, version] if version

        # split by dashes, find the rightmost thing that looks like an
        parts = id.split("-")
        version_start_index = parts.rindex { |part| part.match?(/^[\d\.]+$/) }
        return [id, nil] if version_start_index.nil?

        [
          parts[0...version_start_index].join("-"),
          parts[version_start_index..-1].join("-")
        ]
      end
    end
  end
end
