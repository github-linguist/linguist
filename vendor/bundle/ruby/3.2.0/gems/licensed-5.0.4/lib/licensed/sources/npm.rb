# frozen_string_literal: true
require "json"

module Licensed
  module Sources
    class NPM < Source
      class Dependency < ::Licensed::Dependency
        # override license_metadata to pull homepage and summary information
        # from a packages package.json file, if it exists
        # this accounts for the lack of this information in npm 7's `npm list` output
        def license_metadata
          data = super
          return data if !data["homepage"].to_s.empty? && !data["summary"].to_s.empty?

          package_json_path = File.join(path, "package.json")
          return data unless File.exist?(package_json_path)

          package_json = JSON.parse(File.read(package_json_path))
          data["homepage"] = package_json["homepage"]
          data["summary"] = package_json["description"]

          data
        end
      end

      def enabled?
        Licensed::Shell.tool_available?("npm") && File.exist?(package_json_path)
      end

      def enumerate_dependencies
        packages.map do |name, package|
          next if package["name"] == project_name

          errors = package["problems"] unless package["path"]
          Dependency.new(
            name: name,
            version: package["version"] || package["required"],
            path: package["path"],
            errors: Array(errors),
            metadata: {
              "type"     => NPM.type,
              "name"     => package["name"],
              "summary"  => package["description"],
              "homepage" => package["homepage"]
            }
          )
        end
      end

      def packages
        root_dependencies = package_metadata["dependencies"] || {}
        recursive_dependencies(root_dependencies).each_with_object({}) do |(name, results), hsh|
          results.uniq! { |package| package["version"] }
          if results.size == 1
            hsh[name] = results[0]
          else
            results.each do |package|
              name_with_version = "#{name}-#{package["version"]}"
              hsh[name_with_version] = package
            end
          end
        end
      end

      # Recursively parse dependency JSON data.  Returns a hash mapping the
      # package name to it's metadata
      def recursive_dependencies(dependencies, result = {}, parent = nil)
        dependencies.each do |name, dependency|
          next if missing_peer?(parent, dependency, name)
          next if yarn_lock_present && dependency["missing"]
          next if dependency["extraneous"] && dependency["missing"]

          dependency["name"] = name
          dependency["version"] ||= extract_version(parent, name) if dependency["missing"]

          (result[name] ||= []) << dependency
          recursive_dependencies(dependency["dependencies"] || {}, result, dependency)
        end
        result
      end

      # Returns parsed package metadata returned from `npm list`
      def package_metadata
        return @package_metadata if defined?(@package_metadata)
        @package_metadata = JSON.parse(package_metadata_command)
      rescue JSON::ParserError => e
        message = "Licensed was unable to parse the output from 'npm list'. JSON Error: #{e.message}"
        npm_error = package_metadata_error
        message = "#{message}. npm Error: #{npm_error}" if npm_error
        raise Licensed::Sources::Source::Error, message
      end

      # Returns an error, if one exists, from running `npm list` to get package metadata
      def package_metadata_error
        Licensed::Shell.execute("npm", "list", *package_metadata_args)
        return ""
      rescue Licensed::Shell::Error => e
        return e.message
      end

      # Returns the output from running `npm list` to get package metadata
      def package_metadata_command
        args = %w(--json --long)
        args.concat(package_metadata_args)

        Licensed::Shell.execute("npm", "list", *args, allow_failure: true)
      end

      # Returns an array of arguments that should be used for all `npm list`
      # calls, regardless of how the output is formatted
      def package_metadata_args
        args = []
        args << "--production" unless include_non_production?

        # on npm 7+, the --all argument is necessary to evaluate the project's
        # full dependency tree
        args << "--all" if npm_version >= Gem::Version.new("7.0.0")

        return args
      end

      # Returns the currently installed version of npm as a Gem::Version object
      def npm_version
        @npm_version ||= begin
          Gem::Version.new(Licensed::Shell.execute("npm", "-v").strip)
        end
      end

      # Returns true if a yarn.lock file exists in the current directory
      def yarn_lock_present
        @yarn_lock_present ||= File.exist?(config.pwd.join("yarn.lock"))
      end

      # Returns whether to include non production dependencies based on the licensed configuration settings
      def include_non_production?
        config.dig("npm", "production_only") == false
      end

      def missing_peer?(parent, dependency, name)
        # return true if dependency is marked as "peerMissing"
        return true if dependency["peerMissing"]

        # return false unless the parent has registered the dependency
        # as a peer
        return false unless peer_dependency(parent, name)
        # return true if the dependency itself is marked as missing
        return true if dependency["missing"]
        dependency.empty? && parent&.dig("peerDependenciesMeta", name, "optional")
      end

      def peer_dependency(parent, name)
        return unless parent.is_a?(Hash)

        peerDependencies = parent["peerDependencies"]
        # "peerDependencies" could be set to the string "[Circular]"
        return unless peerDependencies.is_a?(Hash)

        peerDependencies[name]
      end

      def extract_version(parent, name)
        parent&.dig("_dependencies", name) || peer_dependency(parent, name)
      end

      # Returns the current projects name
      def project_name
        return unless package_json
        package_json["name"]
      end

      ## Returns the parse package.json for the current project
      def package_json
        return unless File.exist?(package_json_path)

        @package_json ||= JSON.parse(File.read(package_json_path))
      rescue JSON::ParserError => e
        message = "Licensed was unable to parse package.json. JSON Error: #{e.message}"
        raise Licensed::Sources::Source::Error, message
      end

      def package_json_path
        @package_json_path ||= File.join(config.pwd, "package.json")
      end
    end
  end
end
