# frozen_string_literal: true
require "json"

module Licensed
  module Sources
    class PNPM < Source
      # The PNPM source requires matching reviewed or ignored dependencies
      # on both name and version
      def self.require_matched_dependency_version
        true
      end

      # Returns true when pnpm is installed and a pnpm-lock.yaml file is found,
      # otherwise false
      def enabled?
        return false unless Licensed::Shell.tool_available?("pnpm")
        File.exist?(File.join(config.pwd, "pnpm-lock.yaml"))
      end

      def enumerate_dependencies
        packages.flat_map do |package|
          versions = package.key?("versions") ? package["versions"] : [package["version"]]
          paths = package.key?("paths") ? package["paths"] : [package["path"]]

          versions.zip(paths).map do |version, path|
            name_with_version = "#{package["name"]}@#{version}"
            Dependency.new(
              name: name_with_version,
              version: version,
              path: path,
              metadata: {
                "type"     => PNPM.type,
                "name"     => package["name"],
                "summary"  => package["description"],
                "homepage" => package["homepage"]
              }
            )
          end
        end
      end

      # Returns package metadata returned from `pnpm licensed list`
      def packages
        JSON.parse(package_metadata_command).values.flatten
      rescue JSON::ParserError => e
        message = "Licensed was unable to parse the output from 'pnpm licenses list'. JSON Error: #{e.message}"
        raise Licensed::Sources::Source::Error, message
      end

      # Returns the output from running `pnpm licenses list` to get package metadata
      def package_metadata_command
        args = %w(--json --long)
        args << "--prod" unless include_non_production?
        Licensed::Shell.execute("pnpm", "licenses", "list", *args, allow_failure: true)
      end

      # Returns whether to include non production dependencies based on the licensed configuration settings
      def include_non_production?
        config.dig("pnpm", "production_only") == false
      end
    end
  end
end
