# frozen_string_literal: true
require "json"

module Licensed
  module Sources
    class Composer < Source
      DEFAULT_COMPOSER_APPLICATON_PATH = "composer.phar"

      def enabled?
        return false unless Licensed::Shell.tool_available?("php")
        File.exist?(composer_lock) && File.exist?(composer_application_path)
      end

      def enumerate_dependencies
        packages.map do |package|
          Dependency.new(
            name: package["name"],
            version: package["version"],
            path: package_paths[package["name"]],
            metadata: {
              "type"     => Composer.type,
              "name"     => package["name"],
              "summary"  => package["description"],
              "homepage" => package["homepage"]
            }
          )
        end
      end

      def packages
        packages = JSON.parse(File.read(composer_lock))
        return packages["packages"] unless include_dev?

        packages["packages"] + packages["packages-dev"]
      end

      # Returns the output from running `php composer.phar` to get package metadata
      def package_paths
        return @package_paths if defined?(@package_paths)

        @package_paths = begin
          output = Licensed::Shell.execute("php", composer_application_path, "show", "--format", "json", "--path", allow_failure: true)
          return {} if output.to_s.empty?

          path_json = JSON.parse(output)
          return {} unless path_json["installed"]

          path_json["installed"].each_with_object({}) do |package, hsh|
            hsh[package["name"]] = package["path"]
          end
        end
      end

      def composer_application_path
        setting = config.dig("composer", "application_path") || DEFAULT_COMPOSER_APPLICATON_PATH
        File.expand_path(setting, config.pwd)
      end

      def composer_lock
        config.pwd.join("composer.lock")
      end

      # Returns whether to include dev packages based on the licensed configuration settings
      def include_dev?
        config.dig("composer", "include_dev") == true
      end
    end
  end
end
