# frozen_string_literal: true
require "json"

module Licensed
  module Sources
    class Yarn::Berry < Source
      include Licensed::Sources::Yarn

      def self.version_requirement
        Gem::Requirement.new(">= 2.0")
      end

      def enumerate_dependencies
        packages.map do |name, package|
          Dependency.new(
            name: name,
            version: package["version"],
            path: package["path"],
            metadata: {
              "type"     => self.class.type,
              "name"     => package["name"],
              "homepage" => package["homepage"]
            }
          )
        end
      end

      # Finds packages that the current project relies on based on the output from `yarn info`
      def packages
        # parse all lines of output to json and find one that is "type": "tree"
        yarn_info = JSON.parse("[#{yarn_info_command.lines.join(",")}]")
        mapped_packages = yarn_info.reduce({}) do |accum, package|
          name, _ = package["value"].rpartition("@")
          version = package.dig("children", "Version")
          id = "#{name}@#{version}"

          accum[name] ||= []
          accum[name] << {
            "id" => id,
            "name" => name,
            "version" => version,
            "homepage" => package.dig("children", "Manifest", "Homepage"),
            "path" => dependency_paths[id]
          }
          accum
        end

        mapped_packages.each_with_object({}) do |(name, results), hsh|
          results.uniq! { |package| package["version"] }
          if results.size == 1
            # if there is only one package for a name, reference it by name
            hsh[name] = results[0]
          else
            # if there is more than one package for a name, reference each by id
            results.each do |package|
              hsh[package["id"]] = package
            end
          end
        end
      end

      # Returns the output from running `yarn list` to get project dependencies
      def yarn_info_command
        args = %w(--json --manifest --recursive --all)
        Licensed::Shell.execute("yarn", "info", *args)
      end
    end
  end
end
