# frozen_string_literal: true
require "json"

module Licensed
  module Sources
    class Yarn::V1 < Source
      include Licensed::Sources::Yarn

      # `yarn licenses list --json` returns data in a table format with header
      # ordering specified in the output.  Look for these specific headers and use
      # their indices to get data from the table body
      YARN_NAME_HEAD = "Name".freeze
      YARN_VERSION_HEAD = "Version".freeze
      YARN_URL_HEAD = "URL".freeze

      def self.version_requirement
        Gem::Requirement.new("< 2.0")
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
              "homepage" => dependency_urls[package["id"]]
            }
          )
        end
      end

      # Finds packages that the current project relies on
      def packages
        return [] if yarn_package_tree.nil?
        all_dependencies = {}
        recursive_dependencies(yarn_package_tree).each do |name, results|
          results.uniq! { |package| package["version"] }
          if results.size == 1
            # if there is only one package for a name, reference it by name
            all_dependencies[name] = results[0]
          else
            # if there is more than one package for a name, reference each by
            # "<name>-<version>"
            results.each do |package|
              all_dependencies["#{name}-#{package["version"]}"] = package
            end
          end
        end

        all_dependencies
      end

      # Recursively parse dependency JSON data.  Returns a hash mapping the
      # package name to it's metadata
      def recursive_dependencies(dependencies, result = {})
        dependencies.each do |dependency|
          # "shadow" indicate a dependency requirement only, not a
          # resolved package identifier
          next if dependency["shadow"]
          name, _, version = dependency["name"].rpartition("@")

          (result[name] ||= []) << {
            "id" => dependency["name"],
            "name" => name,
            "version" => version,
            "path" => dependency_paths[dependency["name"]]
          }
          recursive_dependencies(dependency["children"], result)
        end
        result
      end

      # Finds and returns the yarn package tree listing from `yarn list` output
      def yarn_package_tree
        return @yarn_package_tree if defined?(@yarn_package_tree)
        @yarn_package_tree = begin
          # parse all lines of output to json and find one that is "type": "tree"
          tree = yarn_list_command.lines
                                  .map(&:strip)
                                  .map(&JSON.method(:parse))
                                  .find { |json| json["type"] == "tree" }
          tree&.dig("data", "trees")
        end
      end

      # Returns a mapping of unique dependency identifiers to urls
      def dependency_urls
        @dependency_urls ||= begin
          table = yarn_licenses_command.lines
                                       .map(&:strip)
                                       .map(&JSON.method(:parse))
                                       .find { |json| json["type"] == "table" }
          return {} if table.nil?

          head = table.dig("data", "head")
          return {} if head.nil?

          name_index = head.index YARN_NAME_HEAD
          version_index = head.index YARN_VERSION_HEAD
          url_index = head.index YARN_URL_HEAD
          return {} if name_index.nil? || version_index.nil? || url_index.nil?

          body = table.dig("data", "body")
          return {} if body.nil?

          body.each_with_object({}) do |row, hsh|
            id = "#{row[name_index]}@#{row[version_index]}"
            hsh[id] = row[url_index]
          end
        end
      end

      # Returns the output from running `yarn list` to get project dependencies
      def yarn_list_command
        args = %w(--json -s --no-progress)
        args << "--production" unless include_non_production?
        Licensed::Shell.execute("yarn", "list", *args, allow_failure: true)
      end

      # Returns the output from running `yarn licenses list` to get project urls
      def yarn_licenses_command
        args = %w(--json -s --no-progress)
        args << "--production" unless include_non_production?
        Licensed::Shell.execute("yarn", "licenses", "list", *args, allow_failure: true)
      end

      # Returns whether to include non production dependencies based on the licensed configuration settings
      def include_non_production?
        config.dig("yarn", "production_only") == false
      end
    end
  end
end
