# frozen_string_literal: true

require "json"

module Licensed
  module Sources
    class Cargo < Source
      # Source is enabled when the cargo tool and Cargo.toml manifest file are available
      def enabled?
        return false unless Licensed::Shell.tool_available?("cargo")
        config.pwd.join("Cargo.toml").exist?
      end

      def enumerate_dependencies
        packages.map do |package|
          Dependency.new(
            name: "#{package["name"]}-#{package["version"]}",
            version: package["version"],
            path: File.dirname(package["manifest_path"]),
            metadata: {
              "name" => package["name"],
              "type" => Cargo.type,
              "summary" => package["description"],
              "homepage" => package["homepage"]
            }
          )
        end
      end

      # Returns the package data for all dependencies used to build the current package
      def packages
        cargo_metadata_resolved_node_ids.map { |id| cargo_metadata_packages[id] }
      end

      # Returns the ids of all resolved nodes used to build the current package
      def cargo_metadata_resolved_node_ids
        cargo_metadata.dig("resolve", "nodes")
                      .map { |node| node["id"] }
                      .reject { |id| cargo_metadata_workspace_members.include?(id) }

      end

      # Returns a hash of id => package pairs sourced from the "packages" cargo metadata property
      def cargo_metadata_packages
        @cargo_metadata_packages ||= cargo_metadata["packages"].each_with_object({}) do |package, hsh|
          hsh[package["id"]] = package
        end
      end

      # Returns a set of the ids of packages in the current workspace
      def cargo_metadata_workspace_members
        @cargo_metadata_workspace_members ||= Set.new(Array(cargo_metadata["workspace_members"]))
      end

      # Returns parsed JSON metadata returned from the cargo CLI
      def cargo_metadata
        @cargo_metadata ||= JSON.parse(cargo_metadata_command)
      rescue JSON::ParserError => e
        message = "Licensed was unable to parse the output from 'cargo metadata'. JSON Error: #{e.message}"
        raise Licensed::Sources::Source::Error, message
      end

      # Runs a command to get cargo metadata for the current package
      def cargo_metadata_command
        options = Array(config.dig("cargo", "metadata_options")).flat_map(&:split)
        Licensed::Shell.execute("cargo", "metadata", "--format-version=1", *options)
      end
    end
  end
end
