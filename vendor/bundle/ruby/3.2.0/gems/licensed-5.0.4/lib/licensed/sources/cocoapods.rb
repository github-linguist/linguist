# frozen_string_literal: true
require "json"
require "pathname"
require "uri"

module Licensed
  module Sources
    class Cocoapods < Source
      DEFAULT_POD_COMMAND = "pod".freeze
      MISSING_PLUGIN_MESSAGE = "Error running `pods dependencies`. Please ensure the cocoapods-dependencies-list gem is installed, it is required for licensed to enumerate dependencies.".freeze

      def enabled?
        return unless Licensed::Shell.tool_available?("pod")

        config.pwd.join("Podfile").exist? && config.pwd.join("Podfile.lock").exist?
      end

      def enumerate_dependencies
        pods.map do |pod|
          Dependency.new(
            name: pod["name"],
            version: pod["version"],
            path: pod["path"],
            metadata: {
              "type" => Cocoapods.type,
              "summary"  => pod["summary"],
              "homepage" => pod["homepage"]
            }
          )
        end
      end

      private

      def pods
        cocoapods_dependencies_json.values.flatten
      end

      def cocoapods_dependencies_json
        args = ["dependencies", "--include-path"]
        args << "--targets=#{targets.join(",")}" if targets.any?

        output = Licensed::Shell.execute(*pod_command, *args, allow_failure: true)
        if output.include? "Unknown command"
          raise Licensed::Sources::Source::Error, MISSING_PLUGIN_MESSAGE
        end

        JSON.parse(output)
      rescue JSON::ParserError => e
        message = "Licensed was unable to parse the output from 'pod dependencies'. JSON Error: #{e.message}"
        raise Licensed::Sources::Source::Error, message
      end

      def targets
        return [] unless [String, Array].any? { |type| source_config["targets"].is_a?(type) }
        Array(source_config["targets"]).map { |t| "Pods-#{t}" }
      end

      def pod_command
        return DEFAULT_POD_COMMAND unless source_config["command"].is_a?(String)
        source_config["command"].split
      end
    end
  end
end
