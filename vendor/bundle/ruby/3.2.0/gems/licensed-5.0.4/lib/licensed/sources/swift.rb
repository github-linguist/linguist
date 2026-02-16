# frozen_string_literal: true
require "json"
require "pathname"
require "uri"

module Licensed
  module Sources
    class Swift < Source
      def enabled?
        return unless Licensed::Shell.tool_available?("swift") && swift_package?
        File.exist?(package_resolved_file_path)
      end

      def enumerate_dependencies
        pins.map { |pin|
          name = pin["package"]
          version = pin.dig("state", "version")
          path = dependency_path_for_url(pin["repositoryURL"])
          error = "Unable to determine project path from #{url}" unless path

          Dependency.new(
            name: name,
            path: path,
            version: version,
            errors: Array(error),
            metadata: {
              "type"      => Swift.type,
              "homepage"  => homepage_for_url(pin["repositoryURL"])
            }
          )
        }
      end

      private

      def pins
        return @pins if defined?(@pins)

        @pins = begin
          json = JSON.parse(File.read(package_resolved_file_path))
          json.dig("object", "pins")
        rescue => e
          message = "Licensed was unable to read the Package.resolved file. Error: #{e.message}"
          raise Licensed::Sources::Source::Error, message
        end
      end

      def dependency_path_for_url(url)
        last_path_component = URI(url).path.split("/").last.sub(/\.git$/, "")
        File.join(config.pwd, ".build", "checkouts", last_path_component)
      rescue URI::InvalidURIError
      end

      def homepage_for_url(url)
        return unless %w{http https}.include?(URI(url).scheme)
        url.sub(/\.git$/, "")
      rescue URI::InvalidURIError
      end

      def package_resolved_file_path
        File.join(config.pwd, "Package.resolved")
      end

      def swift_package?
        Licensed::Shell.success?("swift", "package", "describe")
      end
    end
  end
end
