# frozen_string_literal: true

module Licensed
  module Sources
    module Yarn
      module ClassMethods
        def type
          "yarn"
        end
      end

      def self.included(klass)
        klass.extend ClassMethods
      end

      def enabled?
        return unless Licensed::Shell.tool_available?("yarn")
        return unless self.class.version_requirement.satisfied_by?(yarn_version)

        config.pwd.join("package.json").exist? && config.pwd.join("yarn.lock").exist?
      end

      def yarn_version
        Gem::Version.new(Licensed::Shell.execute("yarn", "-v"))
      end

      # Returns a hash that maps all dependency names to their location on disk
      # by parsing every package.json file under node_modules.
      def dependency_paths
        @dependency_paths ||= [
            *Dir.glob(config.pwd.join("**/node_modules/*/package.json")),
            *Dir.glob(config.pwd.join("**/node_modules/@*/*/package.json"))
          ].each_with_object({}) do |file, hsh|
          begin
            dirname = File.dirname(file)
            json = JSON.parse(File.read(file))
            hsh["#{json["name"]}@#{json["version"]}"] = dirname
          rescue JSON::ParserError
            # don't crash execution if there is a problem parsing a package.json file
            # if the bad package.json file relates to a package that licensed should be reporting on
            # then this will still result in an error about a missing package
          end
        end
      end
    end
  end
end

require "licensed/sources/yarn/v1"
require "licensed/sources/yarn/berry"
