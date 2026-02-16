# frozen_string_literal: true
require "delegate"
begin
  require "bundler"
  require "licensed/sources/bundler/missing_specification"
  require "licensed/sources/bundler/definition"
rescue LoadError
end

module Licensed
  module Sources
    class Bundler < Source
      class Dependency < Licensed::Dependency
        attr_reader :loaded_from

        def initialize(name:, version:, path:, loaded_from:, errors: [], metadata: {})
          @loaded_from = loaded_from
          super name: name, version: version, path: path, errors: errors, metadata: metadata
        end

        # Load a package manager file from the base Licensee::Projects::FsProject
        # or from a gem specification file.
        def package_file
          super || spec_file
        end

        private

        # Find a package manager file from the given bundler specification's
        # `loaded_from` if available.
        def spec_file
          return @spec_file if defined?(@spec_file)
          return @spec_file = nil unless loaded_from && File.file?(loaded_from)
          @spec_file = begin
            file = { name: File.basename(loaded_from), dir: File.dirname(loaded_from) }
            Licensee::ProjectFiles::PackageManagerFile.new(File.read(loaded_from), file)
          end
        end
      end

      DEFAULT_WITHOUT_GROUPS = %i{development test}

      def enabled?
        # if Bundler isn't loaded, this enumerator won't work!
        return false unless defined?(::Bundler)

        with_application_environment { ::Bundler.default_lockfile&.exist? }
      rescue ::Bundler::GemfileNotFound
        false
      end

      def enumerate_dependencies
        with_application_environment do
          definition.specs.map do |spec|
            next if spec.name == config["name"]

            error = spec.error if spec.respond_to?(:error)
            Dependency.new(
              name: spec.name,
              version: spec.version.to_s,
              path: spec.full_gem_path,
              loaded_from: spec.loaded_from,
              errors: Array(error),
              metadata: {
                "type"     => Bundler.type,
                "summary"  => spec.summary,
                "homepage" => spec.homepage
              }
            )
          end
        end
      end

      def definition
        @definition ||= begin
          definition = ::Bundler::Definition.build(::Bundler.default_gemfile, ::Bundler.default_lockfile, nil)
          definition.extend Licensed::Bundler::DefinitionExtensions
          definition.force_exclude_groups = exclude_groups
          definition
        end
      end

      # Returns any groups to exclude specified from both licensed configuration
      # and bundler configuration.
      # Defaults to [:development, :test] + ::Bundler.settings[:without]
      def exclude_groups
        @exclude_groups ||= begin
          exclude = Array(config.dig("bundler", "without"))
          exclude = DEFAULT_WITHOUT_GROUPS if exclude.empty?
          exclude.uniq.map(&:to_sym)
        end
      end

      # helper to clear all bundler environment around a yielded block
      def with_application_environment
        backup = nil

        ::Bundler.ui.silence do
          if ::Bundler.root != config.source_path
            backup = ENV.to_hash
            ENV.replace(::Bundler.original_env)

            # reset bundler to load from the current app's source path
            ::Bundler.reset!
          end

          # ensure the bundler environment is loaded before enumeration
          ::Bundler.load

          yield
        end
      ensure
        if backup
          # restore bundler configuration
          ENV.replace(backup)
          ::Bundler.reset!
        end

        # reload the bundler environment after enumeration
        ::Bundler.load
      end
    end
  end
end
