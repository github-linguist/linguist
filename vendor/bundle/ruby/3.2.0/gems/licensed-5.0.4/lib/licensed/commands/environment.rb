# frozen_string_literal: true
module Licensed
  module Commands
    class Environment < Command
      class AppEnvironment
        include Licensed::Sources::ContentVersioning

        attr_reader :config
        def initialize(config)
          @config = config
        end

        def enabled_source_types
          config.sources.select { |s| s.enabled? }.map { |s| s.class.type }
        end

        def to_h
          {
            "name" => config["name"],
            "source_path" => config.source_path,
            "cache_path" => config.cache_path,
            "sources" => enabled_source_types,
            "allowed" => config["allowed"],
            "ignored" => config["ignored"],
            "reviewed" => config["reviewed"],
            "version_strategy" => self.version_strategy,
            "root" => config.root
          }
        end
      end

      # Returns the default reporter to use during the command run
      #
      # options - The options the command was run with
      #
      # Returns a Licensed::Reporters::StatusReporter
      def default_reporter(options)
        Licensed::Reporters::YamlReporter.new
      end

      protected

      def run_command(report)
        report["git_repo"] = Licensed::Git.git_repo?
        super
      end

      def run_app(app, report)
        report.merge! AppEnvironment.new(app).to_h
        super
      end

      def run_source(app, source, report)
        true
      end
    end
  end
end
