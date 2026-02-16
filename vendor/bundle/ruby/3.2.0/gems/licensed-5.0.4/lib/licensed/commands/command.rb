# frozen_string_literal: true
module Licensed
  module Commands
    class Command
      attr_reader :config
      attr_reader :reporter
      attr_reader :options

      def initialize(config:)
        @config = config
      end

      # Run the command
      #
      # options - Options to run the command with
      #
      # Returns whether the command was a success
      def run(**options)
        @options = options
        @reporter = create_reporter(options)

        command_report = Licensed::Report.new(name: nil, target: self)
        run_command(command_report)
      ensure
        @options = nil
        @reporter = nil
      end

      # Creates a reporter to use during a command run
      #
      # options - The options the command was run with
      #
      # Returns the reporter to use during the command run
      def create_reporter(options)
        return options[:reporter] if options[:reporter].is_a?(Licensed::Reporters::Reporter)

        if options[:reporter].is_a?(String)
          klass = "#{options[:reporter].capitalize}Reporter"
          return Licensed::Reporters.const_get(klass).new if Licensed::Reporters.const_defined?(klass)
        end

        default_reporter(options)
      end

      # Returns the default reporter to use during the command run
      #
      # options - The options the command was run with
      #
      # Raises an error
      def default_reporter(options)
        raise "`default_reporter` must be implemented by commands"
      end

      protected

      # Run the command for all application configurations
      #
      # report - A Licensed::Report object for this command
      #
      # Returns whether the command succeeded
      def run_command(report)
        reporter.begin_report_command(self, report)
        apps = config.apps.sort_by { |app| app["name"] }
        results = apps.map do |app|
          app_report = Licensed::Report.new(name: app["name"], target: app)
          report.reports << app_report
          run_app(app, app_report)
        end

        result = results.all?

        result = yield(result) if block_given?

        result
      ensure
        reporter.end_report_command(self, report)
      end

      # Run the command for all enabled sources for an application configuration,
      # recording results in a report.
      #
      # app - An application configuration
      # report - A report object for this application
      #
      # Returns whether the command succeeded for the application.
      def run_app(app, report)
        reporter.begin_report_app(app, report)

        # ensure the app source path exists before evaluation
        if !Dir.exist?(app.source_path)
          report.errors << "No such directory #{app.source_path}"
          return false
        end

        Dir.chdir app.source_path do
          sources = app.sources.select(&:enabled?)
                               .sort_by { |source| source.class.type }
          results = sources.map do |source|
            source_report = Licensed::Report.new(name: [report.name, source.class.type].join("."), target: source)
            report.reports << source_report
            run_source(app, source, source_report)
          end

          result = results.all?

          result = yield(result) if block_given?

          result
        end
      rescue Licensed::Shell::Error => err
        report.errors << err.message
        false
      ensure
        reporter.end_report_app(app, report)
      end

      # Run the command for all enumerated dependencies found in a dependency source,
      # recording results in a report.
      #
      # app - The application configuration for the source
      # source - A dependency source enumerator
      # report - A report object for this source
      #
      # Returns whether the command succeeded, failed, or was skipped for the dependency source enumerator
      def run_source(app, source, report)
        reporter.begin_report_source(source, report)

        if !sources_overrides.empty? && !sources_overrides.include?(source.class.type)
          report.warnings << "skipped source"

          # return a symbol to speficy the source was skipped.
          # This is truthy and will result in the source being considered successful
          return :skipped
        end

        dependencies = source.dependencies.sort_by { |dependency| dependency.name }
        results = dependencies.map do |dependency|
          dependency_report = Licensed::Report.new(name: [report.name, dependency.name].join("."), target: dependency)
          report.reports << dependency_report
          run_dependency(app, source, dependency, dependency_report)
        end

        result = results.all?

        result = yield(result) if block_given?

        result
      rescue Licensed::Shell::Error => err
        report.errors << err.message
        false
      rescue Licensed::Sources::Source::Error => err
        report.errors << err.message
        false
      ensure
        reporter.end_report_source(source, report)
      end

      # Run the command for a dependency, evaluating the dependency and
      # recording results in a report.  Dependencies that were found with errors
      # are not evaluated and add any errors to the dependency report.
      #
      # app - The application configuration for the dependency
      # source - The dependency source enumerator for the dependency
      # dependency - An application dependency
      # report - A report object for this dependency
      #
      # Returns whether the command succeeded for the dependency
      def run_dependency(app, source, dependency, report)
        reporter.begin_report_dependency(dependency, report)

        if dependency.errors?
          report.errors.concat(dependency.errors)
          return false
        end

        result = evaluate_dependency(app, source, dependency, report)

        result = yield(result) if block_given?

        result
      rescue Licensed::DependencyRecord::Error, Licensed::Shell::Error => err
        report.errors << err.message
        false
      ensure
        reporter.end_report_dependency(dependency, report)
      end

      # Evaluate a dependency for the command.  Must be implemented by a command implementation.
      #
      # app - The application configuration for the dependency
      # source - The dependency source enumerator for the dependency
      # dependency - An application dependency
      # report - A report hash for the command to provide extra data for the report output.
      #
      # Returns whether the command succeeded for the dependency
      def evaluate_dependency(app, source, dependency, report)
        raise "`evaluate_dependency` must be implemented by a command"
      end

      def sources_overrides
        @sources_overrides = Array(options[:sources])
      end
    end
  end
end
