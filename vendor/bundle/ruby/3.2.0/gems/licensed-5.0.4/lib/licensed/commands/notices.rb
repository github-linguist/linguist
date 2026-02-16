# frozen_string_literal: true
module Licensed
  module Commands
    class Notices < Command
      # Returns the default reporter to use during the command run
      #
      # options - The options the command was run with
      #
      # Returns a Licensed::Reporters::CacheReporter
      def default_reporter(options)
        Licensed::Reporters::NoticesReporter.new
      end

      protected

      # Load a dependency record data and add it to the notices report.
      #
      # app - The application configuration for the dependency
      # source - The dependency source enumerator for the dependency
      # dependency - An application dependency
      # report - A report hash for the command to provide extra data for the report output.
      #
      # Returns true.
      def evaluate_dependency(app, source, dependency, report)
        report["record"] =
          if load_dependency_record_from_files
            load_cached_dependency_record(app, source, dependency, report)
          else
            dependency.record
          end

        true
      end

      # Loads a dependency record from a cached file.
      #
      # app - The application configuration for the dependency
      # source - The dependency source enumerator for the dependency
      # dependency - An application dependency
      # report - A report hash for the command to provide extra data for the report output.
      #
      # Returns a dependency record or nil if one doesn't exist
      def load_cached_dependency_record(app, source, dependency, report)
        filename = app.cache_path.join(source.class.type, "#{dependency.name}.#{DependencyRecord::EXTENSION}")
        record = Licensed::DependencyRecord.read(filename)
        if !record
          report.warnings << "expected cached record not found at #{filename}"
        end

        record
      end

      def load_dependency_record_from_files
        !options.fetch(:computed, false)
      end
    end
  end
end
