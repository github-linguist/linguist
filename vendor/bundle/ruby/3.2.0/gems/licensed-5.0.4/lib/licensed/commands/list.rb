# frozen_string_literal: true
module Licensed
  module Commands
    class List < Command
      # Returns the default reporter to use during the command run
      #
      # options - The options the command was run with
      #
      # Returns a Licensed::Reporters::ListReporter
      def default_reporter(options)
        Licensed::Reporters::ListReporter.new
      end

      protected

      # Listing dependencies requires no extra work.
      #
      # app - The application configuration for the dependency
      # source - The dependency source enumerator for the dependency
      # dependency - An application dependency
      # report - A report hash for the command to provide extra data for the report output.
      #
      # Returns true.
      def evaluate_dependency(app, source, dependency, report)
        report["dependency"] = dependency.name
        report["version"] = dependency.version

        if options[:licenses]
          report["license"] = dependency.license_key
        end

        true
      end
    end
  end
end
