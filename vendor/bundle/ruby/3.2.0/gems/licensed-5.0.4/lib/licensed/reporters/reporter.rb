# frozen_string_literal: true
module Licensed
  module Reporters
    class Reporter
      class ReportingError < StandardError; end;

      def initialize(shell = Licensed::UI::Shell.new)
        @shell = shell
      end

      # Report the beginning of a command evaluation
      #
      # command - The command being run
      # report - A report object containing information about the command run
      def begin_report_command(command, report)
      end

      # Report the end of a command evaluation
      #
      # command - The command being run
      # report - A report object containing information about the command run
      def end_report_command(command, report)
      end

      # Report the beginning of an app evaluation
      #
      # app - An application configuration
      # report - A report object containing information about the app evaluation
      def begin_report_app(app, report)
      end

      # Report the end of an app evaluation
      #
      # app - An application configuration
      # report - A report object containing information about the app evaluation
      def end_report_app(app, report)
      end

      # Report the beginning of a source evaluation
      #
      # source - A dependency source enumerator
      # report - A report object containing information about the source evaluation
      def begin_report_source(source, report)
      end

      # Report the end of a source evaluation
      #
      # source - A dependency source enumerator
      # report - A report object containing information about the source evaluation
      def end_report_source(source, report)
      end

      # Report the beginning of a dependency evaluation
      #
      # dependency - An application dependency
      # source - A report object containing information about the dependency evaluation
      def begin_report_dependency(dependency, report)
      end

      # Report the end of a dependency evaluation
      #
      # dependency - An application dependency
      # source - A report object containing information about the dependency evaluation
      def end_report_dependency(dependency, report)
      end

      protected

      attr_reader :shell
    end
  end
end
