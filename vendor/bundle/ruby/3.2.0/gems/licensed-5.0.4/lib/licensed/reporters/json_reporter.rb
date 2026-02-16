# frozen_string_literal: true
require "json"

module Licensed
  module Reporters
    class JsonReporter < Reporter
      # Report all information from the command run to the shell as a JSON object
      #
      # command - The command being run
      # report - A report object containing information about the command run
      def end_report_command(command, report)
        report["apps"] = report.reports.map(&:to_h) if report.reports.any?
        shell.info JSON.pretty_generate(report.to_h)
      end

      # Add source report information to the app report hash
      #
      # app - An application configuration
      # report - A report object containing information about the app evaluation
      def end_report_app(app, report)
        report["sources"] = report.reports.map(&:to_h) if report.reports.any?
      end

      # Add dependency report information to the source report hash
      #
      # source - A dependency source enumerator
      # report - A report object containing information about the source evaluation
      def end_report_source(source, report)
        report["dependencies"] = report.reports.map(&:to_h) if report.reports.any?
      end
    end
  end
end
