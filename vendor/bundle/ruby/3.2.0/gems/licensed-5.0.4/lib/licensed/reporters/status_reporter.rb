# frozen_string_literal: true

module Licensed
  module Reporters
    class StatusReporter < Reporter
      # Reports any errors encountered at the command level
      #
      # command - The command being run
      # report - A report object containing information about the command run
      def end_report_command(command, report)
        if report.warnings.any?
          shell.newline
          report.warnings.each { |e| shell.warn e }
        end

        if report.errors.any?
          shell.newline
          report.errors.each { |e| shell.error e }
        end
      end

      # Reports the start of checking records for an app
      #
      # app - An application configuration
      # report - A report containing information about the app evaluation
      def begin_report_app(app, report)
        shell.info "Checking cached dependency records for #{app["name"]}"
      end

      # Reports any errors found when checking status, as well as
      # overall number of dependencies checked
      #
      # app - An application configuration
      # report - A report containing information about the app evaluation
      def end_report_app(app, report)
        all_reports = report.all_reports

        warning_reports = all_reports.select { |r| r.warnings.any? }.to_a
        if warning_reports.any?
          shell.newline
          shell.warn "Warnings:"
          warning_reports.each do |r|
            display_metadata = r.map { |k, v| "#{k}: #{v}" }.join(", ")

            shell.warn "* #{r.name}"
            shell.warn "  #{display_metadata}" unless display_metadata.empty?
            r.warnings.each do |warning|
              shell.warn "    - #{warning}"
            end
            shell.newline
          end
        end

        errored_reports = all_reports.select { |r| r.errors.any? }.to_a

        dependency_count = all_reports.count { |r| r.target.is_a?(Licensed::Dependency) }
        error_count = errored_reports.reduce(0) { |count, r| count + r.errors.size }

        if error_count > 0
          shell.newline
          shell.error "Errors:"
          errored_reports.each do |r|
            display_metadata = r.map { |k, v| "#{k}: #{v}" }.join(", ")

            shell.error "* #{r.name}"
            shell.error "  #{display_metadata}" unless display_metadata.empty?
            r.errors.each do |error|
              shell.error "    - #{error}"
            end
            shell.newline
          end
        end

        shell.newline
        shell.info "#{dependency_count} dependencies checked, #{error_count} errors found."
      end

      # Reports whether the dependency's status is valid in dot format
      #
      # dependency - An application dependency
      # report - A report containing information about the dependency evaluation
      def end_report_dependency(dependency, report)
        if report.errors.empty?
          shell.confirm(".", false)
        else
          shell.error("F", false)
        end
      end
    end
  end
end
