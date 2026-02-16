# frozen_string_literal: true

module Licensed
  module Reporters
    class ListReporter < Reporter
      # Reports the start of application configuration in a list command run
      #
      # app - An application configuration
      # report - A report object containing information about the app evaluation
      def begin_report_app(app, report)
        shell.info "Listing dependencies for #{app["name"]}"
      end

      # Reports the start of a source evaluation
      #
      # source - A dependency source enumerator
      # report - A report object containing information about the source evaluation
      def begin_report_source(source, report)
        shell.info "  #{source.class.type}"
      end

      # Report the type and count of dependencies found by the source,
      # along with any warnings and errors
      #
      # source - A dependency source enumerator
      # report - A report object containing information about the source evaluation
      def end_report_source(source, report)
        warning_reports = report.all_reports.select { |r| r.warnings.any? }.to_a
        if warning_reports.any?
          shell.newline
          shell.warn "  * Warnings:"
          warning_reports.each do |r|
            display_metadata = r.map { |k, v| "#{k}: #{v}" }.join(", ")

            shell.warn "    * #{r.name}"
            shell.warn "    #{display_metadata}" unless display_metadata.empty?
            r.warnings.each do |warning|
              shell.warn "      - #{warning}"
            end
            shell.newline
          end
        end

        errored_reports = report.all_reports.select { |r| r.errors.any? }.to_a
        if errored_reports.any?
          shell.newline
          shell.error "  * Errors:"
          errored_reports.each do |r|
            display_metadata = r.map { |k, v| "#{k}: #{v}" }.join(", ")

            shell.error "    * #{r.name}"
            shell.error "    #{display_metadata}" unless display_metadata.empty?
            r.errors.each do |error|
              shell.error "      - #{error}"
            end
            shell.newline
          end
        else
          shell.confirm "  * #{report.reports.size} #{source.class.type} dependencies"
        end
      end

      # Reports on a dependency in a list command run.
      #
      # dependency - An application dependency
      # report - A report object containing information about the dependency evaluation
      def end_report_dependency(dependency, report)
        info = "#{dependency.name} (#{dependency.version})"
        info = "#{info}: #{report["license"]}" if report["license"]
        shell.info "    #{info}"
      end
    end
  end
end
