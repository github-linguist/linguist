# frozen_string_literal: true

module Licensed
  module Reporters
    class NoticesReporter < Reporter
      TEXT_SEPARATOR = "\n\n#{("-" * 5)}\n\n".freeze
      LICENSE_SEPARATOR = "\n#{("*" * 5)}\n".freeze

      # Reports the start of an application evaluation in a notices command run
      #
      # app - An application configuration
      # report - A report object containing information about the app evaluation
      def begin_report_app(app, report)
        shell.info "Writing notices for #{app["name"]} to #{app_notices_path(app)}"
      end

      # Writes the licensing information gathered during the application evaluation
      # to a notices file
      #
      # app - An application configuration
      # report - A report object containing information about the app evaluation
      def end_report_app(app, report)
        File.open(app_notices_path(app), "w") do |file|
          file << "THIRD PARTY NOTICES\n"
          file << LICENSE_SEPARATOR
          file << report.all_reports
                        .map { |r| notices(r) }
                        .compact
                        .join(LICENSE_SEPARATOR)
        end
      end

      # Reports any warnings encountered during the run.
      #
      # source - A dependency source enumerator
      # report - A report object containing information about the source evaluation
      def end_report_source(source, report)
        report.warnings.each do |warning|
          shell.warn "* #{report.name}: #{warning}"
        end
      end

      # Reports on a dependency in a notices command run.
      #
      # dependency - An application dependency
      # report - A report object containing information about the dependency evaluation
      def end_report_dependency(dependency, report)
        report.warnings.each do |warning|
          shell.warn "* #{report.name}: #{warning}"
        end
      end

      # Returns notices information for a dependency report
      def notices(report)
        return unless report.target.is_a?(Licensed::Dependency)

        record = report["record"]
        return unless record

        texts = record.licenses.map(&:text)
        record.notices.each do |notice|
          case notice
          when Hash
            texts << notice["text"]
          when String
            texts << notice
          else
            shell.warn "* unable to parse notices for #{report.target.name}"
          end
        end

        <<~NOTICE
          #{record["name"]}@#{record["version"]}

          #{texts.map(&:strip).reject(&:empty?).compact.join(TEXT_SEPARATOR)}
        NOTICE
      end

      # Returns the path to an applications notices file
      def app_notices_path(app)
        filename = app["shared_cache"] ? "NOTICE.#{app["name"]}" : "NOTICE"
        app.cache_path.join(filename)
      end
    end
  end
end
