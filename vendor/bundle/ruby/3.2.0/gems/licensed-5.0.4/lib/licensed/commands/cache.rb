# frozen_string_literal: true
module Licensed
  module Commands
    class Cache < Command
      # Returns the default reporter to use during the command run
      #
      # options - The options the command was run with
      #
      # Returns a Licensed::Reporters::CacheReporter
      def default_reporter(options)
        Licensed::Reporters::CacheReporter.new
      end

      protected

      # Run the command.
      # Removes any cached records that don't match a current application
      # dependency.
      #
      # options - Options to run the command with
      #
      # Returns whether the command was a success
      def run_command(report)
        super do |result|
          clear_stale_cached_records if result
          result
        end
      ensure
        cache_paths.clear
        files.clear
      end

      # Run the command for an application configurations.
      # Applies a licensee configuration for the duration of the operation.
      #
      # report - A Licensed::Report object for this command
      #
      # Returns whether the command succeeded
      def run_app(app, report)
        with_licensee_configuration(app, report) do
          super
        end
      end

      # Run the command for all enumerated dependencies found in a dependency source,
      # recording results in a report.
      # Enumerating dependencies in the source is skipped if a :sources option
      # is provided and the evaluated `source.class.type` is not in the :sources values
      #
      # app - The application configuration for the source
      # source - A dependency source enumerator
      #
      # Returns whether the command succeeded for the dependency source enumerator
      def run_source(app, source, report)
        result = super

        # add the full cache path to the list of cache paths
        # that should be cleaned up after the command run
        cache_paths << app.cache_path.join(source.class.type) unless result == :skipped

        result
      end

      # Cache dependency record data.
      #
      # app - The application configuration for the dependency
      # source - The dependency source enumerator for the dependency
      # dependency - An application dependency
      # report - A report hash for the command to provide extra data for the report output.
      #
      # Returns true.
      def evaluate_dependency(app, source, dependency, report)
        if dependency.path.empty?
          report.errors << "dependency path not found"
          return false
        end

        filename = app.cache_path.join(source.class.type, "#{dependency.name}.#{DependencyRecord::EXTENSION}")
        cached_record = Licensed::DependencyRecord.read(filename)

        report["cached"] = false
        report["license"] = cached_record["license"] if cached_record
        report["filename"] = filename.to_s
        report["version"] = dependency.version

        if save_dependency_record?(dependency, cached_record)
          update_dependency_from_cached_record(app, source, dependency, cached_record)

          dependency.record.save(filename)
          report["cached"] = true
          report["license"] = dependency.record["license"]
        end

        if !dependency.exist?
          report.warnings << "expected dependency path #{dependency.path} does not exist"
        end

        # add the absolute dependency file path to the list of files seen during this licensed run
        files << filename.to_s

        true
      end

      # Determine if the current dependency's record should be saved.
      # The record should be saved if:
      # 1. there is no cached record
      # 2. the cached record doesn't have a version set
      # 3. the cached record version doesn't match the current dependency version
      #
      # dependency - An application dependency
      # cached_record - A dependency record to compare with the dependency
      #
      # Returns true if dependency's record should be saved
      def save_dependency_record?(dependency, cached_record)
        return true if cached_record.nil?
        return true if options[:force]

        cached_version = cached_record["version"]
        return true if cached_version.nil? || cached_version.empty?
        return true if dependency.version != cached_version
        false
      end

      # Update dependency metadata from the cached record, to support:
      # 1. continuity between cache runs to cut down on churn
      # 2. notifying users when changed content needs to be reviewed
      def update_dependency_from_cached_record(app, source, dependency, cached_record)
        return if cached_record.nil?
        return if options[:force]

        if dependency.record.matches?(cached_record)
          # use the cached license value if the license text wasn't updated
          dependency.record["license"] = cached_record["license"]
        elsif app.reviewed?(dependency.record, require_version: source.class.require_matched_dependency_version)
          # if the license text changed and the dependency is set as reviewed
          # force a re-review of the dependency
          dependency.record["review_changed_license"] = true
        end
      end

      # Clean up cached files that dont match current dependencies
      #
      # Returns nothing
      def clear_stale_cached_records
        cache_paths.each do |cache_path|
          Dir.glob(cache_path.join("**/*.#{DependencyRecord::EXTENSION}")).each do |file|
            next if files.include?(file)

            FileUtils.rm(file)
          end
        end
      end

      # Set of unique cache paths that are evaluted during the run
      def cache_paths
        @cache_paths ||= Set.new
      end

      # Set of unique absolute file paths of cached records evaluted during the run
      def files
        @files ||= Set.new
      end

      # Configure licensee for the duration of a yielded operation
      def with_licensee_configuration(app, report)
        licensee_configuration = app["licensee"]
        return yield unless licensee_configuration

        report["licensee"] = licensee_configuration

        if new_threshold = licensee_configuration["confidence_threshold"]
          old_threshold, Licensee.confidence_threshold = Licensee.confidence_threshold, new_threshold
        end

        yield
      ensure
        Licensee.confidence_threshold = old_threshold if old_threshold
      end
    end
  end
end
