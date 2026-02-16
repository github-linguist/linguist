# frozen_string_literal: true

require "ruby-xxhash"

module Licensed
  module Sources
    module ContentVersioning
      GIT = "git".freeze
      CONTENTS = "contents".freeze

      # Find the version for a list of paths using the version strategy
      # specified for the source from the configuration
      #
      # paths - list of paths to find version
      #
      # Returns a version identifier for the given files
      def contents_version(*paths)
        case version_strategy
        when CONTENTS
          contents_hash(paths)
        when GIT
          git_version(paths)
        end
      end

      # Returns the version strategy configured for the source
      def version_strategy
        # default to git for backwards compatible behavior
        @version_strategy ||= begin
          case config.fetch("version_strategy", nil)
          when CONTENTS
            CONTENTS
          when GIT
            GIT
          else
            Licensed::Git.available? ? GIT : CONTENTS
          end
        end
      end

      # Find the version for a list of paths using Git commit information
      #
      # paths - list of paths to find version
      #
      # Returns the most recent git SHA from the given paths
      def git_version(paths)
        return if paths.nil?

        paths.map { |path| Licensed::Git.version(path) }
             .reject { |sha| sha.to_s.empty? }
             .max_by { |sha| Licensed::Git.commit_date(sha) }
      end

      # Find the version for a list of paths using their file contents
      #
      # paths - list of paths to find version
      #
      # Returns a hash of the path contents as an identifier for the group
      def contents_hash(paths)
        return if paths.nil?

        paths = paths.compact.select { |path| File.file?(path) }
        return if paths.empty?
        # rubocop:disable GitHub/InsecureHashAlgorithm
        paths.sort
             .reduce(Digest::XXHash64.new, :file)
             .digest
             .to_s(16) # convert to hex
        # rubocop:enable GitHub/InsecureHashAlgorithm
      end
    end
  end
end
