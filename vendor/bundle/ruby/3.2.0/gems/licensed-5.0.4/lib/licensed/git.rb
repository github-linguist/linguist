# frozen_string_literal: true
module Licensed
  module Git
    # Returns whether git commands are available
    def self.available?
      @git ||= Licensed::Shell.tool_available?("git")
    end

    # Returns the root of the current git repository
    # or nil if not in a git repository.
    def self.repository_root
      return unless available?
      root = Licensed::Shell.execute("git", "rev-parse", "--show-toplevel", allow_failure: true)
      return nil if root.empty?
      root
    end

    # Returns true if a git repository is found, false otherwise
    def self.git_repo?
      !repository_root.to_s.empty?
    end

    # Returns the most recent git SHA for a file or directory
    # or nil if SHA is not available
    #
    # descriptor - file or directory to retrieve latest SHA for
    def self.version(descriptor)
      return unless git_repo? && descriptor
      Licensed::Shell.execute("git", "rev-list", "-1", "HEAD", "--", descriptor, allow_failure: true)
    end

    # Returns the commit date for the provided SHA as a timestamp
    #
    # sha - commit sha to retrieve date
    def self.commit_date(sha)
      return unless git_repo? && sha
      Licensed::Shell.execute("git", "show", "-s", "-1", "--format=%ct", sha)
    end

    # Returns the files in the git repository from `git ls-files --recurse-submodules`
    def self.files
      return unless git_repo?
      output = Licensed::Shell.execute("git", "ls-files", "--full-name", "--recurse-submodules")
      output.lines.map(&:strip)
    end
  end
end
