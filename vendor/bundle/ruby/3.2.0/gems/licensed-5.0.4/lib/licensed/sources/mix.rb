# frozen_string_literal: true

module Licensed
  module Sources
    class Mix < Source

      LOCKFILE = "mix.lock"

      # Returns whether a mix.lock is present
      def enabled?
        File.exist?(lockfile_path)
      end

      def enumerate_dependencies
        find_packages.map do |package|
          convert_package_to_dependency(package)
        end
      end

      private

      # Returns the parsed mix.lock information as an Array of Hash objects.
      def find_packages
        LockfileParser.read(lockfile_path)
      end

      # Returns the absolute path to the mix.lock as a Pathname.
      def lockfile_path
        config.pwd.join(LOCKFILE)
      end

      # Converts a raw package representation to a dependency.
      #
      # name - The name of the package as a String.
      # pkg  - The parsed package data as a Hash.
      #
      # Returns a Dependency.
      def convert_package_to_dependency(pkg)
        path, errors = check_dep_path(pkg)
        Dependency.new(
          name: pkg[:name],
          version: pkg[:version],
          path: path,
          metadata: pkg[:metadata].merge("type" => self.class.type),
          errors: errors + Array(pkg[:error])
        )
      end

      # Check that the package has been installed in deps/.
      #
      # pkg - The package information as a Hash
      #
      # Returns an Array with two members; the path as a String and an Array of
      # any errors.
      def check_dep_path(pkg)
        path = dep_path(pkg[:name])
        if File.directory?(path)
          return [path, []]
        else
          return [path, ["Not installed by `mix deps.get` in deps/"]]
        end
      end

      # Generate the absolute path to the named package.
      #
      # name - The name of the package dependency as a String.
      #
      # Returns a Pathname.
      def dep_path(name)
        config.pwd.join("deps", name)
      end

      class LockfileParser

        class ParseError < RuntimeError; end

        # Top-level pattern extracting the name and Mix.SCM type
        LINE_PATTERN = /
          \A                 # At the beginning of input
          \s*                # after any number of spaces
          "(?<name>.*?)"     # capture the contents of a double-quoted string as the name
          :\s*\{             # then skipping a colon, any number of spaces, and an opening brace,
          :(?<scm>hex|git)   # capture the contents of a Elixir atom as the scm
          ,\s*               # and, skipping a comma and any number of spaces,
          (?<contents>.*)    # capture the rest of input as the contents.
        /x

        # Patterns to extract the version and repo information for each Mix.SCM type.
        SCM_PATTERN = {
          # The Hex Package Manager
          "hex" => /
            \A                # At the beginning of input
            :[a-zA-Z0-9_]+    # after an Elixir atom,
            ,\s*              # and skipping a comma and any number of spaces,
            "(?<version>.*?)" # capture the contents of a double-quoted string as the version,
            .*?\],\s*         # and later
            "(?<repo>.*?)"    # capture the contents of a double-quoted string as the repo
            (?:
              ,\s*            # a comma
              "[a-f0-9]{64}"  # a digest
            )?
            \},?\s*\Z         # right before the final closing brace.
          /x,

          # Git
          "git" => /
            \A                # At the beginning of input
            "(?<repo>.*?)"    # capture the contents of a double-quoted string as the repo
            ,\s*              # and, skipping a comma and any number of spaces,
            "(?<version>.*?)" # capture the contents of a second double-quoted string as the version.
          /x
        }

        # Parses a mix.lock to extract raw package information.
        #
        # path - The path to the mix.lock as a Pathname or String.
        #
        # Returns an Array of Hash package entries, or raises a ParserError if
        # unsuccessful.
        def self.read(path)
          lines = File.readlines(path)
          new(lines).result
        end

        def initialize(lines)
          @lines = lines
        end

        # Parses the input lines.
        #
        # Returns an Array of Hash package entries, or raises a ParseError if
        # unsuccessful.
        def result
          # Ignore the first and last lines of the file (the beginning and
          # ending of the enclosing map).
          @lines[1..-2].map do |line|
            parse_line(line)
          end
        end

        private

        # Parse a line from the mix.lock file.
        #
        # line - A line of input as a String.
        #
        # Returns a Hash package entry, or raises a ParserError if unsuccessful.
        def parse_line(line)
          match = LINE_PATTERN.match(line)
          if match
            data = SCM_PATTERN[match[:scm]].match(match[:contents])
            if data
              valid_package_entry(match, data)
            else
              invalid_package_entry(match, line)
            end
          else
            raise Licensed::Sources::Source::Error, "Unknown mix.lock line format: #{line}"
          end
        end

        # Format a valid package entry.
        #
        # match - A MatchData containing name and scm information.
        # data  - A MatchData containing version and repo information.
        #
        # Returns a Hash representing the package.
        def valid_package_entry(match, data)
          {
            name: match[:name],
            version: data[:version],
            metadata: {
              "scm" => match[:scm],
              "repo" => data[:repo]
            }
          }
        end

        # Format an invalid package entry.
        #
        # match - A MatchData containing name and scm information.
        # line  - The line from mix.lock that could not be parsed, as a String.
        #
        # Returns a Hash representing the package, with error information.
        def invalid_package_entry(match, line)
          {
            name: match[:name],
            version: nil,
            metadata: {
              "scm" => match[:scm]
            },
            error: "Could not extract data from mix.lock line: #{line}"
          }
        end
      end
    end
  end
end
