# frozen_string_literal: true
require "json"
require "pathname"
require "licensed/sources/helpers/content_versioning"

module Licensed
  module Sources
    class Go < Source
      include Licensed::Sources::ContentVersioning

      def enabled?
        Licensed::Shell.tool_available?("go") && go_source?
      end

      def enumerate_dependencies
        with_configured_gopath do
          packages.map do |package|
            import_path = non_vendored_import_path(package)
            error = package.dig("Error", "Err") if package["Error"]

            Dependency.new(
              name: import_path,
              version: package_version(package),
              path: package["Dir"],
              search_root: search_root(package),
              errors: Array(error),
              metadata: {
                "type"        => Go.type,
                "summary"     => package["Doc"],
                "homepage"    => homepage(import_path)
              }
            )
          end
        end
      end

      # Returns an array of dependency package import paths
      def packages
        # don't include go std packages
        # don't include packages under the root project that aren't vendored
        go_list_deps
          .reject { |pkg| go_std_package?(pkg) }
          .reject { |pkg| local_package?(pkg) }
      end

      # Returns the list of dependencies as returned by "go list -json -deps"
      # available in go 1.11
      def go_list_deps
        args = ["-deps"]
        args << "-mod=vendor" if config.dig("go", "mod") == "vendor"

        # the CLI command returns packages in a pretty-printed JSON format but
        # not separated by commas. this gsub adds commas after all non-indented
        # "}" that close root level objects.
        # (?!\z) uses negative lookahead to not match the final "}"
        deps = package_info_command(*args).gsub(/^}(?!\z)$/m, "},")
        JSON.parse("[#{deps}]")
      end

      # Returns whether the given package import path belongs to the
      # go std library or not
      #
      # package - package to check as part of the go standard library
      def go_std_package?(package)
        return false unless package

        # return true if package self-identifies
        return true if package["Standard"]

        import_path = non_vendored_import_path(package)
        return false unless import_path

        # check different variations of the import path to match against
        # what's returned from `go list std`
        [
          import_path,
          import_path.sub("golang.org", "internal"),
          import_path.sub("golang.org", "golang_org"),
        ].any? do |path|
          # true if go standard packages includes the path or "vendor/<path>"
          go_std_packages.include?(path) || go_std_packages.include?("vendor/#{path}")
        end
      end

      # Returns whether the package is local to the current project
      def local_package?(package)
        return false unless package && package["Dir"]
        return false unless File.fnmatch?("#{config.root}*", package["Dir"], File::FNM_CASEFOLD)
        vendored_path_parts(package).nil?
      end

      # Returns the version for a given package
      #
      # package - package to get version of
      def package_version(package)
        # use module version if it exists
        go_mod = package["Module"]
        return go_mod["Version"] if go_mod

        package_directory = package["Dir"]
        return unless package_directory && File.exist?(package_directory)

        # find most recent git SHA for a package, or nil if SHA is
        # not available
        Dir.chdir package_directory do
          contents_version(*contents_version_arguments)
        end
      end

      # Determines the arguments to pass to contents_version based on which
      # version strategy is selected
      #
      # Returns an array of arguments to pass to contents version
      def contents_version_arguments
        if version_strategy == Licensed::Sources::ContentVersioning::GIT
          ["."]
        else
          Dir["*"]
        end
      end

      # Returns the pkg.go.dev page for a package.
      def homepage(import_path)
        return unless import_path
        "https://pkg.go.dev/#{import_path}"
      end

      # Returns the root directory to search for a package license
      #
      # package - package object obtained from package_info
      def search_root(package)
        return if package.nil?

        # search root choices:
        # 1. module directory if using go modules and directory is available
        module_dir = package.dig("Module", "Dir")
        return module_dir if module_dir

        # 2. vendor folder if package is vendored
        parts = vendored_path_parts(package)
        return parts[:vendor_path] if parts

        # 3. package root value if available
        return package["Root"] if package["Root"]

        # 4. GOPATH if the package directory is under the gopath
        return gopath if package["Dir"]&.start_with?(gopath)

        # 5. nil
        nil
      end

      # If the package is vendored, returns a Match object containing named
      # :vendor_path and :import_path match groups based on the packages "Dir" value
      #
      # If the package is not vendored, returns nil
      #
      # package - Package to get vendored path information for
      def vendored_path_parts(package)
        return if package.nil? || package["Dir"].nil?
        package["Dir"].match(/^(?<vendor_path>#{config.root}(\/.+)*\/[^\/]*vendor[^\/]*)\/(?<import_path>.+)$/i)
      end

      # Returns the non-vendored portion of the package import path if vendored,
      # otherwise returns the package's import path as given
      #
      # package - Package to get the non-vendored import path for
      def non_vendored_import_path(package)
        return if package.nil?
        parts = vendored_path_parts(package)
        return parts[:import_path] if parts

        # if a package isn't vendored, return the packages "ImportPath"
        package["ImportPath"]
      end

      # Returns package information as a JSON string
      #
      # args - additional arguments to `go list`, e.g. Go package import path
      def package_info_command(*args)
        Licensed::Shell.execute("go", "list", "-e", "-json", *Array(args)).strip
      end

      # Returns whether go source is found
      def go_source?
        with_configured_gopath { Licensed::Shell.success?("go", "doc") }
      end

      # Returns a list of go standard packages
      def go_std_packages
        @std_packages ||= Licensed::Shell.execute("go", "list", "std").lines.map(&:strip)
      end

      # Returns a GOPATH value from either a configuration value or ENV["GOPATH"],
      # with the configuration value preferred over the ENV var
      def gopath
        return @gopath if defined?(@gopath)

        @gopath = begin
          path = config.dig("go", "GOPATH")
          return File.expand_path(path, config.root) unless path.to_s.empty?
          return ENV["GOPATH"] if ENV["GOPATH"]
          Licensed::Shell.execute("go", "env", "GOPATH")
        end
      end

      private

      # Execute a block with ENV["GOPATH"] set to the value of #gopath.
      # Any pre-existing ENV["GOPATH"] value is restored after the block ends.
      def with_configured_gopath(&block)
        begin
          original_gopath = ENV["GOPATH"]
          ENV["GOPATH"] = gopath

          block.call
        ensure
          ENV["GOPATH"] = original_gopath
        end
      end
    end
  end
end
