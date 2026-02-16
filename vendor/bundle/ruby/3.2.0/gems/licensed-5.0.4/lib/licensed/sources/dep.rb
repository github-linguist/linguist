# frozen_string_literal: true
require "tomlrb"

module Licensed
  module Sources
    class Dep < Source
      def enabled?
        go_dep_available?
      end

      def enumerate_dependencies
        packages.map do |package|
          package_dir = config.pwd.join("vendor", package[:name])
          search_root = config.pwd.join("vendor", package[:project])

          Dependency.new(
            name: package[:name],
            version: package[:version],
            path: package_dir.to_s,
            search_root: search_root.to_s,
            metadata: {
              "type"        => Dep.type,
              "homepage"    => homepage(package[:name])
            }
          )
        end
      end

      # Returns an array of dependency packages specified from Gopkg.lock
      def packages
        gopkg_lock = Tomlrb.load_file(gopkg_lock_path, symbolize_keys: true)
        return [] unless gopkg_lock && gopkg_lock[:projects]

        gopkg_lock[:projects].flat_map do |project|
          # map each package to a full import path
          # then return a hash for each import path containing the path and the version
          project[:packages].map { |package| package == "." ? project[:name] : "#{project[:name]}/#{package}" }
                            .reject { |import_path| go_std_package?(import_path) }
                            .map { |import_path| { name: import_path, version: project[:revision], project: project[:name] } }
        end
      end

      # Returns the pkg.go.dev page for a package.
      def homepage(import_path)
        return unless import_path
        "https://pkg.go.dev/#{import_path}"
      end

      # Returns whether the package is part of the go std list.  Replaces
      # "golang.org" with "golang_org" to match packages listed in `go list std`
      # as "vendor/golang_org/*" but are vendored as "vendor/golang.org/*"
      def go_std_package?(import_path)
        return true if go_std_packages.include? "vendor/#{import_path}"
        go_std_packages.include? "vendor/#{import_path.sub(/^golang.org/, "golang_org")}"
      end

      def go_dep_available?
        gopkg_lock_path.exist?
      end

      def gopkg_lock_path
        config.pwd.join("Gopkg.lock")
      end

      # Returns a list of go standard packages
      def go_std_packages
        @std_packages ||= begin
          return [] unless Licensed::Shell.tool_available?("go")
          Licensed::Shell.execute("go", "list", "std").lines.map(&:strip)
        end
      end
    end
  end
end
