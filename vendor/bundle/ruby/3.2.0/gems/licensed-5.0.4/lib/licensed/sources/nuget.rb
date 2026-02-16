# frozen_string_literal: true
require "json"
require "reverse_markdown"

module Licensed
  module Sources
    # Only supports ProjectReference (project.assets.json) style restore used in .NET Core.
    # Does not currently support packages.config style restore.
    class NuGet < Source
      def self.type_and_version
        ["nuget"]
      end

      class NuGetDependency < Licensed::Dependency
        LICENSE_FILE_REGEX = /<license\s*type\s*=\s*\"\s*file\s*\"\s*>\s*(.*)\s*<\/license>/ix.freeze
        LICENSE_URL_REGEX = /<licenseUrl>\s*(.*)\s*<\/licenseUrl>/ix.freeze
        PROJECT_URL_REGEX = /<projectUrl>\s*(.*)\s*<\/projectUrl>/ix.freeze
        PROJECT_DESC_REGEX = /<description>\s*(.*)\s*<\/description>/ix.freeze

        # Returns the metadata that represents this dependency.  This metadata
        # is written to YAML in the dependencys cached text file
        def license_metadata
          super.tap do |record_metadata|
            record_metadata["homepage"] = project_url if project_url
            record_metadata["summary"] = description if description
          end
        end

        def nuspec_path
          name = @metadata["name"]
          File.join(self.path, "#{name.downcase}.nuspec")
        end

        def nuspec_contents
          return @nuspec_contents if defined?(@nuspec_contents)
          @nuspec_contents = begin
            return unless nuspec_path && File.exist?(nuspec_path)
            File.read(nuspec_path)
          end
        end

        def project_url
          return @project_url if defined?(@project_url)
          @project_url = begin
            return unless nuspec_contents
            match = nuspec_contents.match PROJECT_URL_REGEX
            match[1] if match && match[1]
          end
        end

        def description
          return @description if defined?(@description)
          @description = begin
            return unless nuspec_contents
            match = nuspec_contents.match PROJECT_DESC_REGEX
            match[1] if match && match[1]
          end
        end

        def project_files
          @nuget_project_files ||= begin
            files = super().flatten.compact

            # Only include the local file if it's a file licensee didn't already detect
            nuspec_license_filename = File.basename(nuspec_local_license_file.filename) if nuspec_local_license_file
            if nuspec_license_filename && files.none? { |file| File.basename(file.filename) == nuspec_license_filename }
              files.push(nuspec_local_license_file)
            end

            # Only download licenseUrl if no recognized license was found locally
            if files.none? { |file| file.license && file.license.key != "other" }
              files.push(nuspec_remote_license_file)
            end

            files.compact
          end
        end

        # Look for a <license type="file"> element in the nuspec that points to an
        # on-disk license file (which licensee may not find due to a non-standard filename)
        def nuspec_local_license_file
          return @nuspec_local_license_file if defined?(@nuspec_local_license_file)
          return unless nuspec_contents

          match = nuspec_contents.match LICENSE_FILE_REGEX
          return unless match && match[1]

          license_path = File.join(File.dirname(nuspec_path), match[1])
          return unless File.exist?(license_path)

          license_data = File.read(license_path)
          @nuspec_local_license_file = Licensee::ProjectFiles::LicenseFile.new(license_data, license_path)
        end

        # Look for a <licenseUrl> element in the nuspec that either is known to contain a license identifier
        # in the URL, or points to license text on the internet that can be downloaded.
        def nuspec_remote_license_file
          return @nuspec_remote_license_file if defined?(@nuspec_remote_license_file)
          return unless nuspec_contents

          match = nuspec_contents.match LICENSE_URL_REGEX
          return unless match && match[1]

          # Attempt to fetch the license content
          license_content = self.class.retrieve_license(match[1])
          @nuspec_remote_license_file = Licensee::ProjectFiles::LicenseFile.new(license_content, { uri: match[1] }) if license_content
        end

        def self.strip_html(html)
          return unless html

          return html unless html.downcase.include?("<html")
          ReverseMarkdown.convert(html, unknown_tags: :bypass)
        end

        def self.ignored_url?(url)
          # Many Microsoft packages that now use <license> use this for <licenseUrl>
          # No need to fetch this page - it just contains NuGet documentation
          url == "https://aka.ms/deprecateLicenseUrl"
        end

        def self.text_content_url(url)
          # Convert github file URLs to raw URLs
          return url unless match = url.match(/https?:\/\/(?:www\.)?github.com\/([^\/]+)\/([^\/]+)\/blob\/(.*)/i)
          "https://github.com/#{match[1]}/#{match[2]}/raw/#{match[3]}"
        end

        def self.retrieve_license(url)
          return unless url
          return if ignored_url?(url)

          # Transform URLs that are known to return HTML but have a corresponding text-based URL
          text_url = text_content_url(url)

          raw_content = fetch_content(text_url)
          strip_html(raw_content)
        end

        def self.fetch_content(url, redirect_limit = 5)
          url = URI.parse(url) if url.instance_of? String
          return @response_by_url[url] if (@response_by_url ||= {}).key?(url)
          return if redirect_limit == 0

          begin
            response = Net::HTTP.get_response(url)
            case response
            when Net::HTTPSuccess     then
              @response_by_url[url] = response.body
            when Net::HTTPRedirection then
              redirect_url = URI.parse(response["location"])
              if redirect_url.relative?
                redirect_url = url + redirect_url
              end
              # The redirect might be to a URL that requires transformation, i.e. a github file
              redirect_url = text_content_url(redirect_url.to_s)
              @response_by_url[url] = fetch_content(redirect_url, redirect_limit - 1)
            end
          rescue
            # Host might no longer exist or some other error, ignore
          end
        end
      end

      def project_assets_file_path
        File.join(config.pwd, nuget_obj_path, "project.assets.json")
      end

      def project_assets_file
        return @project_assets_file if defined?(@project_assets_file)
        @project_assets_file = File.read(project_assets_file_path)
      end

      def project_assets_json
        @project_assets_json ||= JSON.parse(project_assets_file)
      rescue JSON::ParserError => e
        message = "Licensed was unable to read the project.assets.json file. Error: #{e.message}"
        raise Licensed::Sources::Source::Error, message
      end

      def nuget_obj_path
        config.dig("nuget", "obj_path") || ""
      end

      def enabled?
        File.exist?(project_assets_file_path)
      end

      # Inspect project.assets.json files for package references.
      # Ideally we'd use `dotnet list package` instead, but its output isn't
      # easily machine readable and doesn't contain everything we need.
      def enumerate_dependencies
        reference_keys.map do |reference_key|
          package_id_parts = reference_key.partition("/")
          name = package_id_parts[0]
          version = package_id_parts[-1]
          id = "#{name}-#{version}"

          path = full_dependency_path(reference_key)
          error = "Package #{id} path was not found in project.assets.json, or does not exist on disk at any project package folder" if path.nil?

          NuGetDependency.new(
            name: id,
            version: version,
            path: path,
            errors: Array(error),
            metadata: {
              "type" => NuGet.type,
              "name" => name
            }
          )
        end
      end

      # Returns a unique set of the package reference keys used across all target groups
      def reference_keys
        all_reference_keys = project_assets_json["targets"].flat_map do |_, references|
          references.select { |key, reference| reference["type"] == "package" }
                    .keys
        end

        Set.new(all_reference_keys)
      end

      # Returns a dependency's path, if it exists, in one of the project's global or fallback package folders
      def full_dependency_path(reference_key)
        dependency_path = project_assets_json.dig("libraries", reference_key, "path")
        return unless dependency_path

        nuget_package_dirs = [
          project_assets_json.dig("project", "restore", "packagesPath"),
          *Array(project_assets_json.dig("project", "restore", "fallbackFolders"))
        ].compact

        nuget_package_dirs.map { |dir| File.join(dir, dependency_path) }
                          .find { |path| File.directory?(path) }
      end
    end
  end
end
