# frozen_string_literal: true
require "tempfile"
require "csv"
require "uri"
require "json"
require "net/http"
require "fileutils"

module Licensed
  module Sources
    class Gradle < Source
      DEFAULT_CONFIGURATIONS   = ["runtimeOnly", "runtimeClasspath"].freeze
      GRADLE_LICENSES_PATH     = ".gradle-licenses".freeze
      GRADLE_LICENSES_CSV_NAME = "licenses.csv".freeze
      class Dependency < Licensed::Dependency
        # Cache and return the results of getting the license content.
        def self.retrieve_license(url)
          (@licenses ||= {})[url] ||= Net::HTTP.get(URI(url))
        end

        def initialize(name:, version:, path:, url:, metadata: {})
          @url = url
          super(name: name, version: version, path: path, metadata: metadata)
        end

        # Returns whether the dependency content exists
        def exist?
          # shouldn't force network connections just to check if content exists
          # only check that the path is not empty
          !path.to_s.empty?
        end

        # Returns a Licensee::ProjectFiles::LicenseFile for the dependency
        def project_files
          return [] if @url.nil?

          license_data = self.class.retrieve_license(@url)
          Array(Licensee::ProjectFiles::LicenseFile.new(license_data, { uri: @url }))
        end
      end

      def enabled?
        !executable.to_s.empty? && File.exist?(config.pwd.join("build.gradle"))
      end

      def enumerate_dependencies
        JSON.parse(gradle_runner.run("printDependencies")).map do |package|
          name = "#{package['group']}:#{package['name']}"
          Dependency.new(
            name: name,
            version: package["version"],
            path: config.pwd,
            url: package_url(name: name, version: package["version"]),
            metadata: {
              "type" => Gradle.type,
            }
          )
        end
      end

      private

      def executable
        return @executable if defined?(@executable)

        @executable = begin
          return gradlew if File.executable?(gradlew)

          "gradle" if Licensed::Shell.tool_available?("gradle")
        end
      end

      def gradle_runner
        @gradle_runner ||= Runner.new(configurations, executable)
      end

      # Returns the configurations to include in license generation.
      # Defaults to ["runtime", "runtimeClasspath"]
      def configurations
        @configurations ||= begin
          if configurations = config.dig("gradle", "configurations")
            Array(configurations)
          else
            DEFAULT_CONFIGURATIONS
          end
        end
      end

      # Returns the path to the Gradle wrapper.
      def gradlew
        @gradlew ||= begin
          gradlew = config.dig("gradle", "gradlew")
          config.root.join(gradlew || "gradlew").to_s
        end
      end

      # Returns a key to uniquely identify a name and version in the obtained CSV content
      def csv_key(name:, version:)
        "#{name}-#{version}"
      end

      def package_url(name:, version:)
        # load and memoize the license report CSV
        @urls ||= load_csv

        # uniquely identify a name and version in the obtained CSV content
        @urls["#{name}-#{version}"]
      end

      def load_csv
        begin
          # create the CSV file including dependency license urls using the gradle plugin
          gradle_licenses_dir = File.join(config.root, GRADLE_LICENSES_PATH)
          gradle_runner.run("generateLicenseReport")

          # parse the CSV report for dependency license urls
          CSV.foreach(File.join(gradle_licenses_dir, GRADLE_LICENSES_CSV_NAME), headers: true).each_with_object({}) do |row, hsh|
            name, _, version = row["artifact"].rpartition(":")
            key = csv_key(name: name, version: version)
            hsh[key] = row["moduleLicenseUrl"]
          end
        ensure
          FileUtils.rm_rf(gradle_licenses_dir)
        end
      end

      # Returns the cached url for the given dependency
      def url_for(dependency)
        @csv[csv_key(name: dependency.name, version: dependency.version)]
      end

      # The Gradle::Runner class is a wrapper which provides
      # an interface to run gradle commands with the init script initialized
      class Runner
        def initialize(configurations, executable)
          @executable = executable
          @init_script = create_init_script(configurations)
        end

        def run(command)
          args = [command]
          # The configuration cache is an incubating feature that can be activated manually.
          # The gradle plugin for licenses does not support it so we prevent it to run for gradle version supporting it.
          args << "--no-configuration-cache" if gradle_version >= Gem::Version.new("6.6")
          Licensed::Shell.execute(@executable, "-q", "--init-script", @init_script.path, *args)
        end

        private

        def create_init_script(configurations)
          # we need to create extensions in the event that the user hasn't configured custom configurations
          # to avoid hitting errors where core Gradle configurations are set with canBeResolved=false
          configuration_map = configurations.map { |c| [c, "licensed#{c}"] }.to_h
          configuration_dsl = configuration_map.map { |orig, custom| "#{custom}.extendsFrom(#{orig})" }

          f = Tempfile.new(["init", ".gradle"])
          f.write(
            <<~EOF
                import com.github.jk1.license.render.CsvReportRenderer
                import com.github.jk1.license.filter.LicenseBundleNormalizer
                final configs = #{configuration_map.values.inspect}

                initscript {
                  repositories {
                    maven {
                      url "https://plugins.gradle.org/m2/"
                    }
                  }
                  dependencies {
                    classpath "com.github.jk1:gradle-license-report:#{gradle_version >= Gem::Version.new("7.0") ? "2.0" : "1.17"}"
                  }
                }

                allprojects {
                  configurations {
                    #{configuration_dsl.join("\n") }
                  }

                  apply plugin: com.github.jk1.license.LicenseReportPlugin
                  licenseReport {
                      outputDir = "$rootDir/#{GRADLE_LICENSES_PATH}"
                      configurations = configs
                      renderers = [new CsvReportRenderer()]
                      filters = [new LicenseBundleNormalizer()]
                  }

                  task printDependencies {
                    doLast {
                        def dependencies = []
                        configs.each {
                            configurations[it].resolvedConfiguration.resolvedArtifacts.each { artifact ->
                                def id = artifact.moduleVersion.id
                                dependencies << "{ \\"group\\": \\"${id.group}\\", \\"name\\": \\"${id.name}\\", \\"version\\": \\"${id.version}\\" }"
                            }
                        }
                        println "[${dependencies.join(", ")}]"
                    }
                  }
                }
              EOF
            )
          f.close
          f
        end

        # Returns the version of gradle used during execution
        def gradle_version
          @gradle_version ||= begin
            version = Licensed::Shell.execute(@executable, "--version").scan(/Gradle [\d+]\.[\d+]/).last.split(" ").last
            Gem::Version.new(version)
          end
        end
      end
    end
  end
end
