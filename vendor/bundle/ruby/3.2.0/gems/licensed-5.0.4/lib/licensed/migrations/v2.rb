# frozen_string_literal: true
require "licensed/shell"

module Licensed
  module Migrations
    class V2
      YAML_FRONTMATTER_PATTERN = /\A---\s*\n(.*?\n?)^---\s*$\n?(.*)\z/m
      TEXT_SEPARATOR = ("-" * 80).freeze
      LICENSE_SEPARATOR = ("*" * 80).freeze

      def self.migrate(config_path, shell = Licensed::UI::Shell.new)
        shell.info "updating to v2"

        shell.info "updating bundler configuration keys"
        # replace all "rubygem" and "rubygems" configuration keys with "bundler"
        # to account for the bundler source's `type` change from `rubygem` to `bundler`
        File.write(config_path, File.read(config_path).gsub(/("?)rubygems?("?):/, "\\1bundler\\2:"))

        shell.info "updating cached records"
        # load the configuration to find and update cached contents
        configuration = Licensed::Configuration.load_from(config_path)
        configuration.apps.each do |app|

          # move any bundler records from the `rubygem` folder to the `bundler` folder
          rubygem_cache = app.cache_path.join("rubygem")
          if rubygem_cache.exist?
            File.rename rubygem_cache, app.cache_path.join("bundler")
          end

          app.sources.each do |source|
            cache_path = app.cache_path.join(source.class.type)
            next unless File.exist?(cache_path)
            Dir.chdir cache_path do
              # licensed v1 cached records were stored as .txt files with YAML frontmatter
              Dir["**/*.txt"].each do |file|
                yaml, licenses, notices = parse_file(file)

                # rename the rubygem type to bundler
                yaml["type"] = "bundler" if yaml["type"] == "rubygem"

                # set licenses and notices as yaml properties
                yaml["licenses"] = licenses.map { |text| { "text" => text } }
                yaml["notices"] = notices.map { |text| { "text" => text } }

                # v2 records are stored in `.dep.yml` files
                # write the new yaml contents to the new file and delete old file
                new_file = file.gsub(".txt", ".dep.yml")
                File.write(new_file, yaml.to_yaml)
                File.delete(file)
              end
            end
          end
        end
      end

      # find the yaml and non-yaml data according to parsing logic from v1
      def self.parse_file(filename)
        match = File.read(filename).scrub.match(YAML_FRONTMATTER_PATTERN)
        yaml = YAML.load(match[1])
        # in v1, licenses and notices are separated by special text dividers
        licenses, *notices = match[2].split(TEXT_SEPARATOR).map(&:strip)
        licenses = licenses.split(LICENSE_SEPARATOR).map(&:strip)
        [yaml, licenses, notices]
      end
    end
  end
end
