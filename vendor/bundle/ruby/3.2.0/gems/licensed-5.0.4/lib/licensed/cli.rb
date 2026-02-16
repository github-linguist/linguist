# frozen_string_literal: true
require "licensed"
require "thor"

module Licensed
  class CLI < Thor

    desc "cache", "Cache the licenses of dependencies"
    method_option :force, type: :boolean,
      desc: "Overwrite licenses even if version has not changed."
    method_option :config, aliases: "-c", type: :string,
      desc: "Path to licensed configuration file"
    method_option :sources, aliases: "-s", type: :array,
      desc: "Individual source(s) to evaluate.  Must also be enabled via configuration."
    method_option :format, aliases: "-f", enum: ["yaml", "json"],
      desc: "Output format"
    def cache
      run Licensed::Commands::Cache.new(config: config),
          force: options[:force], sources: options[:sources], reporter: options[:format]
    end

    desc "status", "Check status of dependencies' cached licenses"
    method_option :config, aliases: "-c", type: :string,
      desc: "Path to licensed configuration file"
    method_option :sources, aliases: "-s", type: :array,
      desc: "Individual source(s) to evaluate.  Must also be enabled via configuration."
    method_option :format, aliases: "-f", enum: ["yaml", "json"],
      desc: "Output format"
    method_option :data_source, aliases: "-d",
      enum: ["files", "configuration"], default: "files",
      desc: "Whether to check compliance status from cached records or the configuration file"
    def status
      run Licensed::Commands::Status.new(config: config), sources: options[:sources], reporter: options[:format], data_source: options[:data_source]
    end

    desc "list", "List dependencies"
    method_option :config, aliases: "-c", type: :string,
      desc: "Path to licensed configuration file"
    method_option :sources, aliases: "-s", type: :array,
      desc: "Individual source(s) to evaluate.  Must also be enabled via configuration."
    method_option :format, aliases: "-f", enum: ["yaml", "json"],
      desc: "Output format"
    method_option :licenses, aliases: "-l", type: :boolean,
      desc: "Include detected licenses in output"
    def list
      run Licensed::Commands::List.new(config: config), sources: options[:sources], reporter: options[:format], licenses: options[:licenses]
    end

    desc "notices", "Generate a NOTICE file with dependency data"
    method_option :config, aliases: "-c", type: :string,
      desc: "Path to licensed configuration file"
    method_option :sources, aliases: "-s", type: :array,
      desc: "Individual source(s) to evaluate.  Must also be enabled via configuration."
    method_option :computed, aliases: "-l", type: :boolean,
      desc: "Whether to generate a NOTICE file using computed data or cached records"
    def notices
      run Licensed::Commands::Notices.new(config: config), sources: options[:sources], computed: options[:computed]
    end

    map "-v" => :version
    map "--version" => :version
    desc "version", "Show Installed Version of Licensed, [-v, --version]"
    def version
      puts Licensed::VERSION
    end

    desc "env", "Output licensed environment configuration"
    method_option :format, enum: ["yaml", "json"], default: "yaml",
      desc: "Output format"
    method_option :config, aliases: "-c", type: :string,
      desc: "Path to licensed configuration file"
    def env
      run Licensed::Commands::Environment.new(config: config), reporter: options[:format]
    end

    desc "migrate", "Migrate from a previous version of licensed"
    method_option :config, aliases: "-c", type: :string, required: true,
      desc: "Path to licensed configuration file"
    method_option :from, aliases: "-f", type: :string, required: true,
      desc: "Licensed version to migrate from - #{Licensed.previous_major_versions.map { |major| "v#{major}" }.join(", ")}"
    def migrate
      shell = Thor::Base.shell.new
      case options["from"]
      when "v1"
        Licensed::Migrations::V2.migrate(options["config"])
      when "v2"
        shell.say "No configuration or cached file migration needed."
        shell.say "Please see the documentation at https://github.com/github/licensed/tree/master/docs/migrations/v3.md for details."
      else
        shell.say "Unrecognized option from=#{options["from"]}", :red
        CLI.command_help(shell, "migrate")
        exit 1
      end
    end

    # If an error occurs (e.g. a missing command or argument), exit 1.
    def self.exit_on_failure?
      true
    end

    private

    # Returns a configuration object for the CLI command
    def config
      @config ||= Configuration.load_from(config_path)
    end

    # Returns a config path from the CLI if set.
    # Defaults to the current directory if CLI option is not set
    def config_path
      options["config"] || Dir.pwd
    end

    def run(command, **args)
      exit command.run(**args)
    end
  end
end
