# frozen_string_literal: true
require "open3"

module Licensed
  module Shell
    # Executes a command, returning its standard output on success.
    # On failure it raises an exception that contains the error output, unless
    # `allow_failure` is true.
    def self.execute(cmd, *args, allow_failure: false, env: {})
      stdout, stderr, status = Open3.capture3(env, cmd, *args)

      if !status.success? && !allow_failure
        raise Error.new([cmd, *args], status.exitstatus, encode_content(stderr))
      end

      # ensure that returned data is properly encoded
      encode_content(stdout.strip)
    end

    # Executes a command and returns a boolean value indicating if the command
    # was succesful
    def self.success?(cmd, *args)
      _, _, status = Open3.capture3(cmd, *args)
      status.success?
    end

    # Returns a boolean indicating whether a CLI tool is available in the
    # current environment
    def self.tool_available?(tool)
      output, err, status = Open3.capture3("which", tool)
      status.success? && !output.strip.empty? && err.strip.empty?
    end

    class Error < RuntimeError
      attr_reader :cmd, :status, :stderr
      def initialize(cmd, status, stderr)
        super()
        @cmd = cmd
        @exitstatus = status
        @stderr = stderr.to_s.strip
      end

      def message
        extra = @stderr.empty? ? "" : "#{@stderr.gsub(/^/, "        ")}"
        "'#{escape_cmd}' exited with status #{@exitstatus}\n#{extra}"
      end

      def escape_cmd
        @cmd.map do |arg|
          if arg =~ /[\s'"]/
            escaped = arg.gsub(/([\\"])/, '\\\\\1')
            %("#{escaped}")
          else
            arg
          end
        end.join(" ")
      end
    end

    private

    ENCODING = Encoding::UTF_8
    ENCODING_OPTIONS = {
      invalid: :replace,
      undef:   :replace,
      replace: "",
      univeral_newline: true
    }.freeze

    # Ensure that content that is returned from shell commands is in a usable
    # encoding for the rest of the application
    def self.encode_content(content)
      content.encode(ENCODING, **ENCODING_OPTIONS)
    end
  end
end
