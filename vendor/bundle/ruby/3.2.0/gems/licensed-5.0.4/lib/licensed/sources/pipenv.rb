# frozen_string_literal: true

require "parallel"

module Licensed
  module Sources
    class Pipenv < Pip
      def enabled?
        Licensed::Shell.tool_available?("pipenv") && File.exist?(config.pwd.join("Pipfile.lock"))
      end

      protected

      # Returns the command to run pip
      def pip_command
        %w(pipenv run pip)
      end
    end
  end
end
