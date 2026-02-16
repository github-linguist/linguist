# frozen_string_literal: true
require "thor"

module Licensed
  module UI
    class Shell
      LEVELS = %w(silent error warn confirm info debug)

      def initialize
        @shell = STDOUT.tty? ? Thor::Base.shell.new : Thor::Shell::Basic.new
        @level = ENV["DEBUG"] ? "debug" : "info"
      end

      def debug(msg, newline = true)
        @shell.say msg, nil, newline if level?("debug")
      end

      def info(msg, newline = true)
        @shell.say msg, nil, newline if level?("info")
      end

      def confirm(msg, newline = true)
        @shell.say msg, :green, newline if level?("confirm")
      end

      def warn(msg, newline = true)
        @shell.say msg, :yellow, newline if level?("warn")
      end

      def error(msg, newline = true)
        @shell.say msg, :red, newline if level?("error")
      end

      def newline
        info ""
      end

      def level=(level)
        raise ArgumentError unless LEVELS.include?(level.to_s)
        @level = level
      end

      def level?(name = nil)
        name ? LEVELS.index(name) <= LEVELS.index(@level) : @level
      end

      def silence
        old_level, @level = @level, "silent"
        yield
      ensure
        @level = old_level
      end
    end
  end
end
