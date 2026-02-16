# frozen_string_literal: true

require "pathname"
require_relative "../command"
require_relative "../helpers/frame"

module Byebug
  #
  # Show current backtrace.
  #
  class WhereCommand < Command
    include Helpers::FrameHelper

    self.allow_in_post_mortem = true

    def self.regexp
      /^\s* (?:w(?:here)?|bt|backtrace) (?:\s+(\S+))? \s*$/x
    end

    def self.description
      <<-DESCRIPTION
        w[here]|bt|backtrace[ maximum-frame]

        #{short_description}

        Print the entire stack frame. Each frame is numbered; the most recent
        frame is 0. A frame number can be referred to in the "frame" command.
        "up" and "down" add or subtract respectively to frame numbers shown.
        The position of the current frame is marked with -->. C-frames hang
        from their most immediate Ruby frame to indicate that they are not
        navigable.

        Without an argument, the command prints all the frames. With an argument,
        the command prints the nth first frames, where n is the largest between
        the argument or the maximum stack frame.
      DESCRIPTION
    end

    def self.short_description
      "Displays the backtrace"
    end

    def execute
      print_backtrace
    end

    private

    def print_backtrace
      max_frame =
        if @match[1] && @match[1].to_i <= context.stack_size
          @match[1].to_i
        else
          context.stack_size
        end

      bt = prc("frame.line", 0...max_frame) do |_, index|
        Frame.new(context, index).to_hash
      end

      print(bt)
    end
  end
end
