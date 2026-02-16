# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class Diff
    class Line
      attr_reader :line_origin, :content, :old_lineno, :new_lineno, :content_offset

      def context?
        @line_origin == :context
      end

      def addition?
        @line_origin == :addition
      end

      def deletion?
        @line_origin == :deletion
      end

      def eof_no_newline?
        @line_origin == :eof_no_newline
      end

      def eof_newline_added?
        @line_origin == :eof_newline_added
      end

      def eof_newline_removed?
        @line_origin == :eof_newline_removed
      end

      def file_header?
        @line_origin == :file_header
      end

      def hunk_header?
        @line_origin == :hunk_header
      end

      def binary?
        @line_origin == :binary
      end

      def inspect
        "#<#{self.class.name}:#{object_id} {line_origin: #{line_origin.inspect}, content: #{content.inspect}>"
      end
    end
  end
end
