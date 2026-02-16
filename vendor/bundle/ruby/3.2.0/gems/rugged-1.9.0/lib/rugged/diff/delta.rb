# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class Diff
    class Delta
      attr_reader :owner
      alias diff owner

      attr_reader :old_file
      attr_reader :new_file
      attr_reader :similarity
      attr_reader :status
      attr_reader :status_char
      attr_reader :binary

      alias binary? binary

      def added?
        status == :added
      end

      def deleted?
        status == :deleted
      end

      def modified?
        status == :modified
      end

      def renamed?
        status == :renamed
      end

      def copied?
        status == :copied
      end

      def ignored?
        status == :ignored
      end

      def untracked?
        status == :untracked
      end

      def typechange?
        status == :typechange
      end

      def inspect
        "#<#{self.class.name}:#{object_id} {old_file: #{old_file.inspect}, new_file: #{new_file.inspect}, similarity: #{similarity.inspect}, status: #{status.inspect}>"
      end
    end
  end
end
