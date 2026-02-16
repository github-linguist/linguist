# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class Diff
    class Hunk
      def delta
        @owner
      end

      def inspect
        "#<#{self.class.name}:#{object_id} {header: #{header.inspect}, count: #{count.inspect}}>"
      end

      # Returns an Array containing all lines of the hunk.
      def lines
        each_line.to_a
      end
    end
  end
end
