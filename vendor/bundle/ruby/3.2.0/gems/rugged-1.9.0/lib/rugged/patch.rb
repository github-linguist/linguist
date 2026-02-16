# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class Patch
    include Enumerable
    alias each each_hunk

    alias size hunk_count
    alias count hunk_count

    attr_accessor :owner
    alias diff owner

    def inspect
      "#<#{self.class.name}:#{object_id}>"
    end

    # Returns the number of additions in the patch.
    def additions
      stat[0]
    end

    # Returns the number of deletions in the patch.
    def deletions
      stat[1]
    end

    # Returns the number of total changes in the patch.
    def changes
      additions + deletions
    end

    # Returns an Array containing all hunks of the patch.
    def hunks
      each_hunk.to_a
    end
  end
end
