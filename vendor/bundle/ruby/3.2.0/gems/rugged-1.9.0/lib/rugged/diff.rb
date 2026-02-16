# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

require 'rugged/diff/hunk'
require 'rugged/diff/line'
require 'rugged/diff/delta'

module Rugged
  class Diff
    include Enumerable
    alias each each_patch

    attr_reader :owner

    def patches
      each_patch.to_a
    end

    def deltas
      each_delta.to_a
    end
  end
end
