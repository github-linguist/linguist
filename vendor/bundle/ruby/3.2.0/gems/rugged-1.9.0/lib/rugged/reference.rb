# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class Reference
    def inspect
      "#<#{self.class}:#{object_id} {name: #{name.inspect}, target: #{target.inspect}}>"
    end
  end
end
