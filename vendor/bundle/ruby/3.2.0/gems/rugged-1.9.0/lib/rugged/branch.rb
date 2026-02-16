# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class Branch < Rugged::Reference
    def ==(other)
      other.instance_of?(Rugged::Branch) &&
        other.canonical_name == self.canonical_name
    end

    # Get the remote the branch belongs to.
    #
    # If the branch is remote returns the remote it belongs to.
    # In case of local branch, it returns the remote of the branch
    # it tracks or nil if there is no tracking branch.
    #
    def remote
      remote_name = self.remote_name
      @owner.remotes[remote_name] if remote_name
    end
  end
end
