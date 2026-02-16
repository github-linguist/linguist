# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class SubmoduleCollection

    # call-seq:
    #   submodules.setup_add(url, path[, options]) -> submodule
    #
    # Add a new +submodule+.
    #
    # This does <tt>"git submodule add"</tt> incuding fetch and checkout of the
    # submodule contents.
    #
    # - +url+: URL for the submodule's remote
    # - +path+: path at which the submodule should be created
    #
    # The +options+ hash accepts all options supported by Rugged::Remote#fetch
    # and the following:
    # :gitlink ::
    #   (defaults to +true+) should workdir contain a
    #   gitlink to the repository in +.git/modules+ vs. repository
    #   directly in workdir.
    #
    # Returns the newly created +submodule+
    def add(url, path, options = {})
      submodule = setup_add(url, path, **options)
      clone_submodule(submodule.repository, **options)
      submodule.finalize_add
    end

    private
    # currently libgit2's `git_submodule_add_setup` initializes a repo
    # with a workdir for the submodule. libgit2's `git_clone` however
    # requires the target for the clone to be an empty dir.
    #
    # This provides a ghetto clone implementation that:
    # 1. fetches the remote
    # 2. sets up a master branch to be tracking origin/master
    # 3. checkouts the submodule
    def clone_submodule(repo, **fetch_options)
      # the remote was just added by setup_add, no need to check presence
      repo.remotes['origin'].fetch(**fetch_options)

      repo.branches.create('master','origin/master')
      repo.branches['master'].upstream = repo.branches['origin/master']

      repo.checkout_head(strategy: :force)
    end
  end
end
