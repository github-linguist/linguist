# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class Commit
    def self.prettify_message(msg, strip_comments = true)
      Rugged::prettify_message(msg, strip_comments)
    end

    def inspect
      "#<Rugged::Commit:#{object_id} {message: #{message.inspect}, tree: #{tree.inspect}, parents: #{parent_oids}}>"
    end

    def header_field?(field)
      !!header_field(field)
    end

    # Return a diff between this commit and its first parent or another commit or tree.
    #
    # The commit is treated as the new side of the diff by default
    #
    # See Rugged::Tree#diff for more details.
    def diff(*args)
      raise ArgumentError("wrong number of arguments (given #{args.length}, expected 0..2") if args.length > 2
      other, opts = args
      if other.is_a?(Hash)
        opts = other
        other = nil
      end
      opts ||= {}
      # if other is not provided at all (as opposed to explicitly nil, or given)
      # then diff against the prior commit
      if args.empty? || args.first.is_a?(Hash)
        other = parents.first
        opts[:reverse] = !opts[:reverse]
      end
      self.tree.diff(other, opts)
    end

    # Return a diff between this commit and the workdir.
    #
    # See Rugged::Tree#diff_workdir for more details.
    def diff_workdir(options = {})
      self.tree.diff_workdir(**options)
    end

    # The time when this commit was made effective. This is the same value
    # as the +:time+ attribute for +commit.committer+.
    #
    # Returns a Time object
    def time
      @time ||= Time.at(self.epoch_time)
    end

    def to_hash
      {
        :message => message,
        :committer => committer,
        :author => author,
        :tree => tree,
        :parents => parents,
      }
    end

    def modify(new_args, update_ref=nil)
      args = self.to_hash.merge(new_args)
      Commit.create(args, update_ref)
    end
  end
end
