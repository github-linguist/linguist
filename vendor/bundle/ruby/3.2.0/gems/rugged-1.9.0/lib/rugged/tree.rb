# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class Tree
    ##
    # call-seq:
    #   Tree.diff(repo, tree, diffable[, options]) -> diff
    #
    # Returns a diff between the `tree` and the diffable object that was given.
    # +diffable+ can either be a +Rugged::Commit+, a +Rugged::Tree+, a +Rugged::Index+,
    # or +nil+.
    #
    # The +tree+ object will be used as the "old file" side of the diff, while the
    # parent tree or the +diffable+ object will be used for the "new file" side.
    #
    # If +tree+ or +diffable+ are nil, they will be treated as an empty tree. Passing
    # both as `nil` will raise an exception.
    #
    # The following options can be passed in the +options+ Hash:
    #
    # :paths ::
    #   An array of paths / fnmatch patterns to constrain the diff to a specific
    #   set of files. Also see +:disable_pathspec_match+.
    #
    # :max_size ::
    #   An integer specifying the maximum byte size of a file before a it will
    #   be treated as binary. The default value is 512MB.
    #
    # :context_lines ::
    #   The number of unchanged lines that define the boundary of a hunk (and
    #   to display before and after the actual changes). The default is 3.
    #
    # :interhunk_lines ::
    #   The maximum number of unchanged lines between hunk boundaries before the hunks
    #   will be merged into a one. The default is 0.
    #
    # :old_prefix ::
    #   The virtual "directory" to prefix to old filenames in hunk headers.
    #   The default is "a".
    #
    # :new_prefix ::
    #   The virtual "directory" to prefix to new filenames in hunk headers.
    #   The default is "b".
    #
    # :reverse ::
    #   If true, the sides of the diff will be reversed.
    #
    # :force_text ::
    #   If true, all files will be treated as text, disabling binary attributes & detection.
    #
    # :ignore_whitespace ::
    #   If true, all whitespace will be ignored.
    #
    # :ignore_whitespace_change ::
    #   If true, changes in amount of whitespace will be ignored.
    #
    # :ignore_whitespace_eol ::
    #   If true, whitespace at end of line will be ignored.
    #
    # :ignore_submodules ::
    #   if true, submodules will be excluded from the diff completely.
    #
    # :patience ::
    #   If true, the "patience diff" algorithm will be used (currenlty unimplemented).
    #
    # :include_ignored ::
    #   If true, ignored files will be included in the diff.
    #
    # :include_untracked ::
    #  If true, untracked files will be included in the diff.
    #
    # :include_unmodified ::
    #   If true, unmodified files will be included in the diff.
    #
    # :recurse_untracked_dirs ::
    #   Even if +:include_untracked+ is true, untracked directories will only be
    #   marked with a single entry in the diff. If this flag is set to true,
    #   all files under ignored directories will be included in the diff, too.
    #
    # :disable_pathspec_match ::
    #   If true, the given +:paths+ will be applied as exact matches, instead of
    #   as fnmatch patterns.
    #
    # :deltas_are_icase ::
    #   If true, filename comparisons will be made with case-insensitivity.
    #
    # :show_untracked_content ::
    #   if true, untracked content will be contained in the the diff patch text.
    #
    # :skip_binary_check ::
    #   If true, diff deltas will be generated without spending time on binary
    #   detection. This is useful to improve performance in cases where the actual
    #   file content difference is not needed.
    #
    # :include_typechange ::
    #   If true, type changes for files will not be interpreted as deletion of
    #   the "old file" and addition of the "new file", but will generate
    #   typechange records.
    #
    # :include_typechange_trees ::
    #   Even if +:include_typechange+ is true, blob -> tree changes will still
    #   usually be handled as a deletion of the blob. If this flag is set to true,
    #   blob -> tree changes will be marked as typechanges.
    #
    # :ignore_filemode ::
    #   If true, file mode changes will be ignored.
    #
    # :recurse_ignored_dirs ::
    #   Even if +:include_ignored+ is true, ignored directories will only be
    #   marked with a single entry in the diff. If this flag is set to true,
    #   all files under ignored directories will be included in the diff, too.
    #
    # Examples:
    #
    #   # Emulating `git diff <treeish>`
    #   tree = Rugged::Tree.lookup(repo, "d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    #   diff = tree.diff(repo.index)
    #   diff.merge!(tree.diff)
    #
    #   # Tree-to-Tree Diff
    #   tree = Rugged::Tree.lookup(repo, "d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
    #   other_tree = Rugged::Tree.lookup(repo, "7a9e0b02e63179929fed24f0a3e0f19168114d10")
    #   diff = tree.diff(other_tree)
    #

    def self.diff(repo, tree, other_tree = nil, options = {})
      if tree && !tree.is_a?(Rugged::Tree)
        raise TypeError, "At least a Rugged::Tree object is required for diffing"
      end

      if other_tree.nil?
        if tree.nil?
          raise TypeError, "Need 'old' or 'new' for diffing"
        else
          diff_tree_to_tree repo, tree, nil, options
        end
      else
        if other_tree.is_a?(::String)
          other_tree = Rugged::Object.rev_parse repo, other_tree
        end

        case other_tree
        when Rugged::Commit
          diff_tree_to_tree repo, tree, other_tree.tree, options
        when Rugged::Tree
          diff_tree_to_tree repo, tree, other_tree, options
        when Rugged::Index
          diff_tree_to_index repo, tree, other_tree, options
        else
          raise TypeError, "A Rugged::Commit, Rugged::Tree or Rugged::Index instance is required"
        end
      end
    end

    include Enumerable

    attr_reader :owner
    alias repo owner

    def diff(other = nil, options = nil)
      Tree.diff(repo, self, other, options)
    end

    def inspect
      data = "#<Rugged::Tree:#{object_id} {oid: #{oid}}>\n"
      self.each { |e| data << "  <\"#{e[:name]}\" #{e[:oid]}>\n" }
      data
    end

    # Walks the tree but only yields blobs
    def walk_blobs(mode=:postorder)
      return to_enum(__method__) unless block_given?
      self.walk(mode) { |root, e| yield root, e if e[:type] == :blob }
    end

    # Walks the tree but only yields subtrees
    def walk_trees(mode=:postorder)
      return to_enum(__method__) unless block_given?
      self.walk(mode) { |root, e| yield root, e if e[:type] == :tree }
    end

    # Iterate over the blobs in this tree
    def each_blob
      return to_enum(__method__) unless block_given?
      self.each { |e| yield e if e[:type] == :blob }
    end

    # Iterate over the subtrees in this tree
    def each_tree
      return to_enum(__method__) unless block_given?
      self.each { |e| yield e if e[:type] == :tree }
    end
  end
end
