# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class Index
    include Enumerable


    # call-seq:
    #   index.diff([options]) -> diff
    #   index.diff(diffable[, options]) -> diff
    #
    # The first form returns a diff between the index and the current working
    # directory.
    #
    # The second form returns a diff between the index and the given diffable object.
    # +diffable+ can either be a +Rugged::Commit+ or a +Rugged::Tree+.
    #
    # The index will be used as the "old file" side of the diff, while the working
    # directory or the +diffable+ will be used for the "new file" side.
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
    def diff(*args)
      options = args.last.is_a?(Hash) ? args.pop : {}
      other   = args.shift

      case other
      when nil
        diff_index_to_workdir options
      when ::Rugged::Commit
        diff_tree_to_index other.tree, options
      when ::Rugged::Tree
        diff_tree_to_index other, options
      else
        raise TypeError, "A Rugged::Commit or Rugged::Tree instance is required"
      end
    end

    def to_s
      s = "#<Rugged::Index\n"
      self.each do |entry|
        s << "  [#{entry[:stage]}] '#{entry[:path]}'\n"
      end
      s + '>'
    end
  end
end
