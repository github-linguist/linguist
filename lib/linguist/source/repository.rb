module Linguist
  module Source
    # Repository is an interface for providing direct access to functionality in
    # a repository of files whose contents can be scanned for language
    # information.
    class Repository
      # Public: get the number of entries in the root tree of the given commit,
      # with an optional maximum value.
      #
      # commit_id - the string unique identifier of the commit to analyze.
      # limit     - (Optional) the integer maximum number of tree entries to
      #             count.
      #
      # Returns the number of entries in the tree or 'limit', whichever is
      # smaller.
      def get_tree_size(commit_id, limit = nil)
        raise NotImplementedError
      end

      # Public: set the commit whose .gitattributes file(s) should be used as
      # the source of attribute information in 'load_attributes_for_path'.
      #
      # commit_id - the string unique identifier of the attribute source commit.
      #
      # Returns nothing.
      def set_attribute_source(commit_id)
        raise NotImplementedError
      end

      # Public: read the data and size information for the specified file blob.
      #
      # blob_id  - the string unique identifier of the blob to read.
      # max_size - the integer maximum size in bytes to read from the blob.
      #
      # Returns the (possibly truncated) byte string of blob content and
      # the full, untruncated size of the blob.
      def load_blob(blob_id, max_size)
        raise NotImplementedError
      end

      # Public: look up the attribute values for a given path.
      #
      # path       - the path for which we want attribute values.
      # attr_names - the attributes to read for the given path.
      #
      # Returns a Hash mapping attribute names to their corresponding values.
      def load_attributes_for_path(path, attr_names)
        raise NotImplementedError
      end

      # Public: compute the diff between the given old and new commits.
      #
      # old_commit - the string unique identifier of the "before" state of the
      #              diff, or nil (representing an empty tree).
      # new_commit - the string unique identifier of the "after" state of the
      #              diff, or nil (representing an empty tree).
      #
      # Returns a Source::Diff.
      def diff(old_commit, new_commit)
        raise NotImplementedError
      end
    end
  end
end
