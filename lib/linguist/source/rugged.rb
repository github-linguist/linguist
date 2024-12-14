require 'rugged'
require 'linguist/source/diff'

module Linguist
  module Source
    # RuggedRepository is an implementation of the Source::Repository abstract
    # class. It represents a Git repository that is accessed using the libgit2
    # wrapper Rugged.
    class RuggedRepository < Linguist::Source::Repository

      class Diff < Linguist::Source::Diff
        class Delta < Linguist::Source::Diff::Delta
          def initialize(rugged_delta)
            @delta = rugged_delta
          end

          def status; @delta.status; end

          def binary?; @delta.binary; end

          def old_file; @delta.old_file; end

          def new_file; @delta.new_file; end
        end

        def initialize(rugged_diff)
          @diff = rugged_diff
        end

        def each_delta(&block)
          @diff.each_delta.map do |delta|
            Delta.new(delta)
          end.each(&block)
        end
      end

      GIT_ATTR_OPTS = { :priority => [:index], :skip_system => true }
      GIT_ATTR_FLAGS = Rugged::Repository::Attributes.parse_opts(GIT_ATTR_OPTS)

      def initialize(rugged)
        @rugged = rugged
        @tree_map = {}
        @attr_source = nil
      end

      def get_tree_size(commit_id, limit)
        get_tree(commit_id).count_recursive(limit)
      end

      def set_attribute_source(commit_id)
        tree = get_tree(commit_id)
        return if @attr_source == tree

        @attr_source = tree
        attr_index = Rugged::Index.new
        attr_index.read_tree(@attr_source)
        @rugged.index = attr_index
      end

      def load_attributes_for_path(path, attr_names)
        @rugged.fetch_attributes(path, attr_names, GIT_ATTR_FLAGS)
      end

      def load_blob(blob_id, max_size)
        Rugged::Blob.to_buffer(@rugged, blob_id, max_size)
      end

      def diff(old_commit, new_commit)
        old_tree = old_commit.nil? ? nil : get_tree(old_commit)
        new_tree = new_commit.nil? ? nil : get_tree(new_commit)

        Diff.new(Rugged::Tree.diff(@rugged, old_tree, new_tree))
      end

      # Internal: get the Rugged::Tree associated with a given commit ID. This
      # method should not be used outside of Linguist itself and is subject to
      # change or be removed.
      #
      # commit_id - the object ID of the commit whose tree instance we want.
      #
      # Returns the Rugged::Tree of the specified commit.
      def get_tree(commit_id)
        tree = @tree_map[commit_id]
        return tree if tree

        @tree_map[commit_id] = Rugged::Commit.lookup(@rugged, commit_id).tree
        @tree_map[commit_id]
      end

      def method_missing(method_name, *args, &block)
        @rugged.send(method_name, *args, &block)
      end
    end
  end
end
