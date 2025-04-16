require 'linguist/generated'
require 'cgi'
require 'charlock_holmes'
require 'mini_mime'
require 'yaml'

module Linguist
  module Source
    # Diff is an interface representing a diff between two trees. It is composed
    # of a collection of iterable deltas between before/after states of files.
    class Diff
      # A Delta represents a single file's before/after state in a diff.
      class Delta
        # Public: get the status of the file's "after" state as compared to
        # "before". Valid status values include:
        #
        # - :added
        # - :deleted
        # - :modified
        # - :renamed
        # - :copied
        # - :ignored
        # - :untracked
        # - :typechange
        #
        # Returns the status.
        def status
          raise NotImplementedError
        end

        # Public: determine whether the file delta is binary.
        #
        # Returns true if the delta is binary, false otherwise.
        def binary?
          raise NotImplementedError
        end

        # Public: get the metadata of the "before" file in the delta. The
        # metadata is represented as a Hash with the keys:
        #
        # - :path (string)
        # - :oid (string)
        # - :mode (integer)
        #
        # Returns the entry metadata hash.
        def old_file
          raise NotImplementedError
        end

        # Public: get the metadata of the "after" file in the delta. The
        # metadata is represented as a Hash with the keys:
        #
        # - :path (string)
        # - :oid (string)
        # - :mode (integer)
        #
        # Returns the entry metadata hash.
        def new_file
          raise NotImplementedError
        end
      end

      # Public: iterate through each delta of the given diff. Yields a single
      # delta to the given block.
      #
      # Returns nothing.
      def each_delta
        raise NotImplementedError
      end
    end
  end
end
