# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class Repository
    def attributes(path, options = {})
      Attributes.new(self, path, options)
    end

    class Attributes
      include Enumerable

      LOAD_PRIORITIES = {
        [:file, :index] => 0,
        [:index, :file] => 1,
        [:index] => 2,
      }

      def self.parse_opts(opt)
        flags = LOAD_PRIORITIES[opt[:priority]] || 0
        flags |= 4 if opt[:skip_system]
        flags
      end

      def initialize(repository, path, options = {})
        @repository = repository
        @path = path
        @load_flags = Attributes.parse_opts(options)
      end

      def [](attribute)
        @repository.fetch_attributes(@path, attribute, @load_flags)
      end

      def to_h
        @hash ||= @repository.fetch_attributes(@path, nil, @load_flags)
      end

      def each(&block)
        to_h.each(&block)
      end
    end
  end
end
