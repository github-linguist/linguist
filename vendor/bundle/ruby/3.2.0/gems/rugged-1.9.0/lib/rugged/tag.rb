# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

module Rugged
  class Tag < Rugged::Reference
    GPG_SIGNATURE_PREFIX = "-----BEGIN PGP SIGNATURE-----".freeze

    def self.extract_signature(repo, oid, prefix=GPG_SIGNATURE_PREFIX)
      object = repo.read(oid)

      unless object.type == :tag
        raise GitRPC::InvalidObject, "Invalid object type #{object.type}, expected tag"
      end

      if index = object.data.index(prefix)
        [
          object.data.byteslice(index..-1),
          object.data.byteslice(0...index)
        ]
      else
        nil
      end
    end

    def name
      canonical_name.sub(%r{^refs/tags/}, "")
    end

    class Annotation
      def self.prettify_message(msg, strip_comments = true)
        Rugged::prettify_message(msg, strip_comments)
      end

      def inspect
        "#<Rugged::Tag::Annotation:#{object_id} {name: #{name.inspect}, message: #{message.inspect}, target: #{target.inspect}>"
      end

      def to_hash
        {
          :message => message,
          :name => name,
          :target => target,
          :tagger => tagger,
        }
      end

      def modify(new_args, force=True)
        args = self.to_hash.merge(new_args)
        Tag.create(args, force)
      end
    end
  end
end
