require 'digest/md5'

module Linguist
  module MD5
    # Public: Create deep nested digest of value object.
    #
    # Useful for object comparison.
    #
    # obj - Object to digest.
    #
    # Returns String hex digest
    def self.hexdigest(obj)
      digest = Digest::MD5.new

      case obj
      when String, Symbol, Integer
        digest.update "#{obj.class}"
        digest.update "#{obj}"
      when TrueClass, FalseClass, NilClass
        digest.update "#{obj.class}"
      when Array
        digest.update "#{obj.class}"
        for e in obj
          digest.update(hexdigest(e))
        end
      when Hash
        digest.update "#{obj.class}"
        for e in obj.map { |(k, v)| hexdigest([k, v]) }.sort
          digest.update(e)
        end
      else
        raise TypeError, "can't convert #{obj.inspect} into String"
      end

      digest.hexdigest
    end
  end
end
