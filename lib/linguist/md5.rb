require 'digest/md5'

module Linguist
  module MD5
    # Public: Create deep nested digest of value object.
    #
    # Useful for object comparsion.
    #
    # obj - Object to digest.
    #
    # Returns String hex digest
    def self.hexdigest(obj)
      digest = Digest::MD5.new
      digest_strings(obj).each { |e| digest.update(e) }
      digest.hexdigest
    end

    # Internal: Get String representations for digest.
    #
    # obj - Object to digest
    #
    # Returns an Array of Strings.
    def self.digest_strings(obj)
      case obj
      when String
        ["#{obj.class}", "#{obj}"]
      when Symbol
        ["#{obj.class}", "#{obj}"]
      when Integer
        ["#{obj.class}", "#{obj}"]
      when TrueClass, FalseClass, NilClass
        ["#{obj.class}"]
      when Array
        r = ["#{obj.class}"]
        obj.each do |e|
          r.concat(digest_strings(e))
        end
        r
      when Hash
        r = ["#{obj.class}"]
        obj.map { |k, v| digest_strings([k, v]) }.sort.each do |e|
          r.concat(digest_strings(e))
        end
        r
      else
        raise TypeError, "can't convert #{obj.inspect} into String"
      end
    end
  end
end
