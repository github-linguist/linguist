# Copyright 2014 Justin W. Smith

require "ruby-xxhash/version"
require "digest"
require "ruby-xxhash32"
require "ruby-xxhash64"


module XXhash
  def self.xxh32(input, seed = 0)
    xxh = XXhashInternal::XXhash32.new(seed)
    xxh.update(input)
    xxh.digest
  end

  def self.xxh32_stream(io, seed = 0, chunk = 32)
    xxh = XXhashInternal::XXhash32.new(seed)

    while(data = io.read(chunk))
      xxh.update(data)
    end

    xxh.digest
  end

  def self.xxh64(input, seed = 0)
    xxh = XXhashInternal::XXhash64.new(seed)
    xxh.update(input)
    xxh.digest
  end

  def self.xxh64_stream(io, seed = 0, chunk = 32)
    xxh = XXhashInternal::XXhash64.new(seed)

    while(data = io.read(chunk))
      xxh.update(data)
    end

    xxh.digest
  end

end


module Digest
  class XXHash < Digest::Class
    attr_reader :digest_length
    def initialize bitlen, seed = 0
      case bitlen
        when 32
          @hash = XXhash::XXhashInternal::XXhash32.new(seed)
        when 64
          @hash = XXhash::XXhashInternal::XXhash64.new(seed)
        else
          raise ArgumentError, "Unsupported bit length: %s" % bitlen.inspect
      end
      @digest_length = bitlen
    end

    def update chunk
      @hash.update(chunk)
    end

    def digest val=nil
      if val
        @hash.update val
      end

      @hash.digest
    end

    def digest! val=nil
      result = digest(val)
      @hash.reset
      result
    end

    def reset
      @hash.reset
    end

  end

  class XXHash32 < Digest::XXHash
    def initialize seed = 0
      super(32, seed)
    end
  end

  class XXHash64 < Digest::XXHash
    def initialize seed = 0
      super(64, seed)
    end
  end

end
