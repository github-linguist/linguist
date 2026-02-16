require "ruby-xxhash/version"

module XXhash
  module XXhashInternal
    class XXhash64
      @@mem_total_size = 32
      @@prime64_1 = 11400714785074694791
      @@prime64_2 = 14029467366897019727
      @@prime64_3 = 1609587929392839161
      @@prime64_4 = 9650029242287828579
      @@prime64_5 = 2870177450012600261

      @@sixtyfour1s = (2**64-1)

      def initialize seed
        @seed = seed
        reset
      end

      def reset
        @v1 = @seed + @@prime64_1 + @@prime64_2;
        @v2 = @seed + @@prime64_2;
        @v3 = @seed + 0;
        @v4 = @seed - @@prime64_1;
        @total_len = 0
        @memory = Array.new(@@mem_total_size)
        @memsize = 0
      end

      def update bytes
        if String === bytes
          bytes = bytes.unpack("C*")
        end

        @total_len += bytes.length

        p = 0
        while (remaining = (bytes.length - p)) > 0
          mem_avail = @@mem_total_size - @memsize

          if(remaining < mem_avail)
            @memory[@memsize, remaining] = bytes[p, remaining]
            @memsize += remaining
            break
          end

          @memory[@memsize, mem_avail] = bytes[p, mem_avail]

          i = 0
          [:v1, :v2, :v3, :v4].each do |m|
            p64 = uint64(
              @memory[i] |
                (@memory[i+1] << 8) |
                (@memory[i+2] << 16) |
                (@memory[i+3] << 24) |
                (@memory[i+4] << 32) |
                (@memory[i+5] << 40) |
                (@memory[i+6] << 48) |
                (@memory[i+7] << 56))

            v = uint64(self.send(m) + p64 * @@prime64_2)
            v = uint64(uint64((v << 31) | (v >> (64 - 31))) * @@prime64_1)
            self.send((m.to_s + "=").to_sym, v)
            i += 8
          end

          p += mem_avail
          @memsize = 0
        end
      end

      def digest val=nil
        if val
          update val
        end

        if @total_len >= 32
          h64 = ((@v1 << 1) | (@v1 >> (64 - 1))) +
            ((@v2 << 7) | (@v2 >> (64 - 7))) +
            ((@v3 << 12) | (@v3 >> (64 - 12))) +
            ((@v4 << 18) | (@v4 >> (64 - 18)))

          [:v1, :v2, :v3, :v4].each do |m|
            v = uint64(self.send(m) * @@prime64_2)
            v = uint64((v << 31) | (v >> (64 - 31)))
            h64 ^= uint64(v * @@prime64_1)
            h64 = h64 * @@prime64_1 + @@prime64_4
          end
        else
          h64 = @seed + @@prime64_5
        end

        h64 = uint64(h64 + @total_len)

        i = 0
        while i <= (@memsize - 8)
          v = uint64(
            @memory[i] |
              (@memory[i+1] << 8) |
              (@memory[i+2] << 16) |
              (@memory[i+3] << 24) |
              (@memory[i+4] << 32) |
              (@memory[i+5] << 40) |
              (@memory[i+6] << 48) |
              (@memory[i+7] << 56))

          v = uint64(v * @@prime64_2)
          h64 ^= uint64(uint64((v << 31) | (v >> (64 - 31))) * @@prime64_1)
          h64 = uint64(uint64((h64 << 27) | (h64 >> (64 - 27))) * @@prime64_1 + @@prime64_4)
          i += 8
        end

        if i <= (@memsize - 4)
          v = @memory[i] |
            (@memory[i+1] << 8) |
            (@memory[i+2] << 16) |
            (@memory[i+3] << 24)
          h64 ^= uint64(v * @@prime64_1)
          h64 = uint64(uint64((h64 << 23) | (h64 >> (64-23))) * @@prime64_2 + @@prime64_3)
          i += 4
        end

        while i < @memsize
          h64 ^= uint64(@memory[i] * @@prime64_5)
          h64 = uint64(uint64((h64 << 11) | (h64 >> (64 - 11))) * @@prime64_1)
          i += 1
        end

        h64 ^= h64 >> 33
        h64 = uint64(h64 * @@prime64_2)
        h64 ^= h64 >> 29
        h64 = uint64(h64 * @@prime64_3)
        h64 ^= h64 >> 32

        h64
      end

    private

      attr_accessor :v1, :v2, :v3, :v4

      def uint64(x)
        x & @@sixtyfour1s
      end
    end
  end
end