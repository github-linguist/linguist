

module XXhash
  module XXhashInternal
    class XXhash32
      @@mem_total_size = 16
      @@prime32_1 = 2654435761
      @@prime32_2 = 2246822519
      @@prime32_3 = 3266489917
      @@prime32_4 = 668265263
      @@prime32_5 = 374761393

      @@thirtytwo1s = (2**32-1)

      def initialize seed
        @seed = seed
        reset
      end

      def reset
        @v1 = @seed + @@prime32_1 + @@prime32_2
        @v2 = @seed + @@prime32_2
        @v3 = @seed + 0
        @v4 = @seed - @@prime32_1
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
            p32 = uint32(
              @memory[i] |
                (@memory[i+1] << 8) |
                (@memory[i+2] << 16) |
                (@memory[i+3] << 24))

            v = uint32(self.send(m) + p32 * @@prime32_2)
            v = uint32(uint32((v << 13) | (v >> (32 - 13))) * @@prime32_1)
            self.send((m.to_s + "=").to_sym, v)
            i += 4
          end

          p += mem_avail
          @memsize = 0
        end

        return true
      end

      def digest val=nil
        if val
          update val
        end

        if @total_len >= 16
          h32 = ((@v1 << 1) | (@v1 >> (32 - 1))) +
            ((@v2 << 7) | (@v2 >> (32 - 7))) +
            ((@v3 << 12) | (@v3 >> (32 - 12))) +
            ((@v4 << 18) | (@v4 >> (32 - 18)))
        else
          h32 = @seed + @@prime32_5
        end

        h32 = uint32(h32 + @total_len)

        p = 0
        while p <= (@memsize - 4)
          p32 = uint32(@memory[p] |
                         (@memory[p+1] << 8) |
                         (@memory[p+2] << 16) |
                         (@memory[p+3] << 24))
          h32 = uint32(h32 + p32 * @@prime32_3)
          h32 = uint32(uint32((h32 << 17) | (h32 >> (32 - 17))) * @@prime32_4)
          p += 4
        end

        while p < @memsize
          h32 = uint32(h32 + @memory[p] * @@prime32_5)
          h32 = uint32(uint32((h32 << 11) | (h32 >> (32 - 11))) * @@prime32_1)
          p += 1
        end

        h32 ^= h32 >> 15
        h32 = uint32(h32 * @@prime32_2)
        h32 ^= h32 >> 13
        h32 = uint32(h32 * @@prime32_3)
        h32 ^= h32 >> 16

        h32
      end

    private

      attr_accessor :v1, :v2, :v3, :v4

      def uint32(x)
        x & @@thirtytwo1s
      end
    end
  end
end
