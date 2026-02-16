# frozen-string-literal: true

require 'forwardable'

module Tomlrb
  class LocalTime
    extend Forwardable

    def_delegators :@time, :hour, :min, :sec, :usec, :nsec

    def initialize(hour, min, sec)
      @time = Time.utc(0, 1, 1, hour, min, sec)
      raise ArgumentError, "Invalid Local Time: #{hour}-#{min}-#{sec}" unless min.to_i == @time.min && hour.to_i == @time.hour
      @sec = sec
    end

    # @param year [Integer]
    # @param month [Integer]
    # @param day [Integer]
    # @param offset see {LocalDateTime#to_time}
    # @return [Time] the time of the date specified by params
    def to_time(year, month, day, offset = '-00:00')
      Time.new(year, month, day, hour, min, @sec, offset)
    end

    def to_s
      frac = (@sec - sec)
      frac_str = frac.zero? ? '' : frac.to_s[1..-1]
      @time.strftime('%T') << frac_str
    end

    def ==(other)
      other.is_a?(self.class) &&
        @time == other.to_time(0, 1, 1)
    end

    def inspect
      "#<#{self.class}: #{self}>"
    end
  end
end
