# frozen-string-literal: true

require 'forwardable'

module Tomlrb
  class LocalDate
    extend Forwardable

    def_delegators :@time, :year, :month, :day

    def initialize(year, month, day)
      @time = Time.utc(year, month, day, 0, 0, 0)
      raise ArgumentError, "Invalid Local Date: #{year}-#{month}-#{day}" unless day.to_i == @time.day && month.to_i == @time.month && year.to_i == @time.year
    end

    # @param offset see {LocalDateTime#to_time}
    # @return [Time] 00:00:00 of the date
    def to_time(offset = '-00:00')
      return @time if offset == '-00:00'
      Time.new(year, month, day, 0, 0, 0, offset)
    end

    def to_s
      @time.strftime('%F')
    end

    def ==(other)
      other.is_a?(self.class) &&
        to_time == other.to_time
    end

    def inspect
      "#<#{self.class}: #{self}>"
    end
  end
end
