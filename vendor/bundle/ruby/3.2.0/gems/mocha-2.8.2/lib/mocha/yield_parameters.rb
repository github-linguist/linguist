module Mocha
  class YieldParameters
    def initialize
      @parameter_groups = []
    end

    def next_invocation
      case @parameter_groups.length
      when 0 then []
      when 1 then @parameter_groups.first
      else @parameter_groups.shift
      end
    end

    def add(*parameter_groups)
      @parameter_groups << parameter_groups.map do |pg|
        pg.is_a?(Array) ? pg : [pg]
      end
    end
  end
end
