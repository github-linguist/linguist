module Linguist
  class Pathname
    def initialize(path)
      @path = path
    end

    def to_s
      @path.dup
    end
  end
end
