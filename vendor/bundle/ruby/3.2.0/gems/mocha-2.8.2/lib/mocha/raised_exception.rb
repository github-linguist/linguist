module Mocha
  class RaisedException
    def initialize(exception)
      @exception = exception
    end

    def mocha_inspect
      "raised #{@exception}"
    end
  end
end
