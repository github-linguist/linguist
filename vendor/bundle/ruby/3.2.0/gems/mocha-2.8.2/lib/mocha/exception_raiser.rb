module Mocha
  class ExceptionRaiser
    def initialize(exception, message)
      @exception = exception
      @message = message
    end

    def evaluate(invocation)
      invocation.raised(@exception)
      raise @exception, @exception.to_s if @exception.is_a?(Module) && (@exception < Interrupt)
      raise @exception, @message if @message
      raise @exception
    end
  end
end
