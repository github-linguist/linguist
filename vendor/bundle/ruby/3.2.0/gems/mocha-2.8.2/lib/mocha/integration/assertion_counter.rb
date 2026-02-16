module Mocha
  module Integration
    class AssertionCounter
      def initialize(test_case)
        @test_case = test_case
      end

      def increment
        @test_case.assert(true)
      end
    end
  end
end
