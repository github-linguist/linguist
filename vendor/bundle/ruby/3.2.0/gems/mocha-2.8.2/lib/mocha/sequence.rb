module Mocha
  # Used to constrain the order in which expectations can occur.
  #
  # @see API#sequence
  # @see Expectation#in_sequence
  class Sequence
    # @private
    class InSequenceOrderingConstraint
      def initialize(sequence, index)
        @sequence = sequence
        @index = index
      end

      def allows_invocation_now?
        @sequence.satisfied_to_index?(@index)
      end

      def mocha_inspect
        "in sequence #{@sequence.mocha_inspect}"
      end
    end

    # @private
    def initialize(name)
      @name = name
      @expectations = []
    end

    # @private
    def constrain_as_next_in_sequence(expectation)
      index = @expectations.length
      @expectations << expectation
      expectation.add_ordering_constraint(InSequenceOrderingConstraint.new(self, index))
    end

    # @private
    def satisfied_to_index?(index)
      @expectations[0...index].all?(&:satisfied?)
    end

    # @private
    def mocha_inspect
      @name.mocha_inspect.to_s
    end
  end
end
