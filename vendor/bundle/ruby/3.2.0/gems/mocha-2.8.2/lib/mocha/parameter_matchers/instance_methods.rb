require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/equals'
require 'mocha/parameter_matchers/positional_or_keyword_hash'

module Mocha
  module ParameterMatchers
    # @private
    module InstanceMethods
      # @private
      def to_matcher(expectation: nil, top_level: false)
        if is_a?(BaseMethods)
          self
        elsif Hash === self && top_level
          Mocha::ParameterMatchers::PositionalOrKeywordHash.new(self, expectation)
        else
          Mocha::ParameterMatchers::Equals.new(self)
        end
      end
    end
  end
end

# @private
class Object
  include Mocha::ParameterMatchers::InstanceMethods
end
