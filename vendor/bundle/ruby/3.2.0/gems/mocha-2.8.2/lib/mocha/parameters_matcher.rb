require 'mocha/inspect'
require 'mocha/parameter_matchers'

module Mocha
  class ParametersMatcher
    def initialize(expected_parameters = [ParameterMatchers::AnyParameters.new], expectation = nil, &matching_block)
      @expected_parameters = expected_parameters
      @expectation = expectation
      @matching_block = matching_block
    end

    def match?(actual_parameters = [])
      if @matching_block
        @matching_block.call(*actual_parameters)
      else
        parameters_match?(actual_parameters)
      end
    end

    def parameters_match?(actual_parameters)
      matchers.all? { |matcher| matcher.matches?(actual_parameters) } && actual_parameters.empty?
    end

    def mocha_inspect
      if @matching_block
        '(arguments_accepted_by_custom_matching_block)'
      else
        signature = matchers.mocha_inspect
        signature = signature.gsub(/^\[|\]$/, '')
        "(#{signature})"
      end
    end

    def matchers
      @expected_parameters.map { |p| p.to_matcher(expectation: @expectation, top_level: true) }
    end
  end
end
