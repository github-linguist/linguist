module Mocha
  # Matcher classes used as parameters for {Expectation#with} to restrict the parameter values which will match the expectation. Can be nested. Build matcher instances in tests using methods in {Methods}, e.g. {Methods#includes}.
  module ParameterMatchers
    # These methods build instances of the {ParameterMatchers} classes which are used with {Expectation#with} to restrict the parameter values. Can be nested, e.g. see {Methods#all_of} examples.
    module Methods; end
  end
end

require 'mocha/parameter_matchers/deprecations'
require 'mocha/parameter_matchers/instance_methods'

require 'mocha/parameter_matchers/all_of'
require 'mocha/parameter_matchers/any_of'
require 'mocha/parameter_matchers/any_parameters'
require 'mocha/parameter_matchers/anything'
require 'mocha/parameter_matchers/equals'
require 'mocha/parameter_matchers/has_entry'
require 'mocha/parameter_matchers/has_entries'
require 'mocha/parameter_matchers/has_key'
require 'mocha/parameter_matchers/has_keys'
require 'mocha/parameter_matchers/has_value'
require 'mocha/parameter_matchers/includes'
require 'mocha/parameter_matchers/instance_of'
require 'mocha/parameter_matchers/is_a'
require 'mocha/parameter_matchers/kind_of'
require 'mocha/parameter_matchers/not'
require 'mocha/parameter_matchers/optionally'
require 'mocha/parameter_matchers/regexp_matches'
require 'mocha/parameter_matchers/responds_with'
require 'mocha/parameter_matchers/yaml_equivalent'
require 'mocha/parameter_matchers/equivalent_uri'
