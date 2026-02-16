require 'mocha/ruby_version'
require 'mocha/integration/test_unit'

unless Mocha::Integration::TestUnit.activate
  raise "Test::Unit must be loaded *before* `require 'mocha/test_unit'`."
end
