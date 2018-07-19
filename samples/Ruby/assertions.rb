require "test/unit/assertions"
include Test::Unit::Assertions

n = 5
begin
  assert_equal(42, n)
rescue Exception => e
  # Ruby 1.8: e is a Test::Unit::AssertionFailedError
  # Ruby 1.9: e is a MiniTest::Assertion
  puts e
end
