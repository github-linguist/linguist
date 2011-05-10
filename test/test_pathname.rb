require 'linguist/pathname'

require 'test/unit'

class TestPathname < Test::Unit::TestCase
  include Linguist

  def test_to_s
    assert_equal "file.rb", Pathname.new("file.rb").to_s
  end
end
