require 'linguist/mime'

require 'test/unit'

class TestMime < Test::Unit::TestCase
  include Linguist

  def test_lookup
    assert_equal 'application/ruby', Mime.lookup(".rb")
    assert_equal 'application/javascript', Mime.lookup(".js")
    assert_equal 'application/python', Mime.lookup(".py")

    assert_equal 'text/plain; charset=utf-8', Mime.lookup(".kt")
    assert_equal 'text/plain; charset=utf-8', Mime.lookup(".html")
    assert_equal 'text/plain; charset=utf-8', Mime.lookup(".sh")
    assert_equal 'text/plain; charset=utf-8', Mime.lookup(".latex")

    assert_equal 'application/octet-stream', Mime.lookup(".dmg")
  end
end
