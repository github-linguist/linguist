require 'linguist/mime'

require 'test/unit'

class TestMime < Test::Unit::TestCase
  include Linguist

  def test_lookup
    assert_equal 'text/plain; charset=utf-8', Mime.lookup(".rb")
    assert_equal 'text/plain; charset=utf-8', Mime.lookup(".js")
    assert_equal 'text/plain; charset=utf-8', Mime.lookup(".py")

    assert_equal 'text/plain; charset=utf-8', Mime.lookup(".kt")
    assert_equal 'text/plain; charset=utf-8', Mime.lookup(".html")
    assert_equal 'text/plain; charset=utf-8', Mime.lookup(".sh")
    assert_equal 'text/plain; charset=utf-8', Mime.lookup(".latex")

    assert_equal 'application/octet-stream', Mime.lookup(".dmg")

    assert_equal 'application/java-archive', Mime.lookup(".jar")
    assert_equal 'application/java-archive', Mime.lookup(".ear")
    assert_equal 'application/java-archive', Mime.lookup(".war")
  end
end
