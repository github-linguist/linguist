require 'linguist/mime'

require 'test/unit'

class TestMime < Test::Unit::TestCase
  include Linguist

  def test_mime
    assert_equal 'text/plain', Mime.mime_for(nil)

    assert_equal 'text/plain', Mime.mime_for(".rb")
    assert_equal 'text/plain', Mime.mime_for(".js")
    assert_equal 'text/plain', Mime.mime_for(".py")

    assert_equal 'text/plain', Mime.mime_for(".kt")
    assert_equal 'text/html', Mime.mime_for(".html")
    assert_equal 'application/sh', Mime.mime_for(".sh")
    assert_equal 'application/latex', Mime.mime_for(".latex")

    assert_equal 'application/octet-stream', Mime.mime_for(".dmg")
    assert_equal 'application/octet-stream', Mime.mime_for(".exe")
    assert_equal 'application/octet-stream', Mime.mime_for(".dll")

    assert_equal 'application/java-archive', Mime.mime_for(".jar")
    assert_equal 'application/java-archive', Mime.mime_for(".ear")
    assert_equal 'application/java-archive', Mime.mime_for(".war")
  end

  def test_content_type
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(nil)

    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".rb")
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".js")
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".py")

    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".kt")
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".html")
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".sh")
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".latex")

    assert_equal 'application/octet-stream', Mime.content_type_for(".dmg")
    assert_equal 'application/octet-stream', Mime.content_type_for(".exe")
    assert_equal 'application/octet-stream', Mime.content_type_for(".dll")

    assert_equal 'application/java-archive', Mime.content_type_for(".jar")
    assert_equal 'application/java-archive', Mime.content_type_for(".ear")
    assert_equal 'application/java-archive', Mime.content_type_for(".war")
  end
end
