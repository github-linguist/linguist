require 'linguist/mime'

require 'test/unit'

class TestMime < Test::Unit::TestCase
  include Linguist

  def test_mime
    assert_equal 'text/plain', Mime.mime_for(nil)

    assert_equal 'application/ruby', Mime.mime_for(".rb")
    assert_equal 'application/javascript', Mime.mime_for(".js")
    assert_equal 'application/python', Mime.mime_for(".py")

    assert_equal 'text/plain', Mime.mime_for(".kt")
    assert_equal 'text/html', Mime.mime_for(".html")
    assert_equal 'application/sh', Mime.mime_for(".sh")
    assert_equal 'application/latex', Mime.mime_for(".latex")

    assert_equal 'application/shockwave-flash', Mime.mime_for(".swf")

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

    assert_equal 'application/octet-stream', Mime.content_type_for(".swf")
    assert_equal 'application/octet-stream', Mime.content_type_for(".dmg")
    assert_equal 'application/octet-stream', Mime.content_type_for(".exe")
    assert_equal 'application/octet-stream', Mime.content_type_for(".dll")

    assert_equal 'application/java-archive', Mime.content_type_for(".jar")
    assert_equal 'application/java-archive', Mime.content_type_for(".ear")
    assert_equal 'application/java-archive', Mime.content_type_for(".war")
  end

  def test_binary
    assert Mime.binary?("application/octet-stream")
    assert !Mime.binary?("text/plain")

    assert Mime.binary?("image/gif")
    assert Mime.binary?("image/jpeg")
    assert Mime.binary?("image/png")
    assert Mime.binary?("java-archive")
    assert Mime.binary?("x-shockwave-flash")

    assert !Mime.binary?("application/atom+xml")
    assert !Mime.binary?("application/javascript")
    assert !Mime.binary?("application/json")
    assert !Mime.binary?("application/rdf+xml")
    assert !Mime.binary?("application/sh")
    assert !Mime.binary?("application/x-perl")
    assert !Mime.binary?("application/x-python")
    assert !Mime.binary?("application/x-ruby")

    assert !Mime.binary?(".ms")
    assert !Mime.binary?(".nc")
    assert !Mime.binary?(".src")
    assert !Mime.binary?(".xul")
  end

  def test_attachment
    assert Mime.attachment?("application/octet-stream")
    assert !Mime.attachment?("text/plain")

    assert Mime.attachment?("application/java-archive")
    assert Mime.attachment?("application/ogg")
    assert Mime.attachment?("application/pdf")
    assert Mime.attachment?("application/x-gzip")
    assert Mime.attachment?("application/zip")
    assert Mime.attachment?("audio/mp4")

    assert Mime.attachment?(".a")
    assert Mime.attachment?(".air")
    assert Mime.attachment?(".blend")
    assert Mime.attachment?(".crx")
    assert Mime.attachment?(".deb")
    assert Mime.attachment?(".dmg")
    assert Mime.attachment?(".exe")
    assert Mime.attachment?(".gem")
    assert Mime.attachment?(".graffle")
    assert Mime.attachment?(".gz")
    assert Mime.attachment?(".icns")
    assert Mime.attachment?(".ipa")
    assert Mime.attachment?(".lib")
    assert Mime.attachment?(".mcz")
    assert Mime.attachment?(".mov")
    assert Mime.attachment?(".mp3")
    assert Mime.attachment?(".nib")
    assert Mime.attachment?(".o")
    assert Mime.attachment?(".odp")
    assert Mime.attachment?(".ods")
    assert Mime.attachment?(".odt")
    assert Mime.attachment?(".ogg")
    assert Mime.attachment?(".ogv")
    assert Mime.attachment?(".otf")
    assert Mime.attachment?(".pfx")
    assert Mime.attachment?(".pigx")
    assert Mime.attachment?(".plgx")
    assert Mime.attachment?(".pptx")
    assert Mime.attachment?(".psd")
    assert Mime.attachment?(".sib")
    assert Mime.attachment?(".so")
    assert Mime.attachment?(".spl")
    assert Mime.attachment?(".sqlite3")
    assert Mime.attachment?(".swc")
    assert Mime.attachment?(".swf")
    assert Mime.attachment?(".tar")
    assert Mime.attachment?(".ucode")
    assert Mime.attachment?(".xpi")
    assert Mime.attachment?(".zip")

    assert !Mime.attachment?("application/atom+xml")
    assert !Mime.attachment?("application/javascript")
    assert !Mime.attachment?("application/json")
    assert !Mime.attachment?("application/rdf+xml")
    assert !Mime.attachment?("application/sh")
    assert !Mime.attachment?("application/xhtml+xml")
    assert !Mime.attachment?("application/xml")
    assert !Mime.attachment?("image/gif")
    assert !Mime.attachment?("image/jpeg")
    assert !Mime.attachment?("image/png")
    assert !Mime.attachment?("text/css")
    assert !Mime.attachment?("text/csv")
    assert !Mime.attachment?("text/html")
    assert !Mime.attachment?("text/javascript")
    assert !Mime.attachment?("text/plain")

    assert !Mime.attachment?(".gif")
    assert !Mime.attachment?(".jpeg")
    assert !Mime.attachment?(".jpg")
    assert !Mime.attachment?(".js")
    assert !Mime.attachment?(".latex")
    assert !Mime.attachment?(".ms")
    assert !Mime.attachment?(".nc")
    assert !Mime.attachment?(".pl")
    assert !Mime.attachment?(".png")
    assert !Mime.attachment?(".ps")
    assert !Mime.attachment?(".py")
    assert !Mime.attachment?(".rb")
    assert !Mime.attachment?(".sh")
    assert !Mime.attachment?(".src")
    assert !Mime.attachment?(".tcl")
    assert !Mime.attachment?(".texi")
    assert !Mime.attachment?(".texinfo")
    assert !Mime.attachment?(".xul")
  end
end
