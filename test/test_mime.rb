require 'linguist/mime'

require 'test/unit'

class TestMime < Test::Unit::TestCase
  include Linguist

  def test_mime
    assert_equal 'text/plain', Mime.mime_for(nil)
    assert_equal 'text/plain', Mime.mime_for("")

    assert_equal 'application/ruby', Mime.mime_for(".rb")
    assert_equal 'application/javascript', Mime.mime_for(".js")
    assert_equal 'application/python', Mime.mime_for(".py")

    assert_equal 'text/plain', Mime.mime_for(".kt")
    assert_equal 'text/html', Mime.mime_for(".html")
    assert_equal 'text/cache-manifest', Mime.mime_for(".manifest")
    assert_equal 'application/sh', Mime.mime_for(".sh")
    assert_equal 'application/latex', Mime.mime_for(".latex")

    assert_equal 'application/vnd.adobe.air-application-installer-package+zip',
      Mime.mime_for(".air")
    assert_equal 'application/shockwave-flash', Mime.mime_for(".swf")

    assert_equal 'application/chrome-extension', Mime.mime_for(".crx")
    assert_equal 'application/debian-package', Mime.mime_for(".deb")

    assert_equal 'video/quicktime', Mime.mime_for(".mov")

    assert_equal 'application/octet-stream', Mime.mime_for(".dmg")
    assert_equal 'application/octet-stream', Mime.mime_for(".exe")
    assert_equal 'application/octet-stream', Mime.mime_for(".dll")

    assert_equal 'application/java-archive', Mime.mime_for(".jar")
    assert_equal 'application/java-archive', Mime.mime_for(".ear")
    assert_equal 'application/java-archive', Mime.mime_for(".war")
  end

  def test_content_type
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(nil)
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for("")

    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".rb")
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".js")
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".py")

    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".kt")
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".html")
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".sh")
    assert_equal 'text/plain; charset=utf-8', Mime.content_type_for(".latex")

    assert_equal 'application/octet-stream', Mime.content_type_for(".air")
    assert_equal 'application/octet-stream', Mime.content_type_for(".dll")
    assert_equal 'application/octet-stream', Mime.content_type_for(".dmg")
    assert_equal 'application/octet-stream', Mime.content_type_for(".exe")
    assert_equal 'application/octet-stream', Mime.content_type_for(".swf")

    assert_equal 'application/java-archive', Mime.content_type_for(".jar")
    assert_equal 'application/java-archive', Mime.content_type_for(".ear")
    assert_equal 'application/java-archive', Mime.content_type_for(".war")
  end

  def test_binary
    assert !Mime.binary?(nil)
    assert !Mime.binary?("")

    assert Mime.binary?("application/octet-stream")
    assert !Mime.binary?("text/plain")

    assert Mime.binary?("application/java-archive")
    assert Mime.binary?("application/ogg")
    assert Mime.binary?("application/pdf")
    assert Mime.binary?("application/x-gzip")
    assert Mime.binary?("application/x-shockwave-flash")
    assert Mime.binary?("application/zip")
    assert Mime.binary?("audio/mp4")
    assert Mime.binary?("image/gif")
    assert Mime.binary?("image/jpeg")
    assert Mime.binary?("image/png")

    assert Mime.binary?(".a")
    assert Mime.binary?(".air")
    assert Mime.binary?(".blend")
    assert Mime.binary?(".crx")
    assert Mime.binary?(".deb")
    assert Mime.binary?(".dmg")
    assert Mime.binary?(".exe")
    assert Mime.binary?(".gem")
    assert Mime.binary?(".graffle")
    assert Mime.binary?(".gz")
    assert Mime.binary?(".icns")
    assert Mime.binary?(".ipa")
    assert Mime.binary?(".lib")
    assert Mime.binary?(".mcz")
    assert Mime.binary?(".mov")
    assert Mime.binary?(".mp3")
    assert Mime.binary?(".nib")
    assert Mime.binary?(".o")
    assert Mime.binary?(".odp")
    assert Mime.binary?(".ods")
    assert Mime.binary?(".odt")
    assert Mime.binary?(".ogg")
    assert Mime.binary?(".ogv")
    assert Mime.binary?(".otf")
    assert Mime.binary?(".pfx")
    assert Mime.binary?(".pigx")
    assert Mime.binary?(".plgx")
    assert Mime.binary?(".pptx")
    assert Mime.binary?(".psd")
    assert Mime.binary?(".sib")
    assert Mime.binary?(".so")
    assert Mime.binary?(".spl")
    assert Mime.binary?(".sqlite3")
    assert Mime.binary?(".swc")
    assert Mime.binary?(".swf")
    assert Mime.binary?(".tar")
    assert Mime.binary?(".ucode")
    assert Mime.binary?(".xpi")
    assert Mime.binary?(".zip")

    assert !Mime.binary?("application/atom+xml")
    assert !Mime.binary?("application/javascript")
    assert !Mime.binary?("application/json")
    assert !Mime.binary?("application/rdf+xml")
    assert !Mime.binary?("application/sh")
    assert !Mime.binary?("application/x-perl")
    assert !Mime.binary?("application/x-python")
    assert !Mime.binary?("application/x-ruby")
    assert !Mime.binary?("application/xhtml+xml")
    assert !Mime.binary?("application/xml")
    assert !Mime.binary?("text/css")
    assert !Mime.binary?("text/csv")
    assert !Mime.binary?("text/html")
    assert !Mime.binary?("text/javascript")
    assert !Mime.binary?("text/plain")

    assert !Mime.binary?(".js")
    assert !Mime.binary?(".latex")
    assert !Mime.binary?(".ms")
    assert !Mime.binary?(".nc")
    assert !Mime.binary?(".pl")
    assert !Mime.binary?(".ps")
    assert !Mime.binary?(".py")
    assert !Mime.binary?(".rb")
    assert !Mime.binary?(".sh")
    assert !Mime.binary?(".src")
    assert !Mime.binary?(".tcl")
    assert !Mime.binary?(".texi")
    assert !Mime.binary?(".texinfo")
    assert !Mime.binary?(".xul")
  end
end
