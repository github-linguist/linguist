require 'linguist/mime'

require 'test/unit'

class TestMime < Test::Unit::TestCase
  include Linguist

  def test_extension_lookup
    # Default to plain text if we have no idea.
    assert_equal 'text/plain', Mime.mime_for(nil)
    assert_equal 'text/plain', Mime.mime_for('')

    # Add an assertion to this list if you add/change any extensions
    # in mimes.yml. Its still useful to test even trivial cases since
    # MIME::Type's extension lookup may return multiple matches and we
    # only pick one of them. Please keep this list alphabetized.
    assert_equal 'application/chrome-extension', Mime.mime_for('.crx')
    assert_equal 'application/debian-package', Mime.mime_for('.deb')
    assert_equal 'application/java-archive', Mime.mime_for('.ear')
    assert_equal 'application/java-archive', Mime.mime_for('.jar')
    assert_equal 'application/java-archive', Mime.mime_for('.war')
    assert_equal 'application/javascript', Mime.mime_for('.js')
    assert_equal 'application/latex', Mime.mime_for('.latex')
    assert_equal 'application/ms-xbap', Mime.mime_for('.xbap')
    assert_equal 'application/octet-stream', Mime.mime_for('.dll')
    assert_equal 'application/octet-stream', Mime.mime_for('.dmg')
    assert_equal 'application/octet-stream', Mime.mime_for('.exe')
    assert_equal 'application/perl', Mime.mime_for('.pl')
    assert_equal 'application/perl', Mime.mime_for('.pm')
    assert_equal 'application/postscript', Mime.mime_for('.ai')
    assert_equal 'application/postscript', Mime.mime_for('.eps')
    assert_equal 'application/postscript', Mime.mime_for('.ps')
    assert_equal 'application/python', Mime.mime_for('.py')
    assert_equal 'application/ruby', Mime.mime_for('.rb')
    assert_equal 'application/sh', Mime.mime_for('.sh')
    assert_equal 'application/shockwave-flash', Mime.mime_for('.swf')
    assert_equal 'application/silverlight-app', Mime.mime_for('.xap')
    assert_equal 'application/supercollider', Mime.mime_for('.sc')
    assert_equal 'application/vnd.adobe.air-application-installer-package+zip', Mime.mime_for('.air')
    assert_equal 'application/vnd.oasis.opendocument.presentation', Mime.mime_for('.odp')
    assert_equal 'application/vnd.oasis.opendocument.spreadsheet', Mime.mime_for('.ods')
    assert_equal 'application/vnd.oasis.opendocument.text', Mime.mime_for('.odt')
    assert_equal 'application/vnd.openofficeorg.extension', Mime.mime_for('.oxt')
    assert_equal 'application/vnd.openxmlformats-officedocument.presentationml.presentation', Mime.mime_for('.pptx')
    assert_equal 'application/xaml+xml', Mime.mime_for('.xaml')
    assert_equal 'text/cache-manifest', Mime.mime_for('.manifest')
    assert_equal 'text/html', Mime.mime_for('.html')
    assert_equal 'text/nimrod', Mime.mime_for('.nim')
    assert_equal 'text/plain', Mime.mime_for('.kt')
    assert_equal 'video/quicktime', Mime.mime_for('.mov')
  end

  def test_binary
    assert Mime.binary?('application/octet-stream')

    # Add an assertion for any binary mime types added to mimes.yml.
    # Please keep this list alphabetized.
    assert Mime.binary?('application/java-archive')
    assert Mime.binary?('application/ogg')
    assert Mime.binary?('application/pdf')
    assert Mime.binary?('application/postscript')
    assert Mime.binary?('application/x-gzip')
    assert Mime.binary?('application/x-shockwave-flash')
    assert Mime.binary?('application/x-silverlight-app')
    assert Mime.binary?('application/zip')
    assert Mime.binary?('audio/mp4')
    assert Mime.binary?('image/gif')
    assert Mime.binary?('image/jpeg')
    assert Mime.binary?('image/png')

    # Legacy. Prefer testing mime types instead of extensions.
    assert Mime.binary?('.a')
    assert Mime.binary?('.air')
    assert Mime.binary?('.blend')
    assert Mime.binary?('.crx')
    assert Mime.binary?('.deb')
    assert Mime.binary?('.dmg')
    assert Mime.binary?('.exe')
    assert Mime.binary?('.gem')
    assert Mime.binary?('.graffle')
    assert Mime.binary?('.gz')
    assert Mime.binary?('.icns')
    assert Mime.binary?('.ipa')
    assert Mime.binary?('.lib')
    assert Mime.binary?('.mcz')
    assert Mime.binary?('.mov')
    assert Mime.binary?('.mp3')
    assert Mime.binary?('.nib')
    assert Mime.binary?('.o')
    assert Mime.binary?('.odp')
    assert Mime.binary?('.ods')
    assert Mime.binary?('.odt')
    assert Mime.binary?('.ogg')
    assert Mime.binary?('.ogv')
    assert Mime.binary?('.otf')
    assert Mime.binary?('.pfx')
    assert Mime.binary?('.pigx')
    assert Mime.binary?('.plgx')
    assert Mime.binary?('.pptx')
    assert Mime.binary?('.psd')
    assert Mime.binary?('.sib')
    assert Mime.binary?('.so')
    assert Mime.binary?('.spl')
    assert Mime.binary?('.sqlite3')
    assert Mime.binary?('.swc')
    assert Mime.binary?('.swf')
    assert Mime.binary?('.tar')
    assert Mime.binary?('.ucode')
    assert Mime.binary?('.xpi')
    assert Mime.binary?('.zip')
  end

  def test_text
    # By default, assume the plain text.
    assert Mime.text?(nil)
    assert Mime.text?('')

    assert Mime.text?('text/plain')

    # Add an assertion for any text mime types added to mimes.yml.
    # Please keep this list alphabetized.
    assert Mime.text?('application/atom+xml')
    assert Mime.text?('application/javascript')
    assert Mime.text?('application/json')
    assert Mime.text?('application/perl')
    assert Mime.text?('application/rdf+xml')
    assert Mime.text?('application/sh')
    assert Mime.text?('application/x-ms-xbap')
    assert Mime.text?('application/x-perl')
    assert Mime.text?('application/x-python')
    assert Mime.text?('application/x-ruby')
    assert Mime.text?('application/xaml+xml')
    assert Mime.text?('application/xhtml+xml')
    assert Mime.text?('application/xml')
    assert Mime.text?('text/cache-manifest')
    assert Mime.text?('text/css')
    assert Mime.text?('text/csv')
    assert Mime.text?('text/html')
    assert Mime.text?('text/javascript')
    assert Mime.text?('text/plain')
    assert Mime.text?('text/x-nimrod')

    # Legacy. Prefer testing mime types instead of extensions.
    assert Mime.text?('.js')
    assert Mime.text?('.latex')
    assert Mime.text?('.ms')
    assert Mime.text?('.nc')
    assert Mime.text?('.pl')
    assert Mime.text?('.pm')
    assert Mime.text?('.py')
    assert Mime.text?('.rb')
    assert Mime.text?('.sc')
    assert Mime.text?('.sh')
    assert Mime.text?('.src')
    assert Mime.text?('.tcl')
    assert Mime.text?('.texi')
    assert Mime.text?('.texinfo')
    assert Mime.text?('.xul')
  end
end
