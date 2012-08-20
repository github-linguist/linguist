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
    assert_equal 'application/javascript', Mime.mime_for('.js')
    assert_equal 'application/octet-stream', Mime.mime_for('.dll')
    assert_equal 'application/octet-stream', Mime.mime_for('.dmg')
    assert_equal 'application/octet-stream', Mime.mime_for('.exe')
    assert_equal 'application/postscript', Mime.mime_for('.ai')
    assert_equal 'application/postscript', Mime.mime_for('.eps')
    assert_equal 'application/postscript', Mime.mime_for('.ps')
    assert_equal 'application/vnd.adobe.air-application-installer-package+zip', Mime.mime_for('.air')
    assert_equal 'application/vnd.oasis.opendocument.presentation', Mime.mime_for('.odp')
    assert_equal 'application/vnd.oasis.opendocument.spreadsheet', Mime.mime_for('.ods')
    assert_equal 'application/vnd.oasis.opendocument.text', Mime.mime_for('.odt')
    assert_equal 'application/vnd.openxmlformats-officedocument.presentationml.presentation', Mime.mime_for('.pptx')
    assert_equal 'application/x-debian-package', Mime.mime_for('.deb')
    assert_equal 'application/x-latex', Mime.mime_for('.latex')
    assert_equal 'application/x-perl', Mime.mime_for('.pl')
    assert_equal 'application/x-perl', Mime.mime_for('.pm')
    assert_equal 'application/x-python', Mime.mime_for('.py')
    assert_equal 'application/x-ruby', Mime.mime_for('.rb')
    assert_equal 'application/x-sh', Mime.mime_for('.sh')
    assert_equal 'application/x-shockwave-flash', Mime.mime_for('.swf')
    assert_equal 'text/html', Mime.mime_for('.html')
    assert_equal 'text/plain', Mime.mime_for('.c')
    assert_equal 'text/plain', Mime.mime_for('.cc')
    assert_equal 'text/plain', Mime.mime_for('.cpp')
    assert_equal 'text/plain', Mime.mime_for('.cxx')
    assert_equal 'text/plain', Mime.mime_for('.h')
    assert_equal 'text/plain', Mime.mime_for('.hh')
    assert_equal 'text/plain', Mime.mime_for('.hpp')
    assert_equal 'text/plain', Mime.mime_for('.kt')
    assert_equal 'video/quicktime', Mime.mime_for('.mov')
  end
end
