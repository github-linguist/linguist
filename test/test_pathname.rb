require 'linguist/pathname'

require 'test/unit'
require 'pygments'

class TestPathname < Test::Unit::TestCase
  include Linguist

  Lexer = Pygments::Lexer

  def test_to_s
    assert_equal "file.rb", Pathname.new("file.rb").to_s
  end

  def test_basename
    assert_equal 'file.rb', Pathname.new("file.rb").basename
    assert_equal 'file.rb', Pathname.new("./file.rb").basename
    assert_equal 'file.rb', Pathname.new("sub/dir/file.rb").basename
    assert_equal '.profile', Pathname.new(".profile").basename
  end

  def test_extname
    assert_equal '.rb', Pathname.new("file.rb").extname
    assert_equal '.rb', Pathname.new("./file.rb").extname
    assert_equal '.rb', Pathname.new("sub/dir/file.rb").extname
    assert_equal '',    Pathname.new(".profile").extname
  end

  def test_language
    assert_nil Pathname.new(".rb").language

    assert_equal Language['Ruby'], Pathname.new("file.rb").language
    assert_equal Language['Ruby'], Pathname.new("./file.rb").language
    assert_equal Language['Ruby'], Pathname.new("sub/dir/file.rb").language

    assert_equal Language['Ruby'], Pathname.new("Rakefile").language
    assert_equal Language['Ruby'], Pathname.new("vendor/Rakefile").language
    assert_equal Language['Ruby'], Pathname.new("./Rakefile").language

    assert_equal Language['Gentoo Ebuild'], Pathname.new("file.ebuild").language
    assert_equal Language['Python'], Pathname.new("itty.py").language
    assert_equal Language['Nu'], Pathname.new("itty.nu").language

    assert_nil Pathname.new("defun.kt").language
  end

  def test_lexer
    assert_equal Lexer['Ruby'],      Pathname.new("file.rb").lexer
    assert_equal Lexer['Ruby'],      Pathname.new("Rakefile").lexer
    assert_equal Lexer['Bash'],      Pathname.new("file.ebuild").lexer
    assert_equal Lexer['Python'],    Pathname.new("itty.py").lexer
    assert_equal Lexer['Scheme'],    Pathname.new("itty.nu").lexer
    assert_equal Lexer['Text only'], Pathname.new("defun.kt").lexer
  end

  def test_mime_type
    assert_equal 'application/x-ruby', Pathname.new("file.rb").mime_type
    assert_equal 'application/javascript', Pathname.new("file.js").mime_type
    assert_equal 'application/x-python', Pathname.new("itty.py").mime_type
    assert_equal 'text/plain', Pathname.new("defun.kt").mime_type
  end
end
