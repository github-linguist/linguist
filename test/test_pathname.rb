require 'linguist/pathname'

require 'test/unit'

class TestPathname < Test::Unit::TestCase
  include Linguist

  def test_to_s
    assert_equal "file.rb", Pathname.new("file.rb").to_s
  end

  def test_basename
    assert_equal 'file.rb', Pathname.new("file.rb").basename
    assert_equal 'file.rb', Pathname.new("./file.rb").basename
    assert_equal 'file.rb', Pathname.new("sub/dir/file.rb").basename
  end

  def test_extname
    assert_equal '.rb', Pathname.new(".rb").extname
    assert_equal '.rb', Pathname.new("file.rb").extname
    assert_equal '.rb', Pathname.new("./file.rb").extname
    assert_equal '.rb', Pathname.new("sub/dir/file.rb").extname

    assert_equal 'Rakefile', Pathname.new("Rakefile").extname
    assert_equal 'Rakefile', Pathname.new("./Rakefile").extname
    assert_equal 'Rakefile', Pathname.new("vendor/Rakefile").extname
  end

  def test_language
    assert_equal Language['Ruby'], Pathname.new(".rb").language
    assert_equal Language['Ruby'], Pathname.new("file.rb").language
    assert_equal Language['Ruby'], Pathname.new("./file.rb").language
    assert_equal Language['Ruby'], Pathname.new("sub/dir/file.rb").language

    assert_equal Language['Ruby'], Pathname.new("Rakefile").language
    assert_equal Language['Ruby'], Pathname.new("vendor/Rakefile").language
    assert_equal Language['Ruby'], Pathname.new("./Rakefile").language

    assert_equal Language['Gentoo Ebuild'], Pathname.new("file.ebuild").language
    assert_equal Language['Python'], Pathname.new("itty.py").language
    assert_equal Language['Nu'], Pathname.new("itty.nu").language
    assert_equal Language['Text'], Pathname.new("defun.kt").language
  end

  def test_lexer
    assert_equal 'ruby',   Pathname.new("file.rb").lexer
    assert_equal 'ruby',   Pathname.new("Rakefile").lexer
    assert_equal 'bash',   Pathname.new("file.ebuild").lexer
    assert_equal 'python', Pathname.new("itty.py").lexer
    assert_equal 'scheme', Pathname.new("itty.nu").lexer
    assert_equal 'text',   Pathname.new("defun.kt").lexer
  end

  def test_mime_type
    assert_equal 'application/ruby', Pathname.new("file.rb").mime_type
    assert_equal 'application/javascript', Pathname.new("file.js").mime_type
    assert_equal 'application/python', Pathname.new("itty.py").mime_type
    assert_equal 'text/plain; charset=utf-8', Pathname.new("defun.kt").mime_type
  end

  def test_media_type
    assert_equal 'application', Pathname.new("file.js").media_type
    assert_equal 'text', Pathname.new("file.txt").media_type
    assert_equal 'text', Pathname.new("defun.kt").media_type
  end

  def test_file
    assert Pathname.new("octocat.png").file?
    assert Pathname.new("linguist.gem").file?
  end

  def test_text
    assert Pathname.new("file.txt").text?
    assert Pathname.new("file.json").text?
  end

  def test_image
    assert Pathname.new("octocat.png").image?
    assert Pathname.new("octocat.jpg").image?
    assert Pathname.new("octocat.jpeg").image?
    assert Pathname.new("octocat.gif").image?
    assert !Pathname.new("octocat.psd").image?
  end
end
