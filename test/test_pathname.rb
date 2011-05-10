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
    assert_nil Pathname.new("defun.kt").language
  end
end
