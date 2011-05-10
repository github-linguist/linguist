require 'linguist/language'

require 'test/unit'

class TestLanguage < Test::Unit::TestCase
  include Linguist

  def test_find_by_name
    ruby = Language['Ruby']
    assert_equal ruby, Language.find_by_name('Ruby')
    assert_equal ruby, Language.find_by_name('ruby')
    assert_equal ruby, Language.find_by_name('RUBY')
  end

  def test_find_by_extension
    ruby = Language['Ruby']
    assert_equal ruby, Language.find_by_extension('.rb')
    assert_equal ruby, Language.find_by_extension('rb')
    assert_nil Language.find_by_extension('foo.rb')
  end

  def test_find_by_lexer
    assert_equal Language['Perl'], Language.find_by_lexer('perl')
    assert_equal Language['Python'], Language.find_by_lexer('python')
    assert_equal Language['Ruby'], Language.find_by_lexer('ruby')
    assert_equal Language['C++'], Language.find_by_lexer('cpp')
    assert_equal Language['JavaScript'], Language.find_by_lexer('javascript')
    assert_equal Language['Scheme'], Language.find_by_lexer('scheme')
    assert_nil Language.find_by_lexer('kt')
  end

  def test_name
    assert_equal "Perl",   Language['Perl'].name
    assert_equal "Python", Language['Python'].name
    assert_equal "Ruby",   Language['Ruby'].name
  end

  def test_error_without_name
    assert_raise ArgumentError do
      Language.new :name => nil
    end
  end

  def test_lexer
    assert_equal 'perl',   Language['Perl'].lexer
    assert_equal 'python', Language['Python'].lexer
    assert_equal 'ruby',   Language['Ruby'].lexer
    assert_equal 'cpp',    Language['C++'].lexer
    assert_equal 'bash',   Language['Gentoo Ebuild'].lexer
    assert_equal 'scheme', Language['Nu'].lexer
  end

  def test_extensions
    assert Language['Perl'].extensions.include?('.pl')
    assert Language['Python'].extensions.include?('.py')
    assert Language['Ruby'].extensions.include?('.rb')
  end
end
