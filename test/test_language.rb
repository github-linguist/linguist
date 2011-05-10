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

  def test_find_by_filename
    ruby = Language['Ruby']
    assert_equal ruby, Language.find_by_filename('.rb')
    assert_equal ruby, Language.find_by_filename('rb')
    assert_equal ruby, Language.find_by_filename('foo.rb')
    assert_equal ruby, Language.find_by_filename('./foo.rb')
    assert_equal ruby, Language.find_by_filename('foo/bar.rb')

    assert_equal ruby, Language.find_by_filename('Rakefile')
    assert_equal ruby, Language.find_by_filename('vendor/Rakefile')
    assert_equal ruby, Language.find_by_filename('./Rakefile')
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
  end

  def test_extensions
    assert Language['Perl'].extensions.include?('.pl')
    assert Language['Python'].extensions.include?('.py')
    assert Language['Ruby'].extensions.include?('.rb')
  end
end
