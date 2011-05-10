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
    assert_equal ruby, Language.find_by_extension('foo.rb')
    assert_equal ruby, Language.find_by_extension('./foo.rb')
    assert_equal ruby, Language.find_by_extension('foo/bar.rb')

    # TODO: Review questionable usage
    assert_equal ruby, Language.find_by_extension('Rakefile')
    assert_equal ruby, Language.find_by_extension('vendor/Rakefile')
    assert_equal ruby, Language.find_by_extension('./Rakefile')
  end

  def test_name
    assert_equal "Perl",   Language['Perl'].name
    assert_equal "Python", Language['Python'].name
    assert_equal "Ruby",   Language['Ruby'].name
  end

  def test_extensions
    assert Language['Perl'].extensions.include?('.pl')
    assert Language['Python'].extensions.include?('.py')
    assert Language['Ruby'].extensions.include?('.rb')
  end
end
