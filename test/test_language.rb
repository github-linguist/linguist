require 'linguist/language'

require 'test/unit'

class TestLanguage < Test::Unit::TestCase
  include Linguist

  def test_find_by_name
    assert_equal "Ruby", Language.find_by_name('Ruby').name
    assert_equal "Ruby", Language.find_by_name('ruby').name
    assert_equal "Ruby", Language.find_by_name('RUBY').name
  end

  def test_name
    assert_equal "Perl",   Language['Perl'].name
    assert_equal "Python", Language['Python'].name
    assert_equal "Ruby",   Language['Ruby'].name
  end
end
