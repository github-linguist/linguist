require 'linguist/language'

require 'test/unit'

class TestLanguage < Test::Unit::TestCase
  include Linguist

  def test_name
    assert_equal "Perl",   Language['Perl'].name
    assert_equal "Python", Language['Python'].name
    assert_equal "Ruby",   Language['Ruby'].name
  end
end
