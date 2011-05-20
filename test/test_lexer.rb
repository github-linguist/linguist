require 'linguist/lexer'

require 'test/unit'

class TestLexer < Test::Unit::TestCase
  include Linguist

  def test_find_by_name
    assert_equal Lexer['Ruby'], Lexer.find_by_name('Ruby')
    assert_equal Lexer['Ruby'], Lexer.find_by_name('ruby')
    assert_equal Lexer['Ruby'], Lexer.find_by_name('RUBY')
  end

  def test_find_by_alias
    assert_equal Lexer['Ruby'], Lexer.find_by_alias('rb')
    assert_equal Lexer['Ruby'], Lexer.find_by_alias('ruby')
    assert_equal Lexer['Ruby'], Lexer.find_by_alias('duby')
  end

  def test_name
    assert_equal 'Ruby',   Lexer['Ruby'].name
    assert_equal 'Python', Lexer['Python'].name
    assert_equal 'Perl',   Lexer['Perl'].name
  end

  def test_aliases
    assert_equal ['rb', 'ruby', 'duby'], Lexer['Ruby'].aliases
    assert_equal ['python', 'py'],       Lexer['Python'].aliases
    assert_equal ['perl', 'pl'],         Lexer['Perl'].aliases
  end

  def test_eql
    assert Lexer['Ruby'].eql?(Lexer['Ruby'])
    assert !Lexer['Ruby'].eql?(Lexer['Python'])
    assert !Lexer['Ruby'].eql?(Lexer.new('Ruby'))
  end
end
