require 'linguist/lexer'

require 'test/unit'

class TestLexer < Test::Unit::TestCase
  include Linguist

  def test_find_by_name
    assert_equal Lexer['Ruby'], Lexer.find_by_name('Ruby')
    assert_equal Lexer['Ruby'], Lexer.find_by_name('ruby')
    assert_equal Lexer['Ruby'], Lexer.find_by_name('RUBY')
  end

  def test_find_all_by_name
    Lexer.all.each do |lexer|
      assert_equal lexer, Lexer.find_by_name(lexer.name)
      assert_equal lexer, Lexer[lexer.name]
    end
  end

  def test_find_by_alias
    assert_equal Lexer['Ruby'], Lexer.find_by_alias('rb')
    assert_equal Lexer['Ruby'], Lexer.find_by_alias('ruby')
  end

  def test_find_all_by_alias
    Lexer.all.each do |lexer|
      lexer.aliases.each do |name|
        assert_equal lexer, Lexer.find_by_alias(name)
        assert_equal lexer, Lexer[name]
      end
    end
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

  def test_colorize
    assert_equal <<-HTML, Lexer['Text only'].colorize("Hello")
<div class="highlight"><pre>Hello
</pre>
</div>
    HTML

    assert_equal <<-HTML, Lexer['Ruby'].colorize("def foo\n  'foo'\nend\n")
<div class="highlight"><pre><span class="k">def</span> <span class="nf">foo</span>
  <span class="s1">&#39;foo&#39;</span>
<span class="k">end</span>
</pre>
</div>
    HTML
  end

  def test_colorize_without_wrapper
    assert_equal <<-HTML, Lexer['Text only'].colorize_without_wrapper("Hello")
Hello
    HTML

    assert_equal <<-HTML, Lexer['Ruby'].colorize_without_wrapper("def foo\n  'foo'\nend\n")
<span class="k">def</span> <span class="nf">foo</span>
  <span class="s1">&#39;foo&#39;</span>
<span class="k">end</span>
    HTML
  end
end
