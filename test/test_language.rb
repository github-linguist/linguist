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
    assert_equal Language['Ruby'], Language.find_by_extension('.rb')
    assert_equal Language['Ruby'], Language.find_by_extension('rb')
    assert_nil Language.find_by_extension('.kt')
  end

  def test_find_by_lexer
    assert_equal Language['C'], Language.find_by_lexer('c')
    assert_equal Language['C++'], Language.find_by_lexer('cpp')
    assert_equal Language['Java'], Language.find_by_lexer('java')
    assert_equal Language['JavaScript'], Language.find_by_lexer('javascript')
    assert_equal Language['OCaml'], Language.find_by_lexer('ocaml')
    assert_equal Language['Perl'], Language.find_by_lexer('perl')
    assert_equal Language['Python'], Language.find_by_lexer('python')
    assert_equal Language['Ruby'], Language.find_by_lexer('ruby')
    assert_equal Language['Scheme'], Language.find_by_lexer('scheme')
    assert_equal Language['Shell'], Language.find_by_lexer('bash')
    assert_equal Language['Text'], Language.find_by_lexer('kt')
  end

  def test_find
    assert_equal "Ruby", Language['Ruby'].name
    assert_equal "Ruby", Language['ruby'].name
    assert_equal "Ruby", Language['RUBY'].name
    assert_equal "C++", Language['C++'].name
    assert_equal "C++", Language['c++'].name
    assert_equal "C++", Language['cpp'].name
    assert_equal "C#", Language['C#'].name
    assert_equal "C#", Language['c#'].name
    assert_equal "C#", Language['csharp'].name
    assert_equal "C#", Language['CSHARP'].name
    assert_equal "Text", Language['defunkt'].name
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

  def test_search_term
    assert_equal 'perl',   Language['Perl'].search_term
    assert_equal 'python', Language['Python'].search_term
    assert_equal 'ruby',   Language['Ruby'].search_term
    assert_equal 'cpp',    Language['C++'].search_term
    assert_equal 'bash',   Language['Gentoo Ebuild'].search_term
    assert_equal 'scheme', Language['Nu'].search_term
    assert_equal 'text',   Language['Text'].search_term
  end

  def test_lexer
    assert_equal Lexer['Perl'],   Language['Perl'].lexer
    assert_equal Lexer['Python'], Language['Python'].lexer
    assert_equal Lexer['Ruby'],   Language['Ruby'].lexer
    assert_equal Lexer['C++'],    Language['C++'].lexer
    assert_equal Lexer['Bash'],   Language['Gentoo Ebuild'].lexer
    assert_equal Lexer['Scheme'], Language['Nu'].lexer
  end

  def test_extensions
    assert Language['Perl'].extensions.include?('.pl')
    assert Language['Python'].extensions.include?('.py')
    assert Language['Ruby'].extensions.include?('.rb')
  end

  def test_eql
    assert Language['Ruby'].eql?(Language['Ruby'])
    assert !Language['Ruby'].eql?(Language['Python'])
    assert !Language['Ruby'].eql?(Language.new(:name => 'Ruby'))
  end

  def test_popular
    assert Language['Ruby'].popular?
    assert Language['Perl'].popular?
    assert Language['Python'].popular?
    assert Language['Assembly'].unpopular?
    assert Language['Brainfuck'].unpopular?
  end

  def test_common
    assert Language['Perl'].common?
    assert Language['Python'].common?
    assert Language['Ruby'].common?
    assert !Language['Brainfuck'].common?
    assert !Language['Makefile'].common?
  end

  def test_colorize
    assert_equal <<-HTML, Language['Text'].colorize("Hello")
<div class="highlight"><pre>Hello
</pre>
</div>
    HTML

    assert_equal <<-HTML, Language['Ruby'].colorize("def foo\n  'foo'\nend\n")
<div class="highlight"><pre><span class="k">def</span> <span class="nf">foo</span>
  <span class="s1">&#39;foo&#39;</span>
<span class="k">end</span>
</pre>
</div>
    HTML
  end

  def test_colorize_without_wrapper
    assert_equal <<-HTML, Language['Text'].colorize_without_wrapper("Hello")
Hello
    HTML

    assert_equal <<-HTML, Language['Ruby'].colorize_without_wrapper("def foo\n  'foo'\nend\n")
<span class="k">def</span> <span class="nf">foo</span>
  <span class="s1">&#39;foo&#39;</span>
<span class="k">end</span>
    HTML
  end
end
