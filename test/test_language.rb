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

  def test_find_all_by_name
    Language.all.each do |language|
      assert_equal language, Language.find_by_name(language.name)
      assert_equal language, Language[language.name]
    end
  end

  def test_find_by_alias
    assert_equal Language['Perl'],      Language.find_by_alias('perl')
    assert_equal Language['Python'],    Language.find_by_alias('python')
    assert_equal Language['Ruby'],      Language.find_by_alias('ruby')
    assert_equal Language['HTML+ERB'],  Language.find_by_alias('html+erb')
    assert_equal Language['Max/MSP'],   Language.find_by_alias('max/msp')
    assert_equal Language['Pure Data'], Language.find_by_alias('pure-data')

    assert_equal Language['ASP'],          Language.find_by_alias('asp')
    assert_equal Language['ASP'],          Language.find_by_alias('aspx')
    assert_equal Language['ASP'],          Language.find_by_alias('aspx-vb')
    assert_equal Language['ActionScript'], Language.find_by_alias('as3')
    assert_equal Language['Assembly'],     Language.find_by_alias('nasm')
    assert_equal Language['Batchfile'],    Language.find_by_alias('bat')

    assert_equal Language['C++'], Language.find_by_alias('c++')
    assert_equal Language['C++'], Language.find_by_alias('cpp')
    assert_equal Language['C#'],  Language.find_by_alias('c#')
    assert_equal Language['C#'],  Language.find_by_alias('csharp')

    assert_equal Language['Java'],              Language.find_by_alias('java')
    assert_equal Language['ChucK'],             Language.find_by_alias('chuck')
    assert_equal Language['Groovy'],            Language.find_by_alias('groovy')
    assert_equal Language['Java Server Pages'], Language.find_by_alias('jsp')

    assert_equal Language['ColdFusion'],  Language.find_by_alias('cfm')
    assert_equal Language['Darcs Patch'], Language.find_by_alias('dpatch')

    assert_equal Language['Common Lisp'], Language.find_by_alias('common-lisp')
    assert_equal Language['Common Lisp'], Language.find_by_alias('lisp')
    assert_equal Language['Emacs Lisp'],  Language.find_by_alias('emacs-lisp')
    assert_equal Language['Emacs Lisp'],  Language.find_by_alias('elisp')
    assert_equal Language['Nu'],          Language.find_by_alias('nu')
    assert_equal Language['Scheme'],      Language.find_by_alias('scheme')

    assert_equal Language['OCaml'],           Language.find_by_alias('ocaml')
    assert_equal Language['F#'],              Language.find_by_alias('f#')
    assert_equal Language['Gettext Catalog'], Language.find_by_alias('pot')
    assert_equal Language['IRC log'],         Language.find_by_alias('irc')

    assert_equal Language['JavaScript'], Language.find_by_alias('javascript')
    assert_equal Language['JavaScript'], Language.find_by_alias('js')
    assert_equal Language['JSON'],       Language.find_by_alias('json')

    assert_equal Language['Haskell'],          Language.find_by_alias('haskell')
    assert_equal Language['Literate Haskell'], Language.find_by_alias('literate-haskell')
    assert_equal Language['Literate Haskell'], Language.find_by_alias('lhs')

    assert_equal Language['Parrot Internal Representation'], Language.find_by_alias('pir')

    assert_equal Language['Python traceback'], Language.find_by_alias('pytb')

    assert_equal Language['Raw token data'],   Language.find_by_alias('raw')
    assert_equal Language['reStructuredText'], Language.find_by_alias('rst')

    assert_equal Language['Shell'], Language.find_by_alias('shell')
    assert_equal Language['Shell'], Language.find_by_alias('sh')
    assert_equal Language['Shell'], Language.find_by_alias('bash')
    assert_equal Language['Shell'], Language.find_by_alias('zsh')

    assert_equal Language['VimL'], Language.find_by_alias('viml')
    assert_equal Language['VimL'], Language.find_by_alias('vim')

    assert_equal Language['XS'], Language.find_by_alias('xs')
  end

  def test_find_all_by_alias
    Language.all.each do |language|
      language.aliases.each do |name|
        assert_equal language, Language.find_by_alias(name)
        assert_equal language, Language[name]
      end
    end
  end

  def test_find_by_extension
    assert_equal Language['Ruby'], Language.find_by_extension('.rb')
    assert_equal Language['Ruby'], Language.find_by_extension('rb')
    assert_nil Language.find_by_extension('.kt')
  end

  def test_find_all_by_extension
    Language.all.each do |language|
      language.extensions.each do |extension|
        assert_equal language, Language.find_by_extension(extension)
      end
    end
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
    assert_equal "Perl",    Language['Perl'].name
    assert_equal "Python",  Language['Python'].name
    assert_equal "Ruby",    Language['Ruby'].name
  end

  # Used for code search indexing. Changing any of these values may
  # require reindexing repositories.
  def test_search_term
    assert_equal "perl",        Language['Perl'].search_term
    assert_equal "python",      Language['Python'].search_term
    assert_equal "ruby",        Language['Ruby'].search_term
    assert_equal "common-lisp", Language['Common Lisp'].search_term
    assert_equal "html+erb",    Language['HTML+ERB'].search_term
    assert_equal "max/msp",     Language['Max/MSP'].search_term
    assert_equal "pure-data",   Language['Pure Data'].search_term

    assert_equal "aspx-vb",    Language['ASP'].search_term
    assert_equal "as3",        Language['ActionScript'].search_term
    assert_equal "nasm",       Language['Assembly'].search_term
    assert_equal "bat",        Language['Batchfile'].search_term
    assert_equal "csharp",     Language['C#'].search_term
    assert_equal "cpp",        Language['C++'].search_term
    assert_equal "java",       Language['ChucK'].search_term
    assert_equal "cfm",        Language['ColdFusion'].search_term
    assert_equal "dpatch",     Language['Darcs Patch'].search_term
    assert_equal "scheme",     Language['Emacs Lisp'].search_term
    assert_equal "ocaml",      Language['F#'].search_term
    assert_equal "bash",       Language['Gentoo Ebuild'].search_term
    assert_equal "bash",       Language['Gentoo Eclass'].search_term
    assert_equal "pot",        Language['Gettext Catalog'].search_term
    assert_equal "irc",        Language['IRC log'].search_term
    assert_equal "java",       Language['Groovy'].search_term
    assert_equal "javascript", Language['JSON'].search_term
    assert_equal "lhs",        Language['Literate Haskell'].search_term
    assert_equal "ruby",       Language['Mirah'].search_term
    assert_equal "scheme",     Language['Nu'].search_term
    assert_equal "pir",        Language['Parrot Internal Representation'].search_term
    assert_equal "pytb",       Language['Python traceback'].search_term
    assert_equal "raw",        Language['Raw token data'].search_term
    assert_equal "bash",       Language['Shell'].search_term
    assert_equal "vim",        Language['VimL'].search_term
    assert_equal "c",          Language['XS'].search_term
    assert_equal "jsp",        Language['Java Server Pages'].search_term
    assert_equal "rst",        Language['reStructuredText'].search_term
  end

  def test_error_without_name
    assert_raise ArgumentError do
      Language.new :name => nil
    end
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
