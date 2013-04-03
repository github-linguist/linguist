require 'linguist/language'

require 'test/unit'
require 'pygments'

class TestLanguage < Test::Unit::TestCase
  include Linguist

  Lexer = Pygments::Lexer

  def test_lexer
    assert_equal Lexer['ActionScript 3'], Language['ActionScript'].lexer
    assert_equal Lexer['Bash'], Language['Gentoo Ebuild'].lexer
    assert_equal Lexer['Bash'], Language['Gentoo Eclass'].lexer
    assert_equal Lexer['Bash'], Language['Shell'].lexer
    assert_equal Lexer['C'], Language['OpenCL'].lexer
    assert_equal Lexer['C'], Language['XS'].lexer
    assert_equal Lexer['C++'], Language['C++'].lexer
    assert_equal Lexer['Coldfusion HTML'], Language['ColdFusion'].lexer
    assert_equal Lexer['Coq'], Language['Coq'].lexer
    assert_equal Lexer['FSharp'], Language['F#'].lexer
    assert_equal Lexer['FSharp'], Language['F#'].lexer
    assert_equal Lexer['Fortran'], Language['FORTRAN'].lexer
    assert_equal Lexer['Gherkin'], Language['Cucumber'].lexer
    assert_equal Lexer['Groovy'], Language['Groovy'].lexer
    assert_equal Lexer['HTML'], Language['HTML'].lexer
    assert_equal Lexer['HTML+Django/Jinja'], Language['HTML+Django'].lexer
    assert_equal Lexer['HTML+PHP'], Language['HTML+PHP'].lexer
    assert_equal Lexer['HTTP'], Language['HTTP'].lexer
    assert_equal Lexer['JSON'], Language['JSON'].lexer
    assert_equal Lexer['Java'], Language['ChucK'].lexer
    assert_equal Lexer['Java'], Language['Java'].lexer
    assert_equal Lexer['JavaScript'], Language['JavaScript'].lexer
    assert_equal Lexer['MOOCode'], Language['Moocode'].lexer
    assert_equal Lexer['MuPAD'], Language['mupad'].lexer
    assert_equal Lexer['NASM'], Language['Assembly'].lexer
    assert_equal Lexer['OCaml'], Language['OCaml'].lexer
    assert_equal Lexer['Ooc'], Language['ooc'].lexer
    assert_equal Lexer['OpenEdge ABL'], Language['OpenEdge ABL'].lexer
    assert_equal Lexer['REBOL'], Language['Rebol'].lexer
    assert_equal Lexer['RHTML'], Language['HTML+ERB'].lexer
    assert_equal Lexer['RHTML'], Language['RHTML'].lexer
    assert_equal Lexer['Ruby'], Language['Mirah'].lexer
    assert_equal Lexer['Ruby'], Language['Ruby'].lexer
    assert_equal Lexer['S'], Language['R'].lexer
    assert_equal Lexer['Scheme'], Language['Emacs Lisp'].lexer
    assert_equal Lexer['Scheme'], Language['Nu'].lexer
    assert_equal Lexer['Racket'], Language['Racket'].lexer
    assert_equal Lexer['Scheme'], Language['Scheme'].lexer
    assert_equal Lexer['Standard ML'], Language['Standard ML'].lexer
    assert_equal Lexer['TeX'], Language['TeX'].lexer
    assert_equal Lexer['Verilog'], Language['Verilog'].lexer
    assert_equal Lexer['XSLT'], Language['XSLT'].lexer
    assert_equal Lexer['aspx-vb'], Language['ASP'].lexer
    assert_equal Lexer['haXe'], Language['Haxe'].lexer
    assert_equal Lexer['reStructuredText'], Language['reStructuredText'].lexer
  end

  def test_find_by_alias
    assert_equal Language['ASP'], Language.find_by_alias('asp')
    assert_equal Language['ASP'], Language.find_by_alias('aspx')
    assert_equal Language['ASP'], Language.find_by_alias('aspx-vb')
    assert_equal Language['ActionScript'], Language.find_by_alias('as3')
    assert_equal Language['ApacheConf'], Language.find_by_alias('apache')
    assert_equal Language['Assembly'], Language.find_by_alias('nasm')
    assert_equal Language['Batchfile'], Language.find_by_alias('bat')
    assert_equal Language['C#'], Language.find_by_alias('c#')
    assert_equal Language['C#'], Language.find_by_alias('csharp')
    assert_equal Language['C'], Language.find_by_alias('c')
    assert_equal Language['C++'], Language.find_by_alias('c++')
    assert_equal Language['C++'], Language.find_by_alias('cpp')
    assert_equal Language['CoffeeScript'], Language.find_by_alias('coffee')
    assert_equal Language['CoffeeScript'], Language.find_by_alias('coffee-script')
    assert_equal Language['ColdFusion'], Language.find_by_alias('cfm')
    assert_equal Language['Common Lisp'], Language.find_by_alias('common-lisp')
    assert_equal Language['Common Lisp'], Language.find_by_alias('lisp')
    assert_equal Language['Darcs Patch'], Language.find_by_alias('dpatch')
    assert_equal Language['Dart'], Language.find_by_alias('dart')
    assert_equal Language['Emacs Lisp'], Language.find_by_alias('elisp')
    assert_equal Language['Emacs Lisp'], Language.find_by_alias('emacs')
    assert_equal Language['Emacs Lisp'], Language.find_by_alias('emacs-lisp')
    assert_equal Language['Gettext Catalog'], Language.find_by_alias('pot')
    assert_equal Language['HTML'], Language.find_by_alias('html')
    assert_equal Language['HTML'], Language.find_by_alias('xhtml')
    assert_equal Language['HTML+ERB'], Language.find_by_alias('html+erb')
    assert_equal Language['HTML+ERB'], Language.find_by_alias('erb')
    assert_equal Language['IRC log'], Language.find_by_alias('irc')
    assert_equal Language['JSON'], Language.find_by_alias('json')
    assert_equal Language['Java Server Pages'], Language.find_by_alias('jsp')
    assert_equal Language['Java'], Language.find_by_alias('java')
    assert_equal Language['JavaScript'], Language.find_by_alias('javascript')
    assert_equal Language['JavaScript'], Language.find_by_alias('js')
    assert_equal Language['Literate Haskell'], Language.find_by_alias('lhs')
    assert_equal Language['Literate Haskell'], Language.find_by_alias('literate-haskell')
    assert_equal Language['Objective-C'], Language.find_by_alias('objc')
    assert_equal Language['OpenEdge ABL'], Language.find_by_alias('openedge')
    assert_equal Language['OpenEdge ABL'], Language.find_by_alias('progress')
    assert_equal Language['OpenEdge ABL'], Language.find_by_alias('abl')
    assert_equal Language['Parrot Internal Representation'], Language.find_by_alias('pir')
    assert_equal Language['PowerShell'], Language.find_by_alias('posh')
    assert_equal Language['Puppet'], Language.find_by_alias('puppet')
    assert_equal Language['Pure Data'], Language.find_by_alias('pure-data')
    assert_equal Language['Raw token data'], Language.find_by_alias('raw')
    assert_equal Language['Ruby'], Language.find_by_alias('rb')
    assert_equal Language['Ruby'], Language.find_by_alias('ruby')
    assert_equal Language['Scheme'], Language.find_by_alias('scheme')
    assert_equal Language['Shell'], Language.find_by_alias('bash')
    assert_equal Language['Shell'], Language.find_by_alias('sh')
    assert_equal Language['Shell'], Language.find_by_alias('shell')
    assert_equal Language['Shell'], Language.find_by_alias('zsh')
    assert_equal Language['TeX'], Language.find_by_alias('tex')
    assert_equal Language['TypeScript'], Language.find_by_alias('ts')
    assert_equal Language['VimL'], Language.find_by_alias('vim')
    assert_equal Language['VimL'], Language.find_by_alias('viml')
    assert_equal Language['reStructuredText'], Language.find_by_alias('rst')
    assert_equal Language['YAML'], Language.find_by_alias('yml')
  end

  def test_groups
    # Test a couple identity cases
    assert_equal Language['Perl'], Language['Perl'].group
    assert_equal Language['Python'], Language['Python'].group
    assert_equal Language['Ruby'], Language['Ruby'].group

    # Test a few special groups
    assert_equal Language['Assembly'], Language['GAS'].group
    assert_equal Language['C'], Language['OpenCL'].group
    assert_equal Language['Haskell'], Language['Literate Haskell'].group
    assert_equal Language['Java'], Language['Java Server Pages'].group
    assert_equal Language['Python'], Language['Cython'].group
    assert_equal Language['Python'], Language['NumPy'].group
    assert_equal Language['Shell'], Language['Batchfile'].group
    assert_equal Language['Shell'], Language['Gentoo Ebuild'].group
    assert_equal Language['Shell'], Language['Gentoo Eclass'].group
    assert_equal Language['Shell'], Language['Tcsh'].group

    # Ensure everyone has a group
    Language.all.each do |language|
      assert language.group, "#{language} has no group"
    end
  end

  # Used for code search indexing. Changing any of these values may
  # require reindexing repositories.
  def test_search_term
    assert_equal 'perl',        Language['Perl'].search_term
    assert_equal 'python',      Language['Python'].search_term
    assert_equal 'ruby',        Language['Ruby'].search_term
    assert_equal 'common-lisp', Language['Common Lisp'].search_term
    assert_equal 'html+erb',    Language['HTML+ERB'].search_term
    assert_equal 'max/msp',     Language['Max'].search_term
    assert_equal 'puppet',      Language['Puppet'].search_term
    assert_equal 'pure-data',   Language['Pure Data'].search_term

    assert_equal 'aspx-vb',       Language['ASP'].search_term
    assert_equal 'as3',           Language['ActionScript'].search_term
    assert_equal 'nasm',          Language['Assembly'].search_term
    assert_equal 'bat',           Language['Batchfile'].search_term
    assert_equal 'csharp',        Language['C#'].search_term
    assert_equal 'cpp',           Language['C++'].search_term
    assert_equal 'cfm',           Language['ColdFusion'].search_term
    assert_equal 'dpatch',        Language['Darcs Patch'].search_term
    assert_equal 'ocaml',         Language['F#'].search_term
    assert_equal 'pot',           Language['Gettext Catalog'].search_term
    assert_equal 'irc',           Language['IRC log'].search_term
    assert_equal 'lhs',           Language['Literate Haskell'].search_term
    assert_equal 'ruby',          Language['Mirah'].search_term
    assert_equal 'raw',           Language['Raw token data'].search_term
    assert_equal 'bash',          Language['Shell'].search_term
    assert_equal 'vim',           Language['VimL'].search_term
    assert_equal 'jsp',           Language['Java Server Pages'].search_term
    assert_equal 'rst',           Language['reStructuredText'].search_term
  end

  def test_popular
    assert Language['Ruby'].popular?
    assert Language['Perl'].popular?
    assert Language['Python'].popular?
    assert Language['Assembly'].unpopular?
    assert Language['Brainfuck'].unpopular?
  end

  def test_programming
    assert_equal :programming, Language['JavaScript'].type
    assert_equal :programming, Language['Perl'].type
    assert_equal :programming, Language['PowerShell'].type
    assert_equal :programming, Language['Python'].type
    assert_equal :programming, Language['Ruby'].type
    assert_equal :programming, Language['TypeScript'].type
  end

  def test_markup
    assert_equal :markup, Language['HTML'].type
    assert_equal :markup, Language['YAML'].type
  end

  def test_other
    assert_nil Language['Brainfuck'].type
    assert_nil Language['Makefile'].type
  end

  def test_searchable
    assert Language['Ruby'].searchable?
    assert !Language['Gettext Catalog'].searchable?
    assert !Language['SQL'].searchable?
  end

  def test_find_by_name
    ruby = Language['Ruby']
    assert_equal ruby, Language.find_by_name('Ruby')
  end

  def test_find_all_by_name
    Language.all.each do |language|
      assert_equal language, Language.find_by_name(language.name)
      assert_equal language, Language[language.name]
    end
  end

  def test_find_all_by_alias
    Language.all.each do |language|
      language.aliases.each do |name|
        assert_equal language, Language.find_by_alias(name)
        assert_equal language, Language[name]
      end
    end
  end

  def test_find_by_filename
    assert_equal [Language['Shell']], Language.find_by_filename('PKGBUILD')
    assert_equal [Language['Ruby']], Language.find_by_filename('foo.rb')
    assert_equal [Language['Ruby']], Language.find_by_filename('foo/bar.rb')
    assert_equal [Language['Ruby']], Language.find_by_filename('Rakefile')
    assert_equal [Language['Ruby']], Language.find_by_filename('PKGBUILD.rb')
    assert_equal Language['ApacheConf'], Language.find_by_filename('httpd.conf').first
    assert_equal [Language['ApacheConf']], Language.find_by_filename('.htaccess')
    assert_equal Language['Nginx'], Language.find_by_filename('nginx.conf').first
    assert_equal ['C', 'C++', 'Objective-C'], Language.find_by_filename('foo.h').map(&:name).sort
    assert_equal [], Language.find_by_filename('rb')
    assert_equal [], Language.find_by_filename('.rb')
    assert_equal [], Language.find_by_filename('.nkt')
    assert_equal [Language['Shell']], Language.find_by_filename('.bashrc')
    assert_equal [Language['Shell']], Language.find_by_filename('bash_profile')
    assert_equal [Language['Shell']], Language.find_by_filename('.zshrc')
  end

  def test_find
    assert_equal 'Ruby', Language['Ruby'].name
    assert_equal 'Ruby', Language['ruby'].name
    assert_equal 'C++', Language['C++'].name
    assert_equal 'C++', Language['c++'].name
    assert_equal 'C++', Language['cpp'].name
    assert_equal 'C#', Language['C#'].name
    assert_equal 'C#', Language['c#'].name
    assert_equal 'C#', Language['csharp'].name
    assert_nil Language['defunkt']
  end

  def test_name
    assert_equal 'Perl',   Language['Perl'].name
    assert_equal 'Python', Language['Python'].name
    assert_equal 'Ruby',   Language['Ruby'].name
  end

  def test_escaped_name
    assert_equal 'C', Language['C'].escaped_name
    assert_equal 'C%23', Language['C#'].escaped_name
    assert_equal 'C%2B%2B', Language['C++'].escaped_name
    assert_equal 'Objective-C', Language['Objective-C'].escaped_name
    assert_equal 'Common%20Lisp', Language['Common Lisp'].escaped_name
  end

  def test_error_without_name
    assert_raise ArgumentError do
      Language.new :name => nil
    end
  end

  def test_color
    assert_equal '#701516', Language['Ruby'].color
    assert_equal '#3581ba', Language['Python'].color
    assert_equal '#f15501', Language['JavaScript'].color
    assert_equal '#31859c', Language['TypeScript'].color
  end

  def test_colors
    assert Language.colors.include?(Language['Ruby'])
    assert Language.colors.include?(Language['Python'])
  end

  def test_ace_mode
    assert_equal 'c_cpp', Language['C++'].ace_mode
    assert_equal 'coffee', Language['CoffeeScript'].ace_mode
    assert_equal 'csharp', Language['C#'].ace_mode
    assert_equal 'css', Language['CSS'].ace_mode
    assert_equal 'javascript', Language['JavaScript'].ace_mode
  end

  def test_ace_modes
    assert Language.ace_modes.include?(Language['Ruby'])
    assert !Language.ace_modes.include?(Language['FORTRAN'])
  end

  def test_wrap
    assert_equal false, Language['C'].wrap
    assert_equal true, Language['Markdown'].wrap
  end

  def test_extensions
    assert Language['Perl'].extensions.include?('.pl')
    assert Language['Python'].extensions.include?('.py')
    assert Language['Ruby'].extensions.include?('.rb')
  end

  def test_primary_extension
    assert_equal '.pl', Language['Perl'].primary_extension
    assert_equal '.py', Language['Python'].primary_extension
    assert_equal '.rb', Language['Ruby'].primary_extension
    assert_equal '.js', Language['JavaScript'].primary_extension
    assert_equal '.coffee', Language['CoffeeScript'].primary_extension
    assert_equal '.t', Language['Turing'].primary_extension
    assert_equal '.ts', Language['TypeScript'].primary_extension

    # This is a nasty requirement, but there's some code in GitHub that
    # expects this. Really want to drop this.
    Language.all.each do |language|
      assert language.primary_extension, "#{language} has no primary extension"
    end
  end

  def test_eql
    assert Language['Ruby'].eql?(Language['Ruby'])
    assert !Language['Ruby'].eql?(Language['Python'])
  end


  def test_colorize
    assert_equal <<-HTML.chomp, Language['Ruby'].colorize("def foo\n  'foo'\nend\n")
<div class="highlight"><pre><span class="k">def</span> <span class="nf">foo</span>
  <span class="s1">&#39;foo&#39;</span>
<span class="k">end</span>
</pre></div>
    HTML
  end
end
