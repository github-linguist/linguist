require 'linguist/language'

require 'test/unit'

class TestLanguage < Test::Unit::TestCase
  include Linguist

  def test_lexer
    # Add an assertion to this list if you add/change any lexers
    # in languages.yml. Please keep this list alphabetized.
    assert_equal Lexer['ActionScript 3'], Language['ActionScript'].lexer
    assert_equal Lexer['Ada'], Language['Ada'].lexer
    assert_equal Lexer['AppleScript'], Language['AppleScript'].lexer
    assert_equal Lexer['Bash'], Language['Gentoo Ebuild'].lexer
    assert_equal Lexer['Bash'], Language['Gentoo Eclass'].lexer
    assert_equal Lexer['Bash'], Language['Shell'].lexer
    assert_equal Lexer['Batchfile'], Language['Batchfile'].lexer
    assert_equal Lexer['Befunge'], Language['Befunge'].lexer
    assert_equal Lexer['BlitzMax'], Language['BlitzMax'].lexer
    assert_equal Lexer['Boo'], Language['Boo'].lexer
    assert_equal Lexer['Brainfuck'], Language['Brainfuck'].lexer
    assert_equal Lexer['C#'], Language['C#'].lexer
    assert_equal Lexer['C'], Language['C'].lexer
    assert_equal Lexer['C'], Language['OpenCL'].lexer
    assert_equal Lexer['C'], Language['XS'].lexer
    assert_equal Lexer['C++'], Language['C++'].lexer
    assert_equal Lexer['CSS'], Language['CSS'].lexer
    assert_equal Lexer['Clojure'], Language['Clojure'].lexer
    assert_equal Lexer['CoffeeScript'], Language['CoffeeScript'].lexer
    assert_equal Lexer['ColdFusion HTML'], Language['ColdFusion'].lexer
    assert_equal Lexer['Common Lisp'], Language['Common Lisp'].lexer
    assert_equal Lexer['Cython'], Language['Cython'].lexer
    assert_equal Lexer['D'], Language['D'].lexer
    assert_equal Lexer['D-ObjDump'], Language['d-objdump'].lexer
    assert_equal Lexer['Darcs Patch'], Language['Darcs Patch'].lexer
    assert_equal Lexer['Delphi'], Language['Delphi'].lexer
    assert_equal Lexer['Diff'], Language['Diff'].lexer
    assert_equal Lexer['Dylan'], Language['Dylan'].lexer
    assert_equal Lexer['Erlang'], Language['Erlang'].lexer
    assert_equal Lexer['Factor'], Language['Factor'].lexer
    assert_equal Lexer['Fortran'], Language['FORTRAN'].lexer
    assert_equal Lexer['GAS'], Language['GAS'].lexer
    assert_equal Lexer['Genshi'], Language['Genshi'].lexer
    assert_equal Lexer['Gettext Catalog'], Language['Gettext Catalog'].lexer
    assert_equal Lexer['Gherkin'], Language['Cucumber'].lexer
    assert_equal Lexer['Go'], Language['Go'].lexer
    assert_equal Lexer['Groff'], Language['Groff'].lexer
    assert_equal Lexer['HTML'], Language['HTML'].lexer
    assert_equal Lexer['HTML+Django/Jinja'], Language['HTML+Django'].lexer
    assert_equal Lexer['HTML+PHP'], Language['HTML+PHP'].lexer
    assert_equal Lexer['Haml'], Language['Haml'].lexer
    assert_equal Lexer['Haskell'], Language['Haskell'].lexer
    assert_equal Lexer['INI'], Language['INI'].lexer
    assert_equal Lexer['IRC logs'], Language['IRC log'].lexer
    assert_equal Lexer['Io'], Language['Io'].lexer
    assert_equal Lexer['Java Server Page'], Language['Java Server Pages'].lexer
    assert_equal Lexer['Java'], Language['ChucK'].lexer
    assert_equal Lexer['Java'], Language['Groovy'].lexer
    assert_equal Lexer['Java'], Language['Java'].lexer
    assert_equal Lexer['JavaScript'], Language['JSON'].lexer
    assert_equal Lexer['JavaScript'], Language['JavaScript'].lexer
    assert_equal Lexer['LLVM'], Language['LLVM'].lexer
    assert_equal Lexer['Literate Haskell'], Language['Literate Haskell'].lexer
    assert_equal Lexer['Lua'], Language['Lua'].lexer
    assert_equal Lexer['Makefile'], Language['Makefile'].lexer
    assert_equal Lexer['Mako'], Language['Mako'].lexer
    assert_equal Lexer['Matlab'], Language['Matlab'].lexer
    assert_equal Lexer['Moocode'], Language['Moocode'].lexer
    assert_equal Lexer['MuPAD'], Language['mupad'].lexer
    assert_equal Lexer['Myghty'], Language['Myghty'].lexer
    assert_equal Lexer['NASM'], Language['Assembly'].lexer
    assert_equal Lexer['Nimrod'], Language['Nimrod'].lexer
    assert_equal Lexer['NumPy'], Language['NumPy'].lexer
    assert_equal Lexer['OCaml'], Language['F#'].lexer
    assert_equal Lexer['OCaml'], Language['OCaml'].lexer
    assert_equal Lexer['OCaml'], Language['Standard ML'].lexer
    assert_equal Lexer['Objective-C'], Language['Objective-C'].lexer
    assert_equal Lexer['Objective-J'], Language['Objective-J'].lexer
    assert_equal Lexer['Ooc'], Language['ooc'].lexer
    assert_equal Lexer['PHP'], Language['PHP'].lexer
    assert_equal Lexer['Perl'], Language['Perl'].lexer
    assert_equal Lexer['Python Traceback'], Language['Python traceback'].lexer
    assert_equal Lexer['Python'], Language['Python'].lexer
    assert_equal Lexer['REBOL'], Language['Rebol'].lexer
    assert_equal Lexer['RHTML'], Language['HTML+ERB'].lexer
    assert_equal Lexer['RHTML'], Language['RHTML'].lexer
    assert_equal Lexer['Raw token data'], Language['Raw token data'].lexer
    assert_equal Lexer['Redcode'], Language['Redcode'].lexer
    assert_equal Lexer['Ruby'], Language['Mirah'].lexer
    assert_equal Lexer['Ruby'], Language['Ruby'].lexer
    assert_equal Lexer['S'], Language['R'].lexer
    assert_equal Lexer['SQL'], Language['SQL'].lexer
    assert_equal Lexer['Sass'], Language['Sass'].lexer
    assert_equal Lexer['Scala'], Language['Scala'].lexer
    assert_equal Lexer['Scheme'], Language['Emacs Lisp'].lexer
    assert_equal Lexer['Scheme'], Language['Nu'].lexer
    assert_equal Lexer['Scheme'], Language['Racket'].lexer
    assert_equal Lexer['Scheme'], Language['Scheme'].lexer
    assert_equal Lexer['Smalltalk'], Language['Smalltalk'].lexer
    assert_equal Lexer['Smarty'], Language['Smarty'].lexer
    assert_equal Lexer['Tcl'], Language['Tcl'].lexer
    assert_equal Lexer['Tcsh'], Language['Tcsh'].lexer
    assert_equal Lexer['TeX'], Language['TeX'].lexer
    assert_equal Lexer['Text only'], Language['Text'].lexer
    assert_equal Lexer['Vala'], Language['Vala'].lexer
    assert_equal Lexer['VimL'], Language['VimL'].lexer
    assert_equal Lexer['XML'], Language['XML'].lexer
    assert_equal Lexer['XQuery'], Language['XQuery'].lexer
    assert_equal Lexer['YAML'], Language['YAML'].lexer
    assert_equal Lexer['aspx-vb'], Language['ASP'].lexer
    assert_equal Lexer['c-objdump'], Language['C-ObjDump'].lexer
    assert_equal Lexer['cpp-objdump'], Language['Cpp-ObjDump'].lexer
    assert_equal Lexer['haXe'], Language['HaXe'].lexer
    assert_equal Lexer['objdump'], Language['ObjDump'].lexer
    assert_equal Lexer['reStructuredText'], Language['reStructuredText'].lexer

    # Missing Pygments lexers
    assert_equal Lexer['Text only'], Language['Eiffel'].lexer
    assert_equal Lexer['Text only'], Language['Markdown'].lexer
    assert_equal Lexer['Text only'], Language['Max/MSP'].lexer
    assert_equal Lexer['Text only'], Language['Parrot Internal Representation'].lexer
    assert_equal Lexer['Text only'], Language['Pure Data'].lexer
    assert_equal Lexer['Text only'], Language['Self'].lexer
    assert_equal Lexer['Text only'], Language['SuperCollider'].lexer
    assert_equal Lexer['Text only'], Language['Textile'].lexer
    assert_equal Lexer['Text only'], Language['VHDL'].lexer
    assert_equal Lexer['Text only'], Language['Verilog'].lexer
    assert_equal Lexer['Text only'], Language['Visual Basic'].lexer
    assert_equal Lexer['Text only'], Language['Arc'].lexer
  end

  def test_find_by_alias
    # Add an assertion to this list if you add/change any aliases
    # in languages.yml. Please keep this list alphabetized.
    assert_equal Language['ASP'], Language.find_by_alias('asp')
    assert_equal Language['ASP'], Language.find_by_alias('aspx')
    assert_equal Language['ASP'], Language.find_by_alias('aspx-vb')
    assert_equal Language['ActionScript'], Language.find_by_alias('as3')
    assert_equal Language['Assembly'], Language.find_by_alias('nasm')
    assert_equal Language['Batchfile'], Language.find_by_alias('bat')
    assert_equal Language['C#'], Language.find_by_alias('c#')
    assert_equal Language['C#'], Language.find_by_alias('csharp')
    assert_equal Language['C'], Language.find_by_alias('c')
    assert_equal Language['C++'], Language.find_by_alias('c++')
    assert_equal Language['C++'], Language.find_by_alias('cpp')
    assert_equal Language['ChucK'], Language.find_by_alias('chuck')
    assert_equal Language['ColdFusion'], Language.find_by_alias('cfm')
    assert_equal Language['Common Lisp'], Language.find_by_alias('common-lisp')
    assert_equal Language['Common Lisp'], Language.find_by_alias('lisp')
    assert_equal Language['Darcs Patch'], Language.find_by_alias('dpatch')
    assert_equal Language['Diff'], Language.find_by_alias('diff')
    assert_equal Language['Emacs Lisp'], Language.find_by_alias('elisp')
    assert_equal Language['Emacs Lisp'], Language.find_by_alias('emacs-lisp')
    assert_equal Language['F#'], Language.find_by_alias('f#')
    assert_equal Language['Gettext Catalog'], Language.find_by_alias('pot')
    assert_equal Language['Groovy'], Language.find_by_alias('groovy')
    assert_equal Language['HTML'], Language.find_by_alias('html')
    assert_equal Language['HTML+ERB'], Language.find_by_alias('html+erb')
    assert_equal Language['Haskell'], Language.find_by_alias('haskell')
    assert_equal Language['IRC log'], Language.find_by_alias('irc')
    assert_equal Language['JSON'], Language.find_by_alias('json')
    assert_equal Language['Java Server Pages'], Language.find_by_alias('jsp')
    assert_equal Language['Java'], Language.find_by_alias('java')
    assert_equal Language['JavaScript'], Language.find_by_alias('javascript')
    assert_equal Language['JavaScript'], Language.find_by_alias('js')
    assert_equal Language['Literate Haskell'], Language.find_by_alias('lhs')
    assert_equal Language['Literate Haskell'], Language.find_by_alias('literate-haskell')
    assert_equal Language['Makefile'], Language.find_by_alias('makefile')
    assert_equal Language['Markdown'], Language.find_by_alias('markdown')
    assert_equal Language['Max/MSP'], Language.find_by_alias('max/msp')
    assert_equal Language['Nu'], Language.find_by_alias('nu')
    assert_equal Language['OCaml'], Language.find_by_alias('ocaml')
    assert_equal Language['PHP'], Language.find_by_alias('php')
    assert_equal Language['Parrot Internal Representation'], Language.find_by_alias('pir')
    assert_equal Language['Perl'], Language.find_by_alias('perl')
    assert_equal Language['Pure Data'], Language.find_by_alias('pure-data')
    assert_equal Language['Python traceback'], Language.find_by_alias('pytb')
    assert_equal Language['Python'], Language.find_by_alias('python')
    assert_equal Language['Raw token data'], Language.find_by_alias('raw')
    assert_equal Language['Ruby'], Language.find_by_alias('rb')
    assert_equal Language['Ruby'], Language.find_by_alias('ruby')
    assert_equal Language['Scheme'], Language.find_by_alias('scheme')
    assert_equal Language['Shell'], Language.find_by_alias('bash')
    assert_equal Language['Shell'], Language.find_by_alias('sh')
    assert_equal Language['Shell'], Language.find_by_alias('shell')
    assert_equal Language['Shell'], Language.find_by_alias('zsh')
    assert_equal Language['TeX'], Language.find_by_alias('tex')
    assert_equal Language['Textile'], Language.find_by_alias('textile')
    assert_equal Language['VimL'], Language.find_by_alias('vim')
    assert_equal Language['VimL'], Language.find_by_alias('viml')
    assert_equal Language['XML'], Language.find_by_alias('xml')
    assert_equal Language['XS'], Language.find_by_alias('xs')
    assert_equal Language['YAML'], Language.find_by_alias('yaml')
    assert_equal Language['reStructuredText'], Language.find_by_alias('rst')
  end

  # Used for code search indexing. Changing any of these values may
  # require reindexing repositories.
  def test_search_term
    assert_equal 'perl',        Language['Perl'].search_term
    assert_equal 'python',      Language['Python'].search_term
    assert_equal 'ruby',        Language['Ruby'].search_term
    assert_equal 'common-lisp', Language['Common Lisp'].search_term
    assert_equal 'html+erb',    Language['HTML+ERB'].search_term
    assert_equal 'max/msp',     Language['Max/MSP'].search_term
    assert_equal 'pure-data',   Language['Pure Data'].search_term

    assert_equal 'aspx-vb',       Language['ASP'].search_term
    assert_equal 'as3',           Language['ActionScript'].search_term
    assert_equal 'nasm',          Language['Assembly'].search_term
    assert_equal 'bat',           Language['Batchfile'].search_term
    assert_equal 'csharp',        Language['C#'].search_term
    assert_equal 'cpp',           Language['C++'].search_term
    assert_equal 'chuck',         Language['ChucK'].search_term
    assert_equal 'cfm',           Language['ColdFusion'].search_term
    assert_equal 'dpatch',        Language['Darcs Patch'].search_term
    assert_equal 'emacs-lisp',    Language['Emacs Lisp'].search_term
    assert_equal 'ocaml',         Language['F#'].search_term
    assert_equal 'gentoo-ebuild', Language['Gentoo Ebuild'].search_term
    assert_equal 'gentoo-eclass', Language['Gentoo Eclass'].search_term
    assert_equal 'pot',           Language['Gettext Catalog'].search_term
    assert_equal 'irc',           Language['IRC log'].search_term
    assert_equal 'groovy',        Language['Groovy'].search_term
    assert_equal 'javascript',    Language['JSON'].search_term
    assert_equal 'lhs',           Language['Literate Haskell'].search_term
    assert_equal 'ruby',          Language['Mirah'].search_term
    assert_equal 'nu',            Language['Nu'].search_term
    assert_equal 'pir',           Language['Parrot Internal Representation'].search_term
    assert_equal 'pytb',          Language['Python traceback'].search_term
    assert_equal 'raw',           Language['Raw token data'].search_term
    assert_equal 'bash',          Language['Shell'].search_term
    assert_equal 'vim',           Language['VimL'].search_term
    assert_equal 'xs',            Language['XS'].search_term
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

  def test_common
    assert Language['Perl'].common?
    assert Language['Python'].common?
    assert Language['Ruby'].common?
    assert !Language['Brainfuck'].common?
    assert !Language['Makefile'].common?
  end

  def test_searchable
    assert Language['Ruby'].searchable?
    assert !Language['Gettext Catalog'].searchable?
    assert !Language['SQL'].searchable?
  end

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
    assert_equal Language['Groff'], Language.find_by_extension('man')
    assert_equal Language['Groff'], Language.find_by_extension('1')
    assert_equal Language['Groff'], Language.find_by_extension('2')
    assert_equal Language['Groff'], Language.find_by_extension('3')
    assert_equal Language['PHP'], Language.find_by_extension('php')
    assert_equal Language['PHP'], Language.find_by_extension('php3')
    assert_equal Language['PHP'], Language.find_by_extension('php4')
    assert_equal Language['PHP'], Language.find_by_extension('php5')
    assert_nil Language.find_by_extension('.kt')
  end

  def test_find_all_by_extension
    Language.all.each do |language|
      language.extensions.each do |extension|
        assert_equal language, Language.find_by_extension(extension)
      end
    end
  end

  def test_find_by_filename
    assert_equal Language['Ruby'], Language.find_by_filename('foo.rb')
    assert_equal Language['Ruby'], Language.find_by_filename('foo/bar.rb')
    assert_equal Language['Ruby'], Language.find_by_filename('Rakefile')
    assert_nil Language.find_by_filename('rb')
    assert_nil Language.find_by_filename('.rb')
    assert_nil Language.find_by_filename('.kt')
  end

  def test_find
    assert_equal 'Ruby', Language['Ruby'].name
    assert_equal 'Ruby', Language['ruby'].name
    assert_equal 'Ruby', Language['RUBY'].name
    assert_equal 'C++', Language['C++'].name
    assert_equal 'C++', Language['c++'].name
    assert_equal 'C++', Language['cpp'].name
    assert_equal 'C#', Language['C#'].name
    assert_equal 'C#', Language['c#'].name
    assert_equal 'C#', Language['csharp'].name
    assert_equal 'C#', Language['CSHARP'].name
    assert_equal 'Text', Language['defunkt'].name
  end

  def test_name
    assert_equal 'Perl',   Language['Perl'].name
    assert_equal 'Python', Language['Python'].name
    assert_equal 'Ruby',   Language['Ruby'].name
  end

  def test_error_without_name
    assert_raise ArgumentError do
      Language.new :name => nil
    end
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


  if Lexer.has_pygments?
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
end
