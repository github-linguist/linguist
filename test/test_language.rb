require_relative "./helper"

class TestLanguage < Minitest::Test
  include Linguist

  def test_find_by_alias
    assert_equal Language['ASP.NET'], Language.find_by_alias('aspx')
    assert_equal Language['ASP.NET'], Language.find_by_alias('aspx-vb')
    assert_equal Language['ActionScript'], Language.find_by_alias('as3')
    assert_equal Language['ApacheConf'], Language.find_by_alias('apache')
    assert_equal Language['Assembly'], Language.find_by_alias('nasm')
    assert_equal Language['Batchfile'], Language.find_by_alias('bat')
    assert_equal Language['C#'], Language.find_by_alias('c#')
    assert_equal Language['C#'], Language.find_by_alias('csharp')
    assert_equal Language['C'], Language.find_by_alias('c')
    assert_equal Language['C++'], Language.find_by_alias('c++')
    assert_equal Language['C++'], Language.find_by_alias('cpp')
    assert_equal Language['Chapel'], Language.find_by_alias('chpl')
    assert_equal Language['Classic ASP'], Language.find_by_alias('asp')
    assert_equal Language['CoffeeScript'], Language.find_by_alias('coffee')
    assert_equal Language['CoffeeScript'], Language.find_by_alias('coffee-script')
    assert_equal Language['ColdFusion'], Language.find_by_alias('cfm')
    assert_equal Language['Common Lisp'], Language.find_by_alias('common-lisp')
    assert_equal Language['Common Lisp'], Language.find_by_alias('lisp')
    assert_equal Language['Darcs Patch'], Language.find_by_alias('dpatch')
    assert_equal Language['Dart'], Language.find_by_alias('dart')
    assert_equal Language['EdgeQL'], Language.find_by_alias('esdl')
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
    assert_equal Language['R'], Language.find_by_alias('r')
    assert_equal Language['Scheme'], Language.find_by_alias('scheme')
    assert_equal Language['Shell'], Language.find_by_alias('bash')
    assert_equal Language['Shell'], Language.find_by_alias('sh')
    assert_equal Language['Shell'], Language.find_by_alias('shell')
    assert_equal Language['Shell'], Language.find_by_alias('zsh')
    assert_equal Language['SuperCollider'], Language.find_by_alias('supercollider')
    assert_equal Language['TeX'], Language.find_by_alias('tex')
    assert_equal Language['Tree-sitter Query'], Language.find_by_alias('tsq')
    assert_equal Language['TypeScript'], Language.find_by_alias('ts')
    assert_equal Language['Vim Script'], Language.find_by_alias('vim')
    assert_equal Language['Vim Script'], Language.find_by_alias('viml')
    assert_equal Language['reStructuredText'], Language.find_by_alias('rst')
    assert_equal Language['X BitMap'], Language.find_by_alias('xbm')
    assert_equal Language['X PixMap'], Language.find_by_alias('xpm')
    assert_equal Language['YAML'], Language.find_by_alias('yml')
    assert_nil Language.find_by_alias(nil)
  end

  # Note these are set by `script/update-ids`. If these tests fail then someone
  # has changed the `language_id` fields set in languages.yml which is almost certainly
  # not what you want to happen (these fields are used in GitHub's search indexes)
  def test_language_ids
    assert_equal 4, Language['ANTLR'].language_id
    assert_equal 54, Language['Ceylon'].language_id
    assert_equal 326, Language['Ruby'].language_id
    assert_equal 421, Language['xBase'].language_id
  end

  def test_find_by_id
    assert_equal Language['Elixir'], Language.find_by_id(100)
    assert_equal Language['Ruby'], Language.find_by_id(326)
    assert_equal Language['xBase'], Language.find_by_id(421)
  end

  def test_groups
    # Test a couple identity cases
    assert_equal Language['Perl'], Language['Perl'].group
    assert_equal Language['Python'], Language['Python'].group
    assert_equal Language['Ruby'], Language['Ruby'].group

    # Test a few special groups
    assert_equal Language['Assembly'], Language['Unix Assembly'].group
    assert_equal Language['C'], Language['OpenCL'].group
    assert_equal Language['Haskell'], Language['Literate Haskell'].group
    assert_equal Language['Java'], Language['Java Server Pages'].group
    assert_equal Language['Python'], Language['NumPy'].group
    assert_equal Language['Shell'], Language['Gentoo Ebuild'].group
    assert_equal Language['Shell'], Language['Gentoo Eclass'].group
    assert_equal Language['Shell'], Language['Tcsh'].group

    # Ensure everyone has a group
    Language.all.each do |language|
      assert language.group, "#{language} has no group"
    end
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
    assert_equal :programming, Language['LSL'].type
    assert_equal :programming, Language['Perl'].type
    assert_equal :programming, Language['PowerShell'].type
    assert_equal :programming, Language['Python'].type
    assert_equal :programming, Language['Ruby'].type
    assert_equal :programming, Language['TypeScript'].type
    assert_equal :programming, Language['Makefile'].type
    assert_equal :programming, Language['SuperCollider'].type
  end

  def test_markup
    assert_equal :markup, Language['HTML'].type
    assert_equal :markup, Language['SCSS'].type
  end

  def test_data
    assert_equal :data, Language['YAML'].type
  end

  def test_prose
    assert_equal :prose, Language['Markdown'].type
    assert_equal :prose, Language['Org'].type
  end

  def test_find_by_name
    assert_nil Language.find_by_name(nil)
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

  def test_find_by_extension
    assert_equal [], Language.find_by_extension('.factor-rc')
    assert_equal [Language['Limbo'], Language['M'], Language['MATLAB'], Language['MUF'], Language['Mathematica'], Language['Mercury'], Language['Objective-C']], Language.find_by_extension('foo.m')
    assert_equal [Language['Ruby']], Language.find_by_extension('foo.rb')
    assert_equal [Language['Ruby']], Language.find_by_extension('foo/bar.rb')
    assert_equal [Language['Ruby']], Language.find_by_extension('PKGBUILD.rb')
    assert_equal ['C', 'C++', 'Objective-C'], Language.find_by_extension('foo.h').map(&:name).sort
    assert_equal [], Language.find_by_extension('rb')
    assert_equal [], Language.find_by_extension('.null')
    assert_equal [Language['Jinja']], Language.find_by_extension('index.jinja')
    assert_equal [Language['Chapel']], Language.find_by_extension('examples/hello.chpl')
    assert_equal [], Language.find_by_filename('F.I.L.E.')
  end

  def test_find_all_by_extension
    Language.all.each do |language|
      language.extensions.each do |extension|
        assert_includes Language.find_by_extension(extension), language
      end
    end
  end

  def test_find_by_filename
    assert_equal [Language['Shell']], Language.find_by_filename('PKGBUILD')
    assert_equal [Language['Ruby']], Language.find_by_filename('Rakefile')
    assert_equal Language['ApacheConf'], Language.find_by_filename('httpd.conf').first
    assert_equal [Language['ApacheConf']], Language.find_by_filename('.htaccess')
    assert_equal Language['Nginx'], Language.find_by_filename('nginx.conf').first
    assert_equal [], Language.find_by_filename('foo.rb')
    assert_equal [], Language.find_by_filename('rb')
    assert_equal [], Language.find_by_filename('.null')
    assert_equal [Language['Shell']], Language.find_by_filename('.bashrc')
    assert_equal [Language['Shell']], Language.find_by_filename('bash_profile')
    assert_equal [Language['Shell']], Language.find_by_filename('.zshrc')
    assert_equal [Language['Clojure']], Language.find_by_filename('riemann.config')
  end

  def test_find_by_interpreter
    {
      "ruby" => "Ruby",
      "Rscript" => "R",
      "sh" => "Shell",
      "bash" => "Shell",
      "python" => "Python",
      "python2" => "Python",
      "python3" => "Python",
      "sbcl" => "Common Lisp",
      "sclang" => "SuperCollider"
    }.each do |interpreter, language|
      assert_equal [Language[language]], Language.find_by_interpreter(interpreter)
    end

    assert_equal [], Language.find_by_interpreter(nil)
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
    assert_nil Language[nil]
  end

  def test_find_ignores_case
    assert_equal 'AGS Script', Language['ags script'].name
    assert_equal 'AGS Script', Language['ags sCRIPT'].name
  end

  def test_find_by_name_ignores_case
    assert_equal 'AGS Script', Language.find_by_name('ags script').name
    assert_equal 'AGS Script', Language.find_by_name('ags sCRIPT').name
  end

  def test_find_by_alias_ignores_case
    refute_includes Language['AGS Script'].aliases, 'AGS'
    assert_equal 'AGS Script', Language.find_by_alias('AGS').name
  end

  def test_find_ignores_comma
    assert_equal 'Rust', Language['rust,no_run'].name
  end

  def test_find_by_name_ignores_comma
    assert_equal Language['Rust'], Language.find_by_name('rust,no_run')
  end

  def test_find_by_alias_ignores_comma
    assert_equal Language['Rust'], Language.find_by_alias('rust,no_run')
  end

  def test_doesnt_blow_up_with_blank_lookup
    assert_nil Language.find_by_alias('')
    assert_nil Language.find_by_name(nil)
    assert_nil Language[""]
  end

  def test_does_not_blow_up_with_non_string_lookup
    assert_nil Language.find_by_alias(true)
    assert_nil Language.find_by_name(true)
    assert_nil Language[true]
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
    assert_raises ArgumentError do
      Language.new :name => nil
    end
  end

  def test_color
    assert_equal '#701516', Language['Ruby'].color
    assert_equal '#3572A5', Language['Python'].color
    assert_equal '#f1e05a', Language['JavaScript'].color
    assert_equal '#3178c6', Language['TypeScript'].color
    assert_equal '#3d9970', Language['LSL'].color
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
    assert_equal 'lsl', Language['LSL'].ace_mode
    assert_equal 'javascript', Language['JavaScript'].ace_mode
    assert_equal 'fortran', Language['FORTRAN'].ace_mode
  end

  def test_codemirror_mode
    assert_equal 'ruby', Language['Ruby'].codemirror_mode
    assert_equal 'javascript', Language['JavaScript'].codemirror_mode
    assert_equal 'clike', Language['C'].codemirror_mode
    assert_equal 'clike', Language['C++'].codemirror_mode
  end

  def test_codemirror_mime_type
    assert_equal 'text/x-ruby', Language['Ruby'].codemirror_mime_type
    assert_equal 'text/javascript', Language['JavaScript'].codemirror_mime_type
    assert_equal 'text/x-csrc', Language['C'].codemirror_mime_type
    assert_equal 'text/x-c++src', Language['C++'].codemirror_mime_type
  end

  def test_wrap
    assert_equal false, Language['C'].wrap
    assert_equal true, Language['Markdown'].wrap
  end

  def test_extensions
    assert Language['LSL'].extensions.include?('.lsl')
    assert Language['Perl'].extensions.include?('.pl')
    assert Language['Python'].extensions.include?('.py')
    assert Language['Ruby'].extensions.include?('.rb')
    assert Language['SuperCollider'].extensions.include?('.scd')
  end

  def test_eql
    assert Language['Ruby'].eql?(Language['Ruby'])
    assert !Language['Ruby'].eql?(Language['Python'])
  end

  def test_by_type
    assert !Language.by_type(:prose).nil?
  end

  def test_all_languages_have_grammars
    scopes = YAML.load(File.read(File.expand_path("../../grammars.yml", __FILE__))).values.flatten
    missing = Language.all.reject { |language| language.tm_scope == "none" || scopes.include?(language.tm_scope) }
    message = "The following languages' scopes are not listed in grammars.yml. Please add grammars for all new languages.\n"
    message += "If no grammar exists for a language, mark the language with `tm_scope: none` in lib/linguist/languages.yml.\n"

    width = missing.map { |language| language.name.length }.max
    message += missing.map { |language| sprintf("%-#{width}s %s", language.name, language.tm_scope) }.sort.join("\n")
    assert missing.empty?, message
  end

  def test_all_languages_have_scopes
    languages = YAML.load(File.read(File.expand_path("../../lib/linguist/languages.yml", __FILE__)))
    missing = languages.reject { |name,language| language.has_key?('tm_scope') }
    message = "The following languages do not have a `tm_scope` field defined. Use `tm_scope: none` if the language has no grammar.\n"
    message += missing.keys.sort.join("\n")
    assert missing.empty?, message
  end

  def test_all_languages_have_type
    missing = Language.all.select { |language| language.type.nil? }
    message = "The following languages do not have a type listed in grammars.yml. Please add types for all new languages.\n"

    width = missing.map { |language| language.name.length }.max
    message += missing.map { |language| sprintf("%-#{width}s", language.name) }.sort.join("\n")
    assert missing.empty?, message
  end

  def test_all_languages_have_a_language_id_set
    missing = Language.all.select { |language| language.language_id.nil? }

    message = "The following languages do not have a language_id listed in languages.yml. Please run `script/update-ids` as per the contribution guidelines.\n"
    missing.each { |language| message << "#{language.name}\n" }
    assert missing.empty?, message
  end

  def test_all_languages_have_a_valid_id
    deleted_language_ids = [21]  # Prevent re-use of deleted language IDs
    invalid = Language.all.select { |language| language.language_id < 0 || deleted_language_ids.include?(language.language_id) || (language.language_id > 431 && language.language_id < 1024) || language.language_id >= (2**31 - 1) }

    message = "The following languages do not have a valid language_id. Please run `script/update-ids` as per the contribution guidelines.\n"
    invalid.each { |language| message << "#{language.name}\n" }
    assert invalid.empty?, message
  end

  def test_all_language_id_are_unique
    duplicates = Language.all.group_by{ |language| language.language_id }.select { |k, v| v.size > 1 }.map(&:first)

    message = "The following language_id are used several times in languages.yml. Please run `script/update-ids` as per the contribution guidelines.\n"
    duplicates.each { |language_id| message << "#{language_id}\n" }
    assert duplicates.empty?, message
  end

  def test_all_languages_have_a_valid_ace_mode
    ace_fixture_path = File.join('test', 'fixtures', 'ace_modes.json')
    skip("No ace_modes.json file") unless File.exist?(ace_fixture_path)

    ace_modes = Yajl.load(File.read(ace_fixture_path))
    ace_github_modes = ace_modes[0].concat(ace_modes[1])
    existing_ace_modes = ace_github_modes.map do |ace_github_mode|
      File.basename(ace_github_mode["name"], ".js") if ace_github_mode["name"]  !~ /_highlight_rules|_test|_worker/
    end.compact.uniq.sort.map(&:downcase)

    missing = Language.all.reject { |language| language.ace_mode == "text" || existing_ace_modes.include?(language.ace_mode) }
    message = "The following languages do not have an Ace mode listed in languages.yml. Please add an Ace mode for all new languages.\n"
    message += "If no Ace mode exists for a language, mark the language with `ace_mode: text` in lib/linguist/languages.yml.\n"

    width = missing.map { |language| language.name.length }.max
    message += missing.map { |language| sprintf("%-#{width}s %s", language.name, language.ace_mode) }.sort.join("\n")
    assert missing.empty?, message
  end

  def test_codemirror_modes_present
    Language.all.each do |language|
      if language.codemirror_mode || language.codemirror_mime_type
        assert language.codemirror_mode, "#{language.inspect} missing CodeMirror mode"
        assert language.codemirror_mime_type, "#{language.inspect} missing CodeMirror MIME mode"
      end
    end
  end

  def test_valid_codemirror_mode
    Language.all.each do |language|
      if mode = language.codemirror_mode
        assert File.exist?(File.expand_path("../../vendor/CodeMirror/mode/#{mode}", __FILE__)), "#{mode} isn't a valid CodeMirror mode"
      end
    end
  end

  def test_codemirror_mode_and_mime_defined_by_meta_mapping
    meta = File.read(File.expand_path("../../vendor/CodeMirror/mode/meta.js", __FILE__))
    Language.all.each do |language|
      next unless language.codemirror_mode && language.codemirror_mime_type
      assert meta.match(/^.+#{Regexp.escape(language.codemirror_mime_type)}.+#{Regexp.escape(language.codemirror_mode)}.+$/), "#{language.inspect}: #{language.codemirror_mime_type} not defined under #{language.codemirror_mode}"
    end
  end

  def test_codemirror_mime_declared_in_mode_file
    Language.all.each do |language|
      next unless language.codemirror_mode && language.codemirror_mime_type
      filename = File.expand_path("../../vendor/CodeMirror/mode/#{language.codemirror_mode}/#{language.codemirror_mode}.js", __FILE__)
      assert File.exist?(filename), "#{filename} does not exist"
      assert File.read(filename).match(Regexp.escape language.codemirror_mime_type), "#{language.inspect}: #{language.codemirror_mime_type} not defined in #{filename}"
    end
  end

  def test_all_popular_languages_exist
    popular = YAML.load(File.read(File.expand_path("../../lib/linguist/popular.yml", __FILE__)))

    missing = popular - Language.all.map(&:name)
    message = "The following languages are listed in lib/linguist/popular.yml but not in lib/linguist/languages.yml.\n"
    message += missing.sort.join("\n")
    assert missing.empty?, message
  end

  def test_non_crash_on_comma
    assert_nil Language[',']
    assert_nil Language.find_by_name(',')
    assert_nil Language.find_by_alias(',')
  end

  def test_detect_prefers_markdown_for_md
    blob = Linguist::FileBlob.new(File.join(samples_path, "Markdown/symlink.md"))
    match = Linguist.detect(blob)
    assert_equal Language["Markdown"], match
  end

  def test_fs_names
    Language.all.each do |language|
      next unless /[\\:*?"<>|]/.match(language.name)
      assert language.fs_name, "#{language.name} needs an fs_name for Windows' file system."
      assert !/[\\:*?"<>|]/.match(language.fs_name), "The fs_name for #{language.name} is invalid."
    end
  end
end
