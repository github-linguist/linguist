require 'linguist/file_blob'

require 'test/unit'
require 'mime/types'
require 'pygments'

class TestBlob < Test::Unit::TestCase
  include Linguist

  Lexer = Pygments::Lexer

  def fixtures_path
    File.expand_path("../fixtures", __FILE__)
  end

  def blob(name)
    FileBlob.new(File.join(fixtures_path, name), fixtures_path)
  end

  def script_blob(name)
    blob = blob(name)
    blob.instance_variable_set(:@name, 'script')
    blob
  end

  def test_name
    assert_equal "foo.rb", blob("foo.rb").name
  end

  def test_pathname
    assert_equal Pathname.new("foo.rb"), blob("foo.rb").pathname
  end

  def test_mime_type
    assert_equal "application/octet-stream", blob("dog.o").mime_type
    assert_equal "application/postscript", blob("octocat.ai").mime_type
    assert_equal "application/x-ruby", blob("grit.rb").mime_type
    assert_equal "application/x-sh", blob("script.sh").mime_type
    assert_equal "application/xml", blob("bar.xml").mime_type
    assert_equal "text/plain", blob("README").mime_type
  end

  def test_content_type
    assert_equal "application/octet-stream", blob("dog.o").content_type
    assert_equal "application/pdf", blob("foo.pdf").content_type
    assert_equal "image/png", blob("foo.png").content_type
    assert_equal "text/plain; charset=iso-8859-2", blob("README").content_type
    assert_equal "text/plain; charset=iso-8859-1", blob("script.pl").content_type
    assert_equal "text/plain; charset=iso-8859-1", blob("script.py").content_type
    assert_equal "text/plain; charset=iso-8859-1", blob("script.rb").content_type
    assert_equal "text/plain; charset=iso-8859-1", blob("script.sh").content_type
  end

  def test_disposition
    assert_equal "attachment; filename=foo+bar.jar", blob("foo bar.jar").disposition
    assert_equal "attachment; filename=foo.bin", blob("foo.bin").disposition
    assert_equal "attachment; filename=linguist.gem", blob("pkg/linguist.gem").disposition
    assert_equal "attachment; filename=octocat.ai", blob("octocat.ai").disposition
    assert_equal "inline", blob("README").disposition
    assert_equal "inline", blob("foo.txt").disposition
    assert_equal "inline", blob("grit.rb").disposition
    assert_equal "inline", blob("octocat.png").disposition
  end

  def test_data
    assert_equal "module Foo\nend\n", blob("foo.rb").data
  end

  def test_lines
    assert_equal ["module Foo", "end", ""], blob("foo.rb").lines
  end

  def test_size
    assert_equal 15, blob("foo.rb").size
  end

  def test_loc
    assert_equal 3, blob("foo.rb").loc
  end

  def test_sloc
    assert_equal 2, blob("foo.rb").sloc
  end

  def test_encoding
    assert_equal "ISO-8859-2", blob("README").encoding
    assert_equal "ISO-8859-1", blob("dump.sql").encoding
    assert_equal "UTF-8", blob("foo.txt").encoding
    assert_nil blob("dog.o").encoding
  end

  def test_binary
    # Large blobs aren't loaded
    large_blob = blob("git.exe")
    large_blob.instance_eval do
      def data; end
    end
    assert large_blob.binary?

    assert blob("git.deb").binary?
    assert blob("git.exe").binary?
    assert blob("hello.pbc").binary?
    assert blob("linguist.gem").binary?
    assert blob("octocat.ai").binary?
    assert blob("octocat.png").binary?
    assert blob("zip").binary?
    assert !blob("README").binary?
    assert !blob("file.txt").binary?
    assert !blob("foo.rb").binary?
    assert !blob("script.pl").binary?
  end

  def test_text
    assert blob("README").text?
    assert blob("dump.sql").text?
    assert blob("file.json").text?
    assert blob("file.txt").text?
    assert blob("md").text?
    assert blob("script.sh").text?
    assert blob("tender.md").text?
    assert blob("txt").text?
  end

  def test_image
    assert blob("octocat.gif").image?
    assert blob("octocat.jpeg").image?
    assert blob("octocat.jpg").image?
    assert blob("octocat.png").image?
    assert !blob("octocat.ai").image?
    assert !blob("octocat.psd").image?
  end

  def test_viewable
    assert blob("README").viewable?
    assert blob("foo.rb").viewable?
    assert blob("script.pl").viewable?
    assert !blob("linguist.gem").viewable?
    assert !blob("octocat.ai").viewable?
    assert !blob("octocat.png").viewable?
  end

  def test_generated
    assert !blob("README").generated?
    assert blob("MainMenu.xib").generated?
    assert blob("MainMenu.nib").generated?
    assert blob("project.pbxproj").generated?

    # Generated .NET Docfiles
    assert blob("net_docfile.xml").generated?

    # Long line
    assert !blob("uglify.js").generated?

    # Inlined JS, but mostly code
    assert !blob("json2_backbone.js").generated?

    # Minified JS
    assert !blob("jquery-1.6.1.js").generated?
    assert blob("jquery-1.6.1.min.js").generated?
    assert blob("jquery-1.4.2.min.js").generated?

    # CoffeScript JS

    # These examples are to basic to tell
    assert !blob("coffee/empty.js").generated?
    assert !blob("coffee/hello.js").generated?

    assert blob("coffee/intro-old.js").generated?
    assert blob("coffee/classes-old.js").generated?

    assert blob("coffee/intro.js").generated?
    assert blob("coffee/classes.js").generated?
  end

  def test_vendored
    assert !blob("README").vendored?

    # Node depedencies
    assert blob("node_modules/coffee-script/lib/coffee-script.js").vendored?

    # Rails vendor/
    assert blob("vendor/plugins/will_paginate/lib/will_paginate.rb").vendored?

    # C deps
    assert blob("deps/http_parser/http_parser.c").vendored?
    assert blob("deps/v8/src/v8.h").vendored?

    # Prototype
    assert !blob("public/javascripts/application.js").vendored?
    assert blob("public/javascripts/prototype.js").vendored?
    assert blob("public/javascripts/effects.js").vendored?
    assert blob("public/javascripts/controls.js").vendored?
    assert blob("public/javascripts/dragdrop.js").vendored?

    # jQuery
    assert blob("jquery.js").vendored?
    assert blob("public/javascripts/jquery.js").vendored?
    assert blob("public/javascripts/jquery.min.js").vendored?
    assert blob("public/javascripts/jquery-1.7.js").vendored?
    assert blob("public/javascripts/jquery-1.7.min.js").vendored?
    assert blob("public/javascripts/jquery-1.5.2.js").vendored?
    assert blob("public/javascripts/jquery-1.6.1.js").vendored?
    assert blob("public/javascripts/jquery-1.6.1.min.js").vendored?
    assert !blob("public/javascripts/jquery.github.menu.js").vendored?

    # MooTools
    assert blob("public/javascripts/mootools-core-1.3.2-full-compat.js").vendored?
    assert blob("public/javascripts/mootools-core-1.3.2-full-compat-yc.js").vendored?

    # Dojo
    assert blob("public/javascripts/dojo.js").vendored?

    # MochiKit
    assert blob("public/javascripts/MochiKit.js").vendored?

    # YUI
    assert blob("public/javascripts/yahoo-dom-event.js").vendored?
    assert blob("public/javascripts/yahoo-min.js").vendored?
    assert blob("public/javascripts/yuiloader-dom-event.js").vendored?

    # LESS
    assert blob("public/javascripts/less-1.1.0.js").vendored?
    assert blob("public/javascripts/less-1.1.0.min.js").vendored?

    # WYS editors
    assert blob("public/javascripts/ckeditor.js").vendored?
    assert blob("public/javascripts/tiny_mce.js").vendored?
    assert blob("public/javascripts/tiny_mce_popup.js").vendored?
    assert blob("public/javascripts/tiny_mce_src.js").vendored?

    # Fabric
    assert blob("fabfile.py").vendored?

    # WAF
    assert blob("waf").vendored?

    # Visual Studio IntelliSense
    assert blob("Scripts/jquery-1.7-vsdoc.js").vendored?

    # Microsoft Ajax
    assert blob("Scripts/MicrosoftAjax.debug.js").vendored?
    assert blob("Scripts/MicrosoftAjax.js").vendored?
    assert blob("Scripts/MicrosoftMvcAjax.debug.js").vendored?
    assert blob("Scripts/MicrosoftMvcAjax.js").vendored?
    assert blob("Scripts/MicrosoftMvcValidation.debug.js").vendored?
    assert blob("Scripts/MicrosoftMvcValidation.js").vendored?

    # jQuery validation plugin (MS bundles this with asp.net mvc)
    assert blob("Scripts/jquery.validate.js").vendored?

    # NuGet Packages
    assert blob("packages/Modernizr.2.0.6/Content/Scripts/modernizr-2.0.6-development-only.js").vendored?
  end

  def test_indexable
    assert blob("file.txt").indexable?
    assert blob("foo.rb").indexable?
    assert !blob("defun.kt").indexable?
    assert !blob("dump.sql").indexable?
    assert !blob("github.po").indexable?
    assert !blob("linguist.gem").indexable?
  end

  def test_language
    assert_equal Language['C'],           blob("hello.c").language
    assert_equal Language['C'],           blob("hello.h").language
    assert_equal Language['C++'],         blob("bar.h").language
    assert_equal Language['C++'],         blob("bar.hpp").language
    assert_equal Language['C++'],         blob("hello.cpp").language
    assert_equal Language['C++'],         blob("cuda.cu").language
    assert_equal Language['GAS'],         blob("hello.s").language
    assert_equal Language['Objective-C'], blob("Foo.h").language
    assert_equal Language['Objective-C'], blob("Foo.m").language
    assert_equal Language['Objective-C'], blob("FooAppDelegate.h").language
    assert_equal Language['Objective-C'], blob("FooAppDelegate.m").language
    assert_equal Language['Objective-C'], blob("hello.m").language
    assert_equal Language['OpenCL'],      blob("fft.cl").language
    assert_equal Language['Ruby'],        blob("foo.rb").language
    assert_equal Language['Ruby'],        blob("script.rb").language
    assert_equal Language['Ruby'],        blob("wrong_shebang.rb").language
    assert_equal Language['Arduino'],     blob("hello.ino").language
    assert_nil blob("octocat.png").language

    # .pl disambiguation
    assert_equal Language['Prolog'],      blob("test-prolog.pl").language
    assert_equal Language['Perl'],        blob("test-perl.pl").language
    assert_equal Language['Perl'],        blob("test-perl2.pl").language

    # .m disambiguation
    assert_equal Language['Objective-C'], blob("Foo.m").language
    assert_equal Language['Objective-C'], blob("hello.m").language
    assert_equal Language['Matlab'], blob("matlab_function.m").language
    assert_equal Language['Matlab'], blob("matlab_script.m").language

    # .r disambiguation
    assert_equal Language['R'],           blob("hello-r.R").language
    assert_equal Language['Rebol'],       blob("hello-rebol.r").language

    # .t disambiguation
    assert_equal Language['Perl'],        blob("perl-test.t").language
    assert_equal Language['Turing'],      blob("turing.t").language

    # ML
    assert_equal Language['OCaml'],       blob("Foo.ml").language
    assert_equal Language['Standard ML'], blob("Foo.sig").language
    assert_equal Language['Standard ML'], blob("Foo.sml").language

    # Config files
    assert_equal Language['INI'],   blob(".gitconfig").language
    assert_equal Language['Shell'], blob(".bash_profile").language
    assert_equal Language['Shell'], blob(".bashrc").language
    assert_equal Language['Shell'], blob(".profile").language
    assert_equal Language['Shell'], blob(".zlogin").language
    assert_equal Language['Shell'], blob(".zshrc").language
    assert_equal Language['VimL'],  blob(".gvimrc").language
    assert_equal Language['VimL'],  blob(".vimrc").language
    assert_equal Language['YAML'],  blob(".gemrc").language

    assert_nil blob("blank").language
    assert_nil blob("README").language

    # https://github.com/xquery/xprocxq/blob/master/src/xquery/xproc.xqm
    assert_equal Language['XQuery'], blob("xproc.xqm").language

    # https://github.com/wycats/osx-window-sizing/blob/master/center.applescript
    assert_equal Language['AppleScript'], blob("center.scpt").language
    assert_equal Language['AppleScript'], blob("center.applescript").language

    # https://github.com/Araq/Nimrod/tree/master/examples
    assert_equal Language['Nimrod'], blob("foo.nim").language

    # http://supercollider.sourceforge.net/
    # https://github.com/drichert/BCR2000.sc/blob/master/BCR2000.sc
    assert_equal Language['SuperCollider'], blob("BCR2000.sc").language

    # https://github.com/harrah/xsbt/wiki/Quick-Configuration-Examples
    assert_equal Language['Scala'], blob('build.sbt').language

    # https://github.com/gradleware/oreilly-gradle-book-examples/blob/master/ant-antbuilder/build.gradle
    assert_equal Language['Groovy'], blob("build.gradle").language

    # http://docs.racket-lang.org/scribble/
    assert_equal Language['Racket'], blob("scribble.scrbl").language

    # https://github.com/drupal/drupal/blob/7.x/modules/php/php.module
    assert_equal Language['PHP'], blob("drupal.module").language

    # https://github.com/googleapi/googleapi/blob/master/demos/gmail_demo/gmail.dpr
    assert_equal Language['Delphi'], blob("program.dpr").language

    # https://github.com/philiplaureano/Nemerle.FizzBuzz/blob/master/FizzBuzz/FizzBuzzer.n
    assert_equal Language['Nemerle'], blob("hello.n").language

    # https://github.com/dharmatech/agave/blob/master/demos/asteroids.sps
    assert_equal Language['Scheme'], blob("asteroids.sps").language

    # https://github.com/graydon/rust
    assert_equal Language['Rust'], blob("hello.rs").language

    # https://github.com/olabini/ioke
    assert_equal Language['Ioke'], blob("hello.ik").language

    # https://github.com/parrot/parrot
    assert_equal Language['Parrot Internal Representation'], blob("hello.pir").language
    assert_equal Language['Parrot Assembly'], blob("hello.pasm").language

    # http://gosu-lang.org
    assert_equal Language['Gosu'], blob("Hello.gsx").language
    assert_equal Language['Gosu'], blob("hello.gsp").language
    assert_equal Language['Gosu'], blob("Hello.gst").language
    assert_equal Language['Gosu'], blob("hello.vark").language

    # Groovy Server Pages
    assert_equal Language['Groovy Server Pages'], blob("bar.gsp").language
    assert_equal Language['Groovy Server Pages'], blob("hello-resources.gsp").language
    assert_equal Language['Groovy Server Pages'], blob("hello-pagedirective.gsp").language
    assert_equal Language['Groovy Server Pages'], blob("hello-var.gsp").language

    # https://github.com/Lexikos/AutoHotkey_L
    assert_equal Language['AutoHotkey'], blob("hello.ahk").language

    # Haml
    assert_equal Language['Haml'], blob("hello.haml").language
    assert_equal Language['HTML'], blob("hello.haml").language.group

    # Sass
    assert_equal Language['Sass'], blob("screen.sass").language
    assert_equal Language['CSS'], blob("screen.sass").language.group
    assert_equal Language['SCSS'], blob("screen.scss").language
    assert_equal Language['CSS'], blob("screen.scss").language.group
  end

  def test_lexer
    assert_equal Lexer['Diff'], blob("dude-thing-okay--001.patch").lexer
    assert_equal Lexer['JavaScript'], blob("dude.js").lexer
    assert_equal Lexer['Ruby'], blob("Capfile").lexer
    assert_equal Lexer['Ruby'], blob("grit.rb").lexer
    assert_equal Lexer['Scheme'], blob("dude.el").lexer
    assert_equal Lexer['Text only'], blob("README").lexer
  end

  def test_shebang_script
    assert_equal 'sh', script_blob("script.sh").shebang_script
    assert_equal 'bash', script_blob("script.bash").shebang_script
    assert_equal 'zsh', script_blob("script.zsh").shebang_script
    assert_equal 'perl', script_blob("script.pl").shebang_script
    assert_equal 'ruby', script_blob("script.rb").shebang_script
    assert_equal 'ruby', script_blob("script2.rb").shebang_script
    assert_equal 'python', script_blob("script.py").shebang_script
    assert_equal 'node', script_blob("script.js").shebang_script
    assert_equal 'groovy', script_blob("script.groovy").shebang_script
    assert_equal 'macruby', script_blob("script.mrb").shebang_script
    assert_equal 'rake', script_blob("script.rake").shebang_script
    assert_equal 'foo', script_blob("script.foo").shebang_script
    assert_equal 'nush', script_blob("script.nu").shebang_script
    assert_equal 'scala', script_blob("script.scala").shebang_script
    assert_equal 'racket', script_blob("script.rkt").shebang_script
    assert_equal nil, script_blob("foo.rb").shebang_script
  end

  def test_shebang_language
    assert_equal Language['Shell'], script_blob("script.sh").shebang_language
    assert_equal Language['Shell'], script_blob("script.bash").shebang_language
    assert_equal Language['Shell'], script_blob("script.zsh").shebang_language
    assert_equal Language['Perl'], script_blob("script.pl").shebang_language
    assert_equal Language['Ruby'], script_blob("script.rb").shebang_language
    assert_equal Language['Python'], script_blob("script.py").shebang_language
    assert_equal Language['JavaScript'], script_blob("script.js").shebang_language
    assert_equal Language['Groovy'], script_blob("script.groovy").shebang_language
    assert_equal Language['Ruby'], script_blob("script.mrb").shebang_language
    assert_equal Language['Ruby'], script_blob("script.rake").shebang_language
    assert_equal Language['Nu'], script_blob("script.nu").shebang_language
    assert_equal Language['Scala'], script_blob("script.scala").shebang_language
    assert_equal Language['Racket'], script_blob("script.rkt").shebang_language
    assert_equal nil, script_blob("script.foo").shebang_language
    assert_equal nil, script_blob("foo.rb").shebang_language
  end

  def test_colorize
    assert_equal <<-HTML, blob("foo.rb").colorize
<div class="highlight"><pre><span class="k">module</span> <span class="nn">Foo</span>
<span class="k">end</span>
</pre>
</div>
    HTML
  end

  def test_colorize_without_wrapper
    assert_equal <<-HTML, blob("foo.rb").colorize_without_wrapper
<span class="k">module</span> <span class="nn">Foo</span>
<span class="k">end</span>
    HTML
  end

  def test_colorize_skips_minified_files
    assert_nil blob("jquery-1.6.1.min.js").colorize
  end
end
