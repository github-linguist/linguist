require 'linguist/file_blob'
require 'linguist/sample'

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
    name = File.join(fixtures_path, name) unless name =~ /^\//
    FileBlob.new(name, fixtures_path)
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
    assert_equal "application/octet-stream", blob("binary/dog.o").mime_type
    assert_equal "application/ogg", blob("binary/foo.ogg").mime_type
    assert_equal "application/postscript", blob("binary/octocat.ai").mime_type
    assert_equal "application/x-ruby", blob("ruby/grit.rb").mime_type
    assert_equal "application/x-sh", blob("shell/script.sh").mime_type
    assert_equal "application/xml", blob("xml/bar.xml").mime_type
    assert_equal "text/plain", blob("text/README").mime_type
  end

  def test_content_type
    assert_equal "application/octet-stream", blob("binary/dog.o").content_type
    assert_equal "application/ogg", blob("binary/foo.ogg").content_type
    assert_equal "application/pdf", blob("binary/foo.pdf").content_type
    assert_equal "image/png", blob("binary/foo.png").content_type
    assert_equal "text/plain; charset=iso-8859-2", blob("text/README").content_type
    assert_equal "text/plain; charset=iso-8859-1", blob("perl/script.pl").content_type
    assert_equal "text/plain; charset=iso-8859-1", blob("python/script.py").content_type
    assert_equal "text/plain; charset=iso-8859-1", blob("ruby/script.rb").content_type
    assert_equal "text/plain; charset=iso-8859-1", blob("shell/script.sh").content_type
  end

  def test_disposition
    assert_equal "attachment; filename=foo+bar.jar", blob("binary/foo bar.jar").disposition
    assert_equal "attachment; filename=foo.bin", blob("binary/foo.bin").disposition
    assert_equal "attachment; filename=linguist.gem", blob("binary/linguist.gem").disposition
    assert_equal "attachment; filename=octocat.ai", blob("binary/octocat.ai").disposition
    assert_equal "inline", blob("text/README").disposition
    assert_equal "inline", blob("text/foo.txt").disposition
    assert_equal "inline", blob("ruby/grit.rb").disposition
    assert_equal "inline", blob("binary/octocat.png").disposition
  end

  def test_data
    assert_equal "module Foo\nend\n", blob("ruby/foo.rb").data
  end

  def test_lines
    assert_equal ["module Foo", "end", ""], blob("ruby/foo.rb").lines
  end

  def test_size
    assert_equal 15, blob("ruby/foo.rb").size
  end

  def test_loc
    assert_equal 3, blob("ruby/foo.rb").loc
  end

  def test_sloc
    assert_equal 2, blob("ruby/foo.rb").sloc
  end

  def test_encoding
    assert_equal "ISO-8859-2", blob("text/README").encoding
    assert_equal "ISO-8859-1", blob("text/dump.sql").encoding
    assert_equal "UTF-8", blob("text/foo.txt").encoding
    assert_nil blob("binary/dog.o").encoding
  end

  def test_binary
    # Large blobs aren't loaded
    large_blob = blob("git.exe")
    large_blob.instance_eval do
      def data; end
    end
    assert large_blob.binary?

    assert blob("binary/git.deb").binary?
    assert blob("binary/git.exe").binary?
    assert blob("binary/hello.pbc").binary?
    assert blob("binary/linguist.gem").binary?
    assert blob("binary/octocat.ai").binary?
    assert blob("binary/octocat.png").binary?
    assert blob("binary/zip").binary?
    assert !blob("text/README").binary?
    assert !blob("text/file.txt").binary?
    assert !blob("ruby/foo.rb").binary?
    assert !blob("perl/script.pl").binary?
  end

  def test_text
    assert blob("text/README").text?
    assert blob("text/dump.sql").text?
    assert blob("text/file.json").text?
    assert blob("text/file.txt").text?
    assert blob("text/md").text?
    assert blob("shell/script.sh").text?
    assert blob("text/txt").text?
  end

  def test_image
    assert blob("binary/octocat.gif").image?
    assert blob("binary/octocat.jpeg").image?
    assert blob("binary/octocat.jpg").image?
    assert blob("binary/octocat.png").image?
    assert !blob("binary/octocat.ai").image?
    assert !blob("binary/octocat.psd").image?
  end

  def test_viewable
    assert blob("text/README").viewable?
    assert blob("ruby/foo.rb").viewable?
    assert blob("perl/script.pl").viewable?
    assert !blob("binary/linguist.gem").viewable?
    assert !blob("binary/octocat.ai").viewable?
    assert !blob("binary/octocat.png").viewable?
  end

  def test_generated
    assert !blob("text/README").generated?

    # Xcode project files
    assert blob("xml/MainMenu.xib").generated?
    assert blob("binary/MainMenu.nib").generated?
    assert blob("xml/project.pbxproj").generated?

    # Gemfile.locks
    assert blob("Gemfile.lock").generated?

    # Generated .NET Docfiles
    assert blob("xml/net_docfile.xml").generated?

    # Long line
    assert !blob("javascript/uglify.js").generated?

    # Inlined JS, but mostly code
    assert !blob("javascript/json2_backbone.js").generated?

    # Minified JS
    assert !blob("javascript/jquery-1.6.1.js").generated?
    assert blob("javascript/jquery-1.6.1.min.js").generated?
    assert blob("javascript/jquery-1.4.2.min.js").generated?

    # CoffeScript JS

    # These examples are to basic to tell
    assert !blob("javascript/empty.js").generated?
    assert !blob("javascript/hello.js").generated?

    assert blob("javascript/intro-old.js").generated?
    assert blob("javascript/classes-old.js").generated?

    assert blob("javascript/intro.js").generated?
    assert blob("javascript/classes.js").generated?
  end

  def test_vendored
    assert !blob("text/README").vendored?
    assert !blob("ext/extconf.rb").vendored?

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
    assert blob("text/file.txt").indexable?
    assert blob("ruby/foo.rb").indexable?
    assert !blob("text/defu.nkt").indexable?
    assert !blob("text/dump.sql").indexable?
    assert !blob("binary/github.po").indexable?
    assert !blob("binary/linguist.gem").indexable?
  end

  def test_language
    Sample.each do |sample|
      blob = blob(sample.path)
      assert_equal sample.language, blob.language, blob.name
    end
  end

  def test_lexer
    assert_equal Lexer['Ruby'], blob("ruby/foo.rb").lexer
  end

  def test_shebang_script
    assert_equal 'sh', script_blob("shell/script.sh").shebang_script
    assert_equal 'bash', script_blob("shell/script.bash").shebang_script
    assert_equal 'zsh', script_blob("shell/script.zsh").shebang_script
    assert_equal 'perl', script_blob("perl/script.pl").shebang_script
    assert_equal 'ruby', script_blob("ruby/script.rb").shebang_script
    assert_equal 'ruby', script_blob("ruby/script2.rb").shebang_script
    assert_equal 'python', script_blob("python/script.py").shebang_script
    assert_equal 'node', script_blob("javascript/script.js").shebang_script
    assert_equal 'groovy', script_blob("groovy/script.groovy").shebang_script
    assert_equal 'macruby', script_blob("ruby/macruby-script").shebang_script
    assert_equal 'rake', script_blob("ruby/script.rake").shebang_script
    assert_equal 'foo', script_blob("text/script.foo").shebang_script
    assert_equal 'nush', script_blob("nu/script.nu").shebang_script
    assert_equal 'scala', script_blob("scala/script.scala").shebang_script
    assert_equal 'racket', script_blob("racket/script.rkt").shebang_script
    assert_equal nil, script_blob("ruby/foo.rb").shebang_script
  end

  def test_colorize
    assert_equal <<-HTML, blob("ruby/foo.rb").colorize
<div class="highlight"><pre><span class="k">module</span> <span class="nn">Foo</span>
<span class="k">end</span>
</pre>
</div>
    HTML
  end

  def test_colorize_without_wrapper
    assert_equal <<-HTML, blob("ruby/foo.rb").colorize_without_wrapper
<span class="k">module</span> <span class="nn">Foo</span>
<span class="k">end</span>
    HTML
  end

  def test_colorize_does_skip_minified_files
    assert_nil blob("javascript/jquery-1.6.1.min.js").colorize
  end

  # Pygments.rb was taking exceeding long on this particular file
  def test_colorize_doesnt_blow_up_with_files_with_high_ratio_of_long_lines
    assert_nil blob("javascript/steelseries-min.js").colorize
  end
end
