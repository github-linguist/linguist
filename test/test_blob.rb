require 'linguist/file_blob'
require 'linguist/samples'

require 'test/unit'
require 'mocha/setup'
require 'mime/types'
require 'pygments'

class TestBlob < Test::Unit::TestCase
  include Linguist

  Lexer = Pygments::Lexer

  def samples_path
    File.expand_path("../../samples", __FILE__)
  end

  def blob(name)
    name = File.join(samples_path, name) unless name =~ /^\//
    FileBlob.new(name, samples_path)
  end

  def script_blob(name)
    blob = blob(name)
    blob.instance_variable_set(:@name, 'script')
    blob
  end

  def test_name
    assert_equal "foo.rb", blob("foo.rb").name
  end

  def test_mime_type
    assert_equal "application/postscript", blob("Binary/octocat.ai").mime_type
    assert_equal "application/x-ruby", blob("Ruby/grit.rb").mime_type
    assert_equal "application/x-sh", blob("Shell/script.sh").mime_type
    assert_equal "application/xml", blob("XML/bar.xml").mime_type
    assert_equal "audio/ogg", blob("Binary/foo.ogg").mime_type
    assert_equal "text/plain", blob("Text/README").mime_type
  end

  def test_content_type
    assert_equal "application/pdf", blob("Binary/foo.pdf").content_type
    assert_equal "audio/ogg", blob("Binary/foo.ogg").content_type
    assert_equal "image/png", blob("Binary/foo.png").content_type
    assert_equal "text/plain; charset=iso-8859-2", blob("Text/README").content_type
  end

  def test_disposition
    assert_equal "attachment; filename=foo+bar.jar", blob("Binary/foo bar.jar").disposition
    assert_equal "attachment; filename=foo.bin", blob("Binary/foo.bin").disposition
    assert_equal "attachment; filename=linguist.gem", blob("Binary/linguist.gem").disposition
    assert_equal "attachment; filename=octocat.ai", blob("Binary/octocat.ai").disposition
    assert_equal "inline", blob("Text/README").disposition
    assert_equal "inline", blob("Text/foo.txt").disposition
    assert_equal "inline", blob("Ruby/grit.rb").disposition
    assert_equal "inline", blob("Binary/octocat.png").disposition
  end

  def test_data
    assert_equal "module Foo\nend\n", blob("Ruby/foo.rb").data
  end

  def test_lines
    assert_equal ["module Foo", "end", ""], blob("Ruby/foo.rb").lines
    assert_equal ["line 1", "line 2", ""], blob("Text/mac.txt").lines
    assert_equal 475, blob("Emacs Lisp/ess-julia.el").lines.length
  end

  def test_size
    assert_equal 15, blob("Ruby/foo.rb").size
  end

  def test_loc
    assert_equal 3, blob("Ruby/foo.rb").loc
  end

  def test_sloc
    assert_equal 2, blob("Ruby/foo.rb").sloc
  end

  def test_encoding
    assert_equal "ISO-8859-2", blob("Text/README").encoding
    assert_equal "ISO-8859-1", blob("Text/dump.sql").encoding
    assert_equal "UTF-8", blob("Text/foo.txt").encoding
    assert_nil blob("Binary/dog.o").encoding
  end

  def test_binary
    # Large blobs aren't loaded
    large_blob = blob("git.exe")
    large_blob.instance_eval do
      def data; end
    end
    assert large_blob.binary?

    assert blob("Binary/git.deb").binary?
    assert blob("Binary/git.exe").binary?
    assert blob("Binary/hello.pbc").binary?
    assert blob("Binary/linguist.gem").binary?
    assert blob("Binary/octocat.ai").binary?
    assert blob("Binary/octocat.png").binary?
    assert blob("Binary/zip").binary?
    assert !blob("Text/README").binary?
    assert !blob("Text/file.txt").binary?
    assert !blob("Ruby/foo.rb").binary?
    assert !blob("Perl/script.pl").binary?
  end

  def test_text
    assert blob("Text/README").text?
    assert blob("Text/dump.sql").text?
    assert blob("Text/file.json").text?
    assert blob("Text/file.txt").text?
    assert blob("Text/md").text?
    assert blob("Shell/script.sh").text?
    assert blob("Text/txt").text?
  end

  def test_image
    assert blob("Binary/octocat.gif").image?
    assert blob("Binary/octocat.jpeg").image?
    assert blob("Binary/octocat.jpg").image?
    assert blob("Binary/octocat.png").image?
    assert !blob("Binary/octocat.ai").image?
    assert !blob("Binary/octocat.psd").image?
  end

  def test_solid
    assert blob("Binary/cube.stl").solid?
    assert blob("Text/cube.stl").solid?
  end

  def test_csv
    assert blob("Text/cars.csv").csv?
  end

  def test_pdf
    assert blob("Binary/foo.pdf").pdf?
  end

  def test_viewable
    assert blob("Text/README").viewable?
    assert blob("Ruby/foo.rb").viewable?
    assert blob("Perl/script.pl").viewable?
    assert !blob("Binary/linguist.gem").viewable?
    assert !blob("Binary/octocat.ai").viewable?
    assert !blob("Binary/octocat.png").viewable?
  end

  def test_generated
    assert !blob("Text/README").generated?

    # Xcode project files
    assert blob("XML/MainMenu.xib").generated?
    assert blob("Binary/MainMenu.nib").generated?
    assert blob("XML/project.pbxproj").generated?

    # Gemfile.locks
    assert blob("Gemfile.lock").generated?

    # Generated .NET Docfiles
    assert blob("XML/net_docfile.xml").generated?

    # Long line
    assert !blob("JavaScript/uglify.js").generated?

    # Inlined JS, but mostly code
    assert !blob("JavaScript/json2_backbone.js").generated?

    # Minified JS
    assert !blob("JavaScript/jquery-1.6.1.js").generated?
    assert blob("JavaScript/jquery-1.6.1.min.js").generated?
    assert blob("JavaScript/jquery-1.4.2.min.js").generated?

    # CoffeeScript-generated JS
    # TODO

    # TypeScript-generated JS
    # TODO

    # PEG.js-generated parsers
    assert blob("JavaScript/parser.js").generated?

    # These examples are too basic to tell
    assert !blob("JavaScript/empty.js").generated?
    assert !blob("JavaScript/hello.js").generated?

    assert blob("JavaScript/intro-old.js").generated?
    assert blob("JavaScript/classes-old.js").generated?

    assert blob("JavaScript/intro.js").generated?
    assert blob("JavaScript/classes.js").generated?

    # Protocol Buffer generated code
    assert blob("C++/protocol-buffer.pb.h").generated?
    assert blob("C++/protocol-buffer.pb.cc").generated?
    assert blob("Java/ProtocolBuffer.java").generated?
    assert blob("Python/protocol_buffer_pb2.py").generated?

    # Minified CSS
    assert !blob("CSS/bootstrap.css").generated?
    assert blob("CSS/bootstrap.min.css").generated?
  end

  def test_vendored
    assert !blob("Text/README").vendored?
    assert !blob("ext/extconf.rb").vendored?

    # Node dependencies
    assert blob("node_modules/coffee-script/lib/coffee-script.js").vendored?

    # Rails vendor/
    assert blob("vendor/plugins/will_paginate/lib/will_paginate.rb").vendored?

    # C deps
    assert blob("deps/http_parser/http_parser.c").vendored?
    assert blob("deps/v8/src/v8.h").vendored?

    # Debian packaging
    assert blob("debian/cron.d").vendored?

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
    assert blob("public/javascripts/jquery-1.10.1.js").vendored?
    assert blob("public/javascripts/jquery-1.10.1.min.js").vendored?
    assert !blob("public/javascripts/jquery.github.menu.js").vendored?

    # jQuery UI
    assert blob("themes/ui-lightness/jquery-ui.css").vendored?
    assert blob("themes/ui-lightness/jquery-ui-1.8.22.custom.css").vendored?
    assert blob("themes/ui-lightness/jquery.ui.accordion.css").vendored?
    assert blob("ui/i18n/jquery.ui.datepicker-ar.js").vendored?
    assert blob("ui/i18n/jquery-ui-i18n.js").vendored?
    assert blob("ui/jquery.effects.blind.js").vendored?
    assert blob("ui/jquery-ui-1.8.22.custom.js").vendored?
    assert blob("ui/jquery-ui-1.8.22.custom.min.js").vendored?
    assert blob("ui/jquery-ui-1.8.22.js").vendored?
    assert blob("ui/jquery-ui-1.8.js").vendored?
    assert blob("ui/jquery-ui.min.js").vendored?
    assert blob("ui/jquery.ui.accordion.js").vendored?
    assert blob("ui/minified/jquery.effects.blind.min.js").vendored?
    assert blob("ui/minified/jquery.ui.accordion.min.js").vendored?


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
    assert blob("Scripts/jquery.validate.min.js").vendored?
    assert blob("Scripts/jquery.validate.unobtrusive.js").vendored?
    assert blob("Scripts/jquery.validate.unobtrusive.min.js").vendored?
    assert blob("Scripts/jquery.unobtrusive-ajax.js").vendored?
    assert blob("Scripts/jquery.unobtrusive-ajax.min.js").vendored?

    # NuGet Packages
    assert blob("packages/Modernizr.2.0.6/Content/Scripts/modernizr-2.0.6-development-only.js").vendored?

    # Test fixtures
    assert blob("test/fixtures/random.rkt").vendored?
    assert blob("Test/fixtures/random.rkt").vendored?
  end

  def test_language
    Samples.each do |sample|
      blob = blob(sample[:path])
      assert blob.language, "No language for #{sample[:path]}"
      assert_equal sample[:language], blob.language.name, blob.name
    end
  end

  def test_lexer
    assert_equal Lexer['Ruby'], blob("Ruby/foo.rb").lexer
  end

  def test_colorize
    assert_equal <<-HTML.chomp, blob("Ruby/foo.rb").colorize
<div class="highlight"><pre><span class="k">module</span> <span class="nn">Foo</span>
<span class="k">end</span>
</pre></div>
    HTML
  end

  def test_colorize_does_skip_minified_files
    assert_nil blob("JavaScript/jquery-1.6.1.min.js").colorize
  end

  # Pygments.rb was taking exceeding long on this particular file
  def test_colorize_doesnt_blow_up_with_files_with_high_ratio_of_long_lines
    assert_nil blob("JavaScript/steelseries-min.js").colorize
  end
end
