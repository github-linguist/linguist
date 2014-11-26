require 'linguist/file_blob'
require 'linguist/samples'

require 'test/unit'
require 'mocha/setup'
require 'mime/types'

class TestBlob < Test::Unit::TestCase
  include Linguist

  def setup
    # git blobs are normally loaded as ASCII-8BIT since they may contain data
    # with arbitrary encoding not known ahead of time
    @original_external = Encoding.default_external
    Encoding.default_external = Encoding.find("ASCII-8BIT")
  end

  def teardown
    Encoding.default_external = @original_external
  end

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

  def test_lines_maintains_original_encoding
    # Even if the file's encoding is detected as something like UTF-16LE,
    # earlier versions of the gem made implicit guarantees that the encoding of
    # each `line` is in the same encoding as the file was originally read (in
    # practice, UTF-8 or ASCII-8BIT)
    assert_equal Encoding.default_external, blob("Text/utf16le.txt").lines.first.encoding
  end

  def test_size
    assert_equal 15, blob("Ruby/foo.rb").size
  end

  def test_loc
    assert_equal 3, blob("Ruby/foo.rb").loc
  end

  def test_sloc
    assert_equal 2, blob("Ruby/foo.rb").sloc
    assert_equal 3, blob("Text/utf16le-windows.txt").sloc
    assert_equal 1, blob("Text/iso8859-8-i.txt").sloc
  end

  def test_encoding
    assert_equal "ISO-8859-2", blob("Text/README").encoding
    assert_equal "ISO-8859-2", blob("Text/README").ruby_encoding
    assert_equal "ISO-8859-1", blob("Text/dump.sql").encoding
    assert_equal "ISO-8859-1", blob("Text/dump.sql").ruby_encoding
    assert_equal "UTF-8", blob("Text/foo.txt").encoding
    assert_equal "UTF-8", blob("Text/foo.txt").ruby_encoding
    assert_equal "UTF-16LE", blob("Text/utf16le.txt").encoding
    assert_equal "UTF-16LE", blob("Text/utf16le.txt").ruby_encoding
    assert_equal "UTF-16LE", blob("Text/utf16le-windows.txt").encoding
    assert_equal "UTF-16LE", blob("Text/utf16le-windows.txt").ruby_encoding
    assert_equal "ISO-2022-KR", blob("Text/ISO-2022-KR.txt").encoding
    assert_equal "binary", blob("Text/ISO-2022-KR.txt").ruby_encoding
    assert_nil blob("Binary/dog.o").encoding

    assert_equal "windows-1252", blob("Text/Visual_Battlers.rb").encoding
    assert_equal "Windows-1252", blob("Text/Visual_Battlers.rb").ruby_encoding
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

  def test_all_binary
    Samples.each do |sample|
      blob = blob(sample[:path])
      assert ! (blob.likely_binary? || blob.binary?), "#{sample[:path]} is a binary file"
    end
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
    assert !blob("XML/MainMenu.xib").generated?
    assert blob("Binary/MainMenu.nib").generated?
    assert !blob("XML/project.pbxproj").generated?

    # Gemfile.lock is NOT generated
    assert !blob("Gemfile.lock").generated?

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

    # Composer generated composer.lock file
    assert blob("JSON/composer.lock").generated?

    # PEG.js-generated parsers
    assert blob("JavaScript/parser.js").generated?

    # Generated PostScript
    assert !blob("PostScript/sierpinski.ps").generated?

    # These examples are too basic to tell
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

    # Generated JNI
    assert blob("C/jni_layer.h").generated?

    # Minified CSS
    assert !blob("CSS/bootstrap.css").generated?
    assert blob("CSS/bootstrap.min.css").generated?

    # Generated VCR
    assert blob("YAML/vcr_cassette.yml").generated?

    # Generated by Zephir
    assert blob("Zephir/filenames/exception.zep.c").generated?
    assert blob("Zephir/filenames/exception.zep.h").generated?
    assert blob("Zephir/filenames/exception.zep.php").generated?
    assert !blob("Zephir/Router.zep").generated?


    assert Linguist::Generated.generated?("node_modules/grunt/lib/grunt.js", nil)

    # Godep saved dependencies
    assert blob("Godeps/Godeps.json").generated?
    assert blob("Godeps/_workspace/src/github.com/kr/s3/sign.go").generated?
  end

  def test_vendored
    assert !blob("Text/README").vendored?
    assert !blob("ext/extconf.rb").vendored?

    # Dependencies
    assert blob("dependencies/windows/headers/GL/glext.h").vendored?

    # Node dependencies
    assert blob("node_modules/coffee-script/lib/coffee-script.js").vendored?

    # Bower Components
    assert blob("bower_components/custom/custom.js").vendored?
    assert blob("app/bower_components/custom/custom.js").vendored?
    assert blob("vendor/assets/bower_components/custom/custom.js").vendored?

    # Go dependencies
    assert !blob("Godeps/Godeps.json").vendored?
    assert blob("Godeps/_workspace/src/github.com/kr/s3/sign.go").vendored?

    # Rails vendor/
    assert blob("vendor/plugins/will_paginate/lib/will_paginate.rb").vendored?

    # 'thirdparty' directory
    assert blob("thirdparty/lib/main.c").vendored?

    # 'extern(al)' directory
    assert blob("extern/util/__init__.py").vendored?
    assert blob("external/jquery.min.js").vendored?

    # C deps
    assert blob("deps/http_parser/http_parser.c").vendored?
    assert blob("deps/v8/src/v8.h").vendored?

    # Chart.js
    assert blob("some/vendored/path/Chart.js").vendored?
    assert !blob("some/vendored/path/chart.js").vendored?

    # Codemirror deps
    assert blob("codemirror/mode/blah.js").vendored?

    # Debian packaging
    assert blob("debian/cron.d").vendored?

    # Minified JavaScript and CSS
    assert blob("foo.min.js").vendored?
    assert blob("foo.min.css").vendored?
    assert blob("foo-min.js").vendored?
    assert blob("foo-min.css").vendored?
    assert !blob("foomin.css").vendored?
    assert !blob("foo.min.txt").vendored?

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

    # AngularJS
    assert blob("public/javascripts/angular.js").vendored?
    assert blob("public/javascripts/angular.min.js").vendored?

    # D3.js
    assert blob("public/javascripts/d3.v3.js").vendored?
    assert blob("public/javascripts/d3.v3.min.js").vendored?

    # Modernizr
    assert blob("public/javascripts/modernizr-2.7.1.js").vendored?
    assert blob("public/javascripts/modernizr.custom.01009.js").vendored?

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

    # Font Awesome
    assert blob("some/asset/path/font-awesome.min.css").vendored?
    assert blob("some/asset/path/font-awesome.css").vendored?

    # Normalize
    assert blob("some/asset/path/normalize.css").vendored?

    # Cocoapods
    assert blob('Pods/blah').vendored?

    # Html5shiv
    assert blob("Scripts/html5shiv.js").vendored?
    assert blob("Scripts/html5shiv.min.js").vendored?

    # Test fixtures
    assert blob("test/fixtures/random.rkt").vendored?
    assert blob("Test/fixtures/random.rkt").vendored?

    # Cordova/PhoneGap
    assert blob("cordova.js").vendored?
    assert blob("cordova.min.js").vendored?
    assert blob("cordova-2.1.0.js").vendored?
    assert blob("cordova-2.1.0.min.js").vendored?

    # Foundation js
    assert blob("foundation.js").vendored?
    assert blob("foundation.min.js").vendored?
    assert blob("foundation.abide.js").vendored?

    # Vagrant
    assert blob("Vagrantfile").vendored?

    # Gradle
    assert blob("gradlew").vendored?
    assert blob("gradlew.bat").vendored?
    assert blob("gradle/wrapper/gradle-wrapper.properties").vendored?
    assert blob("subproject/gradlew").vendored?
    assert blob("subproject/gradlew.bat").vendored?
    assert blob("subproject/gradle/wrapper/gradle-wrapper.properties").vendored?

    # Octicons
    assert blob("octicons.css").vendored?
    assert blob("public/octicons.min.css").vendored?
    assert blob("public/octicons/sprockets-octicons.scss").vendored?

    # Typesafe Activator
    assert blob("activator").vendored?
    assert blob("activator.bat").vendored?
    assert blob("subproject/activator").vendored?
    assert blob("subproject/activator.bat").vendored?
  end

  def test_language
    Samples.each do |sample|
      blob = blob(sample[:path])
      assert blob.language, "No language for #{sample[:path]}"
      assert_equal sample[:language], blob.language.name, blob.name
    end
  end

  def test_minified_files_not_safe_to_highlight
    assert !blob("JavaScript/jquery-1.6.1.min.js").safe_to_colorize?
  end

  def test_empty
    blob = Struct.new(:data) { include Linguist::BlobHelper }

    assert blob.new("").empty?
    assert blob.new(nil).empty?
    refute blob.new(" ").empty?
    refute blob.new("nope").empty?
  end
end
