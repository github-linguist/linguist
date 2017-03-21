require_relative "./helper"

class TestFileBlob < Minitest::Test
  include Linguist

  def silence_warnings
    original_verbosity = $VERBOSE
    $VERBOSE = nil
    yield
  ensure
    $VERBOSE = original_verbosity
  end

  def setup
    silence_warnings do
      # git blobs are normally loaded as ASCII-8BIT since they may contain data
      # with arbitrary encoding not known ahead of time
      @original_external = Encoding.default_external
      Encoding.default_external = Encoding.find("ASCII-8BIT")
    end
  end

  def teardown
    silence_warnings do
      Encoding.default_external = @original_external
    end
  end

  def script_blob(name)
    blob = sample_blob(name)
    blob.instance_variable_set(:@name, 'script')
    blob
  end

  def test_extensions
    assert_equal [".gitignore"], Linguist::FileBlob.new(".gitignore").extensions
    assert_equal [".xml"],  Linguist::FileBlob.new("build.xml").extensions
    assert_equal [".html.erb", ".erb"],  Linguist::FileBlob.new("dotted.dir/index.html.erb").extensions
  end

  def test_name
    assert_equal "foo.rb", sample_blob("foo.rb").name
  end

  def test_mime_type
    assert_equal "application/postscript", fixture_blob("Binary/octocat.ai").mime_type
    assert_equal "application/x-ruby", sample_blob("Ruby/grit.rb").mime_type
    assert_equal "application/x-sh", sample_blob("Shell/script.sh").mime_type
    assert_equal "application/xml", sample_blob("XML/bar.xml").mime_type
    assert_equal "audio/ogg", fixture_blob("Binary/foo.ogg").mime_type
    assert_equal "text/plain", fixture_blob("Data/README").mime_type
  end

  def test_content_type
    assert_equal "application/pdf", fixture_blob("Binary/foo.pdf").content_type
    assert_equal "audio/ogg", fixture_blob("Binary/foo.ogg").content_type
    assert_equal "image/png", fixture_blob("Binary/foo.png").content_type
    assert_equal "text/plain; charset=iso-8859-2", fixture_blob("Data/README").content_type
  end

  def test_disposition
    assert_equal "attachment; filename=foo+bar.jar", fixture_blob("Binary/foo bar.jar").disposition
    assert_equal "attachment; filename=foo.bin", fixture_blob("Binary/foo.bin").disposition
    assert_equal "attachment; filename=linguist.gem", fixture_blob("Binary/linguist.gem").disposition
    assert_equal "attachment; filename=octocat.ai", fixture_blob("Binary/octocat.ai").disposition
    assert_equal "inline", fixture_blob("Data/README").disposition
    assert_equal "inline", sample_blob("Text/foo.txt").disposition
    assert_equal "inline", sample_blob("Ruby/grit.rb").disposition
    assert_equal "inline", fixture_blob("Binary/octocat.png").disposition
  end

  def test_data
    assert_equal "module Foo\nend\n", sample_blob("Ruby/foo.rb").data
  end

  def test_lines
    assert_equal ["module Foo", "end", ""], sample_blob("Ruby/foo.rb").lines
    assert_equal ["line 1", "line 2", ""], sample_blob("Text/mac.txt").lines
    assert_equal 475, sample_blob("Emacs Lisp/ess-julia.el").lines.length
  end

  def test_lines_maintains_original_encoding
    # Even if the file's encoding is detected as something like UTF-16LE,
    # earlier versions of the gem made implicit guarantees that the encoding of
    # each `line` is in the same encoding as the file was originally read (in
    # practice, UTF-8 or ASCII-8BIT)
    assert_equal Encoding.default_external, fixture_blob("Data/utf16le").lines.first.encoding
  end

  def test_size
    assert_equal 15, sample_blob("Ruby/foo.rb").size
  end

  def test_loc
    assert_equal 3, sample_blob("Ruby/foo.rb").loc
  end

  def test_sloc
    assert_equal 2, sample_blob("Ruby/foo.rb").sloc
    assert_equal 3, fixture_blob("Data/utf16le-windows").sloc
    assert_equal 1, fixture_blob("Data/iso8859-8-i").sloc
  end

  def test_encoding
    assert_equal "ISO-8859-2", fixture_blob("Data/README").encoding
    assert_equal "ISO-8859-2", fixture_blob("Data/README").ruby_encoding
    assert_equal "UTF-8", sample_blob("Text/foo.txt").encoding
    assert_equal "UTF-8", sample_blob("Text/foo.txt").ruby_encoding
    assert_equal "UTF-16LE", fixture_blob("Data/utf16le").encoding
    assert_equal "UTF-16LE", fixture_blob("Data/utf16le").ruby_encoding
    assert_equal "UTF-16LE", fixture_blob("Data/utf16le-windows").encoding
    assert_equal "UTF-16LE", fixture_blob("Data/utf16le-windows").ruby_encoding
    assert_equal "ISO-2022-KR", sample_blob("Text/ISO-2022-KR.txt").encoding
    assert_equal "binary", sample_blob("Text/ISO-2022-KR.txt").ruby_encoding
    assert_nil fixture_blob("Binary/dog.o").encoding
  end

  def test_binary
    # Large blobs aren't loaded
    large_blob = sample_blob("git.exe")
    large_blob.instance_eval do
      def data; end
    end
    assert large_blob.binary?

    assert fixture_blob("Binary/git.deb").binary?
    assert fixture_blob("Binary/git.exe").binary?
    assert fixture_blob("Binary/hello.pbc").binary?
    assert fixture_blob("Binary/linguist.gem").binary?
    assert fixture_blob("Binary/octocat.ai").binary?
    assert fixture_blob("Binary/octocat.png").binary?
    assert fixture_blob("Binary/zip").binary?
    assert !fixture_blob("Data/README").binary?
    assert !sample_blob("Ruby/foo.rb").binary?
    assert !sample_blob("Perl/script.pl").binary?
  end

  def test_all_binary
    Samples.each do |sample|
      blob = sample_blob(sample[:path])
      assert ! (blob.likely_binary? || blob.binary?), "#{sample[:path]} is a binary file"
    end
  end

  def test_text
    assert fixture_blob("Data/README").text?
    assert fixture_blob("Data/md").text?
    assert sample_blob("Shell/script.sh").text?
    assert fixture_blob("Data/txt").text?
  end

  def test_image
    assert fixture_blob("Binary/octocat.gif").image?
    assert fixture_blob("Binary/octocat.jpeg").image?
    assert fixture_blob("Binary/octocat.jpg").image?
    assert fixture_blob("Binary/octocat.png").image?
    assert !fixture_blob("Binary/octocat.ai").image?
    assert !fixture_blob("Binary/octocat.psd").image?
  end

  def test_solid
    assert fixture_blob("Binary/cube.stl").solid?
    assert fixture_blob("Data/cube.stl").solid?
  end

  def test_csv
    assert sample_blob("CSV/cars.csv").csv?
  end

  def test_pdf
    assert fixture_blob("Binary/foo.pdf").pdf?
  end

  def test_viewable
    assert fixture_blob("Data/README").viewable?
    assert sample_blob("Ruby/foo.rb").viewable?
    assert sample_blob("Perl/script.pl").viewable?
    assert !fixture_blob("Binary/linguist.gem").viewable?
    assert !fixture_blob("Binary/octocat.ai").viewable?
    assert !fixture_blob("Binary/octocat.png").viewable?
  end

  def test_generated
    assert !fixture_blob("Data/README").generated?

    # Xcode project files
    assert !sample_blob("XML/MainMenu.xib").generated?
    assert fixture_blob("Binary/MainMenu.nib").generated?
    assert !sample_blob("XML/project.pbxproj").generated?

    # Gemfile.lock is NOT generated
    assert !sample_blob("Gemfile.lock").generated?

    # Generated .NET Docfiles
    assert sample_blob("XML/net_docfile.xml").generated?

    # Long line
    assert !sample_blob("JavaScript/uglify.js").generated?

    # Inlined JS, but mostly code
    assert !sample_blob("JavaScript/json2_backbone.js").generated?

    # Minified JS
    assert !sample_blob("JavaScript/jquery-1.6.1.js").generated?
    assert sample_blob("JavaScript/jquery-1.6.1.min.js").generated?
    assert sample_blob("JavaScript/jquery-1.4.2.min.js").generated?

    # CoffeeScript-generated JS
    # TODO

    # TypeScript-generated JS
    # TODO

    # Composer generated composer.lock file
    assert sample_blob("JSON/composer.lock").generated?

    # PEG.js-generated parsers
    assert sample_blob("JavaScript/parser.js").generated?

    # Generated PostScript
    assert !sample_blob("PostScript/sierpinski.ps").generated?

    # These examples are too basic to tell
    assert !sample_blob("JavaScript/hello.js").generated?

    assert sample_blob("JavaScript/intro-old.js").generated?
    assert sample_blob("JavaScript/classes-old.js").generated?

    assert sample_blob("JavaScript/intro.js").generated?
    assert sample_blob("JavaScript/classes.js").generated?

    # Protocol Buffer generated code
    assert sample_blob("C++/protocol-buffer.pb.h").generated?
    assert sample_blob("C++/protocol-buffer.pb.cc").generated?
    assert sample_blob("Java/ProtocolBuffer.java").generated?
    assert sample_blob("Python/protocol_buffer_pb2.py").generated?
    assert sample_blob("Go/api.pb.go").generated?
    assert sample_blob("Go/embedded.go").generated?

    # Apache Thrift generated code
    assert sample_blob("Python/gen-py-linguist-thrift.py").generated?
    assert sample_blob("Go/gen-go-linguist-thrift.go").generated?
    assert sample_blob("Java/gen-java-linguist-thrift.java").generated?
    assert sample_blob("JavaScript/gen-js-linguist-thrift.js").generated?
    assert sample_blob("Ruby/gen-rb-linguist-thrift.rb").generated?
    assert sample_blob("Objective-C/gen-cocoa-linguist-thrift.m").generated?

    # Generated JNI
    assert sample_blob("C/jni_layer.h").generated?

    # Minified CSS
    assert !sample_blob("CSS/bootstrap.css").generated?
    assert sample_blob("CSS/bootstrap.min.css").generated?

    # Generated VCR
    assert sample_blob("YAML/vcr_cassette.yml").generated?

    # Generated by Zephir
    assert sample_blob("Zephir/filenames/exception.zep.c").generated?
    assert sample_blob("Zephir/filenames/exception.zep.h").generated?
    assert sample_blob("Zephir/filenames/exception.zep.php").generated?
    assert !sample_blob("Zephir/Router.zep").generated?

    assert sample_blob("node_modules/grunt/lib/grunt.js").generated?

    # Godep saved dependencies
    assert sample_blob("Godeps/Godeps.json").generated?
    assert sample_blob("Godeps/_workspace/src/github.com/kr/s3/sign.go").generated?

    # Cython-generated C/C++
    assert sample_blob("C/sgd_fast.c").generated?
    assert sample_blob("C++/wrapper_inner.cpp").generated?

    # Unity3D-generated metadata
    assert sample_blob("Unity3D Asset/Tiles.meta").generated?
  end

  def test_vendored
    assert !fixture_blob("Data/README").vendored?
    assert !sample_blob("ext/extconf.rb").vendored?

    # Dependencies
    assert sample_blob("dependencies/windows/headers/GL/glext.h").vendored?

    # Node dependencies
    assert sample_blob("node_modules/coffee-script/lib/coffee-script.js").vendored?

    # Bower Components
    assert sample_blob("bower_components/custom/custom.js").vendored?
    assert sample_blob("app/bower_components/custom/custom.js").vendored?
    assert sample_blob("vendor/assets/bower_components/custom/custom.js").vendored?

    # Go dependencies
    assert !sample_blob("Godeps/Godeps.json").vendored?
    assert sample_blob("Godeps/_workspace/src/github.com/kr/s3/sign.go").vendored?

    assert sample_blob(".indent.pro").vendored?

    # Rails vendor/
    assert sample_blob("vendor/plugins/will_paginate/lib/will_paginate.rb").vendored?

    # Vendor/
    assert sample_blob("Vendor/my_great_file.h").vendored?

    # 'thirdparty' directory
    assert sample_blob("thirdparty/lib/main.c").vendored?

    # 'extern(al)' directory
    assert sample_blob("extern/util/__init__.py").vendored?
    assert sample_blob("external/jquery.min.js").vendored?

    # C deps
    assert sample_blob("deps/http_parser/http_parser.c").vendored?
    assert sample_blob("deps/v8/src/v8.h").vendored?

    assert sample_blob("tools/something/else.c").vendored?

    # Chart.js
    assert sample_blob("some/vendored/path/Chart.js").vendored?
    assert !sample_blob("some/vendored/path/chart.js").vendored?

    # CodeMirror deps
    assert sample_blob("codemirror/mode/blah.js").vendored?
    assert sample_blob("codemirror/5.0/mode/blah.js").vendored?

    # Debian packaging
    assert sample_blob("debian/cron.d").vendored?

    # Django env
    assert sample_blob("env/foo.py").vendored?

    # Erlang
    assert sample_blob("rebar").vendored?

    # git config files
    assert_predicate fixture_blob("some/path/.gitattributes"), :vendored?
    assert_predicate fixture_blob(".gitignore"), :vendored?
    assert_predicate fixture_blob("special/path/.gitmodules"), :vendored?

    # Minified JavaScript and CSS
    assert sample_blob("foo.min.js").vendored?
    assert sample_blob("foo.min.css").vendored?
    assert sample_blob("foo-min.js").vendored?
    assert sample_blob("foo-min.css").vendored?
    assert !sample_blob("foomin.css").vendored?
    assert !sample_blob("foo.min.txt").vendored?

    #.osx
    assert sample_blob(".osx").vendored?

    # Prototype
    assert !sample_blob("public/javascripts/application.js").vendored?
    assert sample_blob("public/javascripts/prototype.js").vendored?
    assert sample_blob("public/javascripts/effects.js").vendored?
    assert sample_blob("public/javascripts/controls.js").vendored?
    assert sample_blob("public/javascripts/dragdrop.js").vendored?

    # jQuery
    assert sample_blob("jquery.js").vendored?
    assert sample_blob("public/javascripts/jquery.js").vendored?
    assert sample_blob("public/javascripts/jquery.min.js").vendored?
    assert sample_blob("public/javascripts/jquery-1.7.js").vendored?
    assert sample_blob("public/javascripts/jquery-1.7.min.js").vendored?
    assert sample_blob("public/javascripts/jquery-1.5.2.js").vendored?
    assert sample_blob("public/javascripts/jquery-1.6.1.js").vendored?
    assert sample_blob("public/javascripts/jquery-1.6.1.min.js").vendored?
    assert sample_blob("public/javascripts/jquery-1.10.1.js").vendored?
    assert sample_blob("public/javascripts/jquery-1.10.1.min.js").vendored?
    assert !sample_blob("public/javascripts/jquery.github.menu.js").vendored?

    # jQuery UI
    assert sample_blob("themes/ui-lightness/jquery-ui.css").vendored?
    assert sample_blob("themes/ui-lightness/jquery-ui-1.8.22.custom.css").vendored?
    assert sample_blob("themes/ui-lightness/jquery.ui.accordion.css").vendored?
    assert sample_blob("ui/i18n/jquery.ui.datepicker-ar.js").vendored?
    assert sample_blob("ui/i18n/jquery-ui-i18n.js").vendored?
    assert sample_blob("ui/jquery.effects.blind.js").vendored?
    assert sample_blob("ui/jquery-ui-1.8.22.custom.js").vendored?
    assert sample_blob("ui/jquery-ui-1.8.22.custom.min.js").vendored?
    assert sample_blob("ui/jquery-ui-1.8.22.js").vendored?
    assert sample_blob("ui/jquery-ui-1.8.js").vendored?
    assert sample_blob("ui/jquery-ui.min.js").vendored?
    assert sample_blob("ui/jquery.ui.accordion.js").vendored?
    assert sample_blob("ui/minified/jquery.effects.blind.min.js").vendored?
    assert sample_blob("ui/minified/jquery.ui.accordion.min.js").vendored?

    # jQuery Gantt
    assert sample_blob("web-app/jquery-gantt/js/jquery.fn.gantt.js").vendored?

    # jQuery fancyBox
    assert sample_blob("web-app/fancybox/jquery.fancybox.js").vendored?

    # Fuel UX
    assert sample_blob("web-app/fuelux/js/fuelux.js").vendored?

    # jQuery File Upload
    assert sample_blob("fileupload-9.0.0/jquery.fileupload-process.js").vendored?

    # Slick
    assert sample_blob("web-app/slickgrid/controls/slick.columnpicker.js").vendored?

    # Leaflet plugins
    assert sample_blob("leaflet-plugins/Leaflet.Coordinates-0.5.0.src.js").vendored?
    assert sample_blob("leaflet-plugins/leaflet.draw-src.js").vendored?
    assert sample_blob("leaflet-plugins/leaflet.spin.js").vendored?

    # MooTools
    assert sample_blob("public/javascripts/mootools-core-1.3.2-full-compat.js").vendored?
    assert sample_blob("public/javascripts/mootools-core-1.3.2-full-compat-yc.js").vendored?

    # Dojo
    assert sample_blob("public/javascripts/dojo.js").vendored?

    # MochiKit
    assert sample_blob("public/javascripts/MochiKit.js").vendored?

    # YUI
    assert sample_blob("public/javascripts/yahoo-dom-event.js").vendored?
    assert sample_blob("public/javascripts/yahoo-min.js").vendored?
    assert sample_blob("public/javascripts/yuiloader-dom-event.js").vendored?

    # WYS editors
    assert sample_blob("public/javascripts/ckeditor.js").vendored?
    assert sample_blob("public/javascripts/tiny_mce.js").vendored?
    assert sample_blob("public/javascripts/tiny_mce_popup.js").vendored?
    assert sample_blob("public/javascripts/tiny_mce_src.js").vendored?

    # Ace Editor
    assert sample_blob("ace-builds/src/ace.js").vendored?
    assert sample_blob("static/project/ace-builds/src/ace.js").vendored?

    # Fontello CSS files
    assert sample_blob("fontello.css").vendored?
    assert sample_blob("fontello-ie7.css").vendored?
    assert sample_blob("fontello-codes.css").vendored?
    assert sample_blob("fontello-codes-ie7.css").vendored?
    assert sample_blob("fontello-embedded.css").vendored?
    assert sample_blob("assets/css/fontello.css").vendored?
    assert sample_blob("assets/css/fontello-ie7.css").vendored?
    assert sample_blob("assets/css/fontello-codes.css").vendored?
    assert sample_blob("assets/css/fontello-codes-ie7.css").vendored?
    assert sample_blob("assets/css/fontello-embedded.css").vendored?

    # AngularJS
    assert sample_blob("public/javascripts/angular.js").vendored?
    assert sample_blob("public/javascripts/angular.min.js").vendored?

    # D3.js
    assert sample_blob("public/javascripts/d3.v3.js").vendored?
    assert sample_blob("public/javascripts/d3.v3.min.js").vendored?

    # Modernizr
    assert sample_blob("public/javascripts/modernizr-2.7.1.js").vendored?
    assert sample_blob("public/javascripts/modernizr.custom.01009.js").vendored?

    # Fabric
    assert sample_blob("fabfile.py").vendored?

    # WAF
    assert sample_blob("waf").vendored?

    # Visual Studio IntelliSense
    assert sample_blob("Scripts/jquery-1.7-vsdoc.js").vendored?

    # Microsoft Ajax
    assert sample_blob("Scripts/MicrosoftAjax.debug.js").vendored?
    assert sample_blob("Scripts/MicrosoftAjax.js").vendored?
    assert sample_blob("Scripts/MicrosoftMvcAjax.debug.js").vendored?
    assert sample_blob("Scripts/MicrosoftMvcAjax.js").vendored?
    assert sample_blob("Scripts/MicrosoftMvcValidation.debug.js").vendored?
    assert sample_blob("Scripts/MicrosoftMvcValidation.js").vendored?

    # jQuery validation plugin (MS bundles this with asp.net mvc)
    assert sample_blob("Scripts/jquery.validate.js").vendored?
    assert sample_blob("Scripts/jquery.validate.min.js").vendored?
    assert sample_blob("Scripts/jquery.validate.unobtrusive.js").vendored?
    assert sample_blob("Scripts/jquery.validate.unobtrusive.min.js").vendored?
    assert sample_blob("Scripts/jquery.unobtrusive-ajax.js").vendored?
    assert sample_blob("Scripts/jquery.unobtrusive-ajax.min.js").vendored?

    # NuGet Packages
    assert sample_blob("packages/Modernizr.2.0.6/Content/Scripts/modernizr-2.0.6-development-only.js").vendored?

    # Font Awesome
    assert sample_blob("some/asset/path/font-awesome.min.css").vendored?
    assert sample_blob("some/asset/path/font-awesome.css").vendored?

    # Normalize
    assert sample_blob("some/asset/path/normalize.css").vendored?

    # Carthage
    assert sample_blob('Carthage/blah').vendored?

    # Cocoapods
    assert sample_blob('Pods/blah').vendored?

    # Html5shiv
    assert sample_blob("Scripts/html5shiv.js").vendored?
    assert sample_blob("Scripts/html5shiv.min.js").vendored?

    # Test fixtures
    assert sample_blob("test/fixtures/random.rkt").vendored?
    assert sample_blob("Test/fixtures/random.rkt").vendored?
    assert sample_blob("tests/fixtures/random.rkt").vendored?

    # Cordova/PhoneGap
    assert sample_blob("cordova.js").vendored?
    assert sample_blob("cordova.min.js").vendored?
    assert sample_blob("cordova-2.1.0.js").vendored?
    assert sample_blob("cordova-2.1.0.min.js").vendored?

    # Foundation js
    assert sample_blob("foundation.js").vendored?
    assert sample_blob("foundation.min.js").vendored?
    assert sample_blob("foundation.abide.js").vendored?

    # Vagrant
    assert sample_blob("Vagrantfile").vendored?

    # Gradle
    assert sample_blob("gradlew").vendored?
    assert sample_blob("gradlew.bat").vendored?
    assert sample_blob("gradle/wrapper/gradle-wrapper.properties").vendored?
    assert sample_blob("subproject/gradlew").vendored?
    assert sample_blob("subproject/gradlew.bat").vendored?
    assert sample_blob("subproject/gradle/wrapper/gradle-wrapper.properties").vendored?

    # Octicons
    assert sample_blob("octicons.css").vendored?
    assert sample_blob("public/octicons.min.css").vendored?
    assert sample_blob("public/octicons/sprockets-octicons.scss").vendored?

    # Typesafe Activator
    assert sample_blob("activator").vendored?
    assert sample_blob("activator.bat").vendored?
    assert sample_blob("subproject/activator").vendored?
    assert sample_blob("subproject/activator.bat").vendored?

    assert_predicate fixture_blob(".google_apis/bar.jar"), :vendored?
    assert_predicate fixture_blob("foo/.google_apis/bar.jar"), :vendored?

    # Sphinx docs
    assert sample_blob("docs/_build/asset.doc").vendored?
    assert sample_blob("docs/theme/file.css").vendored?

    # Vagrant
    assert sample_blob("puphpet/file.pp").vendored?

    # Fabric.io
    assert sample_blob("Fabric.framework/Fabric.h").vendored?

    # Crashlytics
    assert sample_blob("Crashlytics.framework/Crashlytics.h").vendored?
    assert sample_blob("myapp/My Template.xctemplate/___FILEBASENAME___.h").vendored?
    assert sample_blob("myapp/My Images.xcassets/some/stuff.imageset/Contents.json").vendored?
    assert !sample_blob("myapp/MyData.json").vendored?

    # Jenkins
    assert sample_blob("Jenkinsfile").vendored?
  end

  def test_documentation
    assert_predicate fixture_blob("doc/foo.html"), :documentation?
    assert_predicate fixture_blob("docs/foo.html"), :documentation?
    refute_predicate fixture_blob("project/doc/foo.html"), :documentation?
    refute_predicate fixture_blob("project/docs/foo.html"), :documentation?

    assert_predicate fixture_blob("Documentation/foo.md"), :documentation?
    assert_predicate fixture_blob("documentation/foo.md"), :documentation?
    assert_predicate fixture_blob("project/Documentation/foo.md"), :documentation?
    assert_predicate fixture_blob("project/documentation/foo.md"), :documentation?

    assert_predicate fixture_blob("javadoc/foo.html"), :documentation?
    assert_predicate fixture_blob("project/javadoc/foo.html"), :documentation?

    assert_predicate fixture_blob("man/foo.html"), :documentation?
    refute_predicate fixture_blob("project/man/foo.html"), :documentation?

    assert_predicate fixture_blob("README"), :documentation?
    assert_predicate fixture_blob("README.md"), :documentation?
    assert_predicate fixture_blob("README.txt"), :documentation?
    assert_predicate fixture_blob("Readme"), :documentation?
    assert_predicate fixture_blob("readme"), :documentation?
    assert_predicate fixture_blob("foo/README"), :documentation?

    assert_predicate fixture_blob("CHANGE"), :documentation?
    assert_predicate fixture_blob("CHANGE.md"), :documentation?
    assert_predicate fixture_blob("CHANGE.txt"), :documentation?
    assert_predicate fixture_blob("foo/CHANGE"), :documentation?

    assert_predicate fixture_blob("CHANGELOG"), :documentation?
    assert_predicate fixture_blob("CHANGELOG.md"), :documentation?
    assert_predicate fixture_blob("CHANGELOG.txt"), :documentation?
    assert_predicate fixture_blob("foo/CHANGELOG"), :documentation?

    assert_predicate fixture_blob("CHANGES"), :documentation?
    assert_predicate fixture_blob("CHANGES.md"), :documentation?
    assert_predicate fixture_blob("CHANGES.txt"), :documentation?
    assert_predicate fixture_blob("foo/CHANGES"), :documentation?

    assert_predicate fixture_blob("CONTRIBUTING"), :documentation?
    assert_predicate fixture_blob("CONTRIBUTING.md"), :documentation?
    assert_predicate fixture_blob("CONTRIBUTING.txt"), :documentation?
    assert_predicate fixture_blob("foo/CONTRIBUTING"), :documentation?

    assert_predicate fixture_blob("examples/some-file.pl"), :documentation?
    assert_predicate fixture_blob("Examples/some-example-file.rb"), :documentation?

    assert_predicate fixture_blob("LICENSE"), :documentation?
    assert_predicate fixture_blob("LICENCE.md"), :documentation?
    assert_predicate fixture_blob("License.txt"), :documentation?
    assert_predicate fixture_blob("LICENSE.txt"), :documentation?
    assert_predicate fixture_blob("foo/LICENSE"), :documentation?

    assert_predicate fixture_blob("COPYING"), :documentation?
    assert_predicate fixture_blob("COPYING.md"), :documentation?
    assert_predicate fixture_blob("COPYING.txt"), :documentation?
    assert_predicate fixture_blob("foo/COPYING"), :documentation?

    assert_predicate fixture_blob("INSTALL"), :documentation?
    assert_predicate fixture_blob("INSTALL.md"), :documentation?
    assert_predicate fixture_blob("INSTALL.txt"), :documentation?
    assert_predicate fixture_blob("foo/INSTALL"), :documentation?

    refute_predicate fixture_blob("foo.md"), :documentation?

    # Samples
    assert sample_blob("Samples/Ruby/foo.rb").documentation?

    assert_predicate fixture_blob("INSTALL.txt"), :documentation?
  end

  def test_language
    Samples.each do |sample|
      blob = sample_blob(sample[:path])
      assert blob.language, "No language for #{sample[:path]}"
      assert_equal sample[:language], blob.language.name, blob.name
    end

    # Test language detection for files which shouldn't be used as samples
    root = File.expand_path('../fixtures', __FILE__)
    Dir.entries(root).each do |language|
      next if language == '.' || language == '..' || language == 'Binary' ||
              File.basename(language) == 'ace_modes.json'

      # Each directory contains test files of a language
      dirname = File.join(root, language)
      Dir.entries(dirname).each do |filename|
        # By default blob search the file in the samples;
        # thus, we need to give it the absolute path
        filepath = File.join(dirname, filename)
        next unless File.file?(filepath)

        blob = fixture_blob(filepath)
        if language == 'Data'
          assert blob.language.nil?, "A language was found for #{filepath}"
        elsif language == 'Generated'
          assert blob.generated?, "#{filepath} is not a generated file"
        else
          assert blob.language, "No language for #{filepath}"
          assert_equal language, blob.language.name, blob.name
        end
      end
    end
  end

  def test_minified_files_not_safe_to_highlight
    assert !sample_blob("JavaScript/jquery-1.6.1.min.js").safe_to_colorize?
  end

  def test_empty
    blob = Struct.new(:data) { include Linguist::BlobHelper }

    assert blob.new("").empty?
    assert blob.new(nil).empty?
    refute blob.new(" ").empty?
    refute blob.new("nope").empty?
  end

  def test_include_in_language_stats
    vendored = sample_blob("bower_components/custom/custom.js")
    assert_predicate vendored, :vendored?
    refute_predicate vendored, :include_in_language_stats?

    documentation = fixture_blob("README")
    assert_predicate documentation, :documentation?
    refute_predicate documentation, :include_in_language_stats?

    generated = sample_blob("CSS/bootstrap.min.css")
    assert_predicate generated, :generated?
    refute_predicate generated, :include_in_language_stats?

    data = sample_blob("Ant Build System/filenames/ant.xml")
    assert_equal :data, data.language.type
    refute_predicate data, :include_in_language_stats?

    prose = sample_blob("Markdown/tender.md")
    assert_equal :prose, prose.language.type
    refute_predicate prose, :include_in_language_stats?

    included = sample_blob("HTML/pages.html")
    assert_predicate included, :include_in_language_stats?
  end
end
