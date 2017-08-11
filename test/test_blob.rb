require_relative "./helper"

class TestBlob < Minitest::Test
  include Linguist

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
    blob = sample_blob_memory(name)
    blob.instance_variable_set(:@name, 'script')
    blob
  end

  def test_name
    assert_equal "foo.rb", sample_blob_memory("Ruby/foo.rb").name
  end

  def test_mime_type
    assert_equal "application/postscript", fixture_blob_memory("Binary/octocat.ai").mime_type
    assert_equal "application/x-ruby", sample_blob_memory("Ruby/grit.rb").mime_type
    assert_equal "application/x-sh", sample_blob_memory("Shell/script.sh").mime_type
    assert_equal "text/plain", fixture_blob_memory("Data/README").mime_type
  end

  def test_content_type
    assert_equal "application/pdf", fixture_blob_memory("Binary/foo.pdf").content_type
    assert_equal "image/png", fixture_blob_memory("Binary/foo.png").content_type
    assert_equal "text/plain; charset=iso-8859-2", fixture_blob_memory("Data/README").content_type
  end

  def test_disposition
    assert_equal "attachment; filename=foo+bar.jar", fixture_blob_memory("Binary/foo bar.jar").disposition
    assert_equal "attachment; filename=foo.bin", fixture_blob_memory("Binary/foo.bin").disposition
    assert_equal "attachment; filename=linguist.gem", fixture_blob_memory("Binary/linguist.gem").disposition
    assert_equal "attachment; filename=octocat.ai", fixture_blob_memory("Binary/octocat.ai").disposition
    assert_equal "inline", fixture_blob_memory("Data/README").disposition
    assert_equal "inline", sample_blob_memory("Text/foo.txt").disposition
    assert_equal "inline", sample_blob_memory("Ruby/grit.rb").disposition
    assert_equal "inline", fixture_blob_memory("Binary/octocat.png").disposition
  end

  def test_data
    assert_equal "module Foo\nend\n", sample_blob_memory("Ruby/foo.rb").data
  end

  def test_lines
    assert_equal ["module Foo", "end", ""], sample_blob_memory("Ruby/foo.rb").lines
    assert_equal ["line 1", "line 2", ""], sample_blob_memory("Text/mac.txt").lines
    assert_equal 475, sample_blob_memory("Emacs Lisp/ess-julia.el").lines.length
  end

  def test_lines_maintains_original_encoding
    # Even if the file's encoding is detected as something like UTF-16LE,
    # earlier versions of the gem made implicit guarantees that the encoding of
    # each `line` is in the same encoding as the file was originally read (in
    # practice, UTF-8 or ASCII-8BIT)
    assert_equal Encoding.default_external, fixture_blob_memory("Data/utf16le").lines.first.encoding
  end

  def test_size
    assert_equal 15, sample_blob_memory("Ruby/foo.rb").size
  end

  def test_loc
    assert_equal 3, sample_blob_memory("Ruby/foo.rb").loc
  end

  def test_sloc
    assert_equal 2, sample_blob_memory("Ruby/foo.rb").sloc
    assert_equal 3, fixture_blob_memory("Data/utf16le-windows").sloc
    assert_equal 1, fixture_blob_memory("Data/iso8859-8-i").sloc
  end

  def test_encoding
    assert_equal "ISO-8859-2", fixture_blob_memory("Data/README").encoding
    assert_equal "ISO-8859-2", fixture_blob_memory("Data/README").ruby_encoding
    assert_equal "UTF-8", sample_blob_memory("Text/foo.txt").encoding
    assert_equal "UTF-8", sample_blob_memory("Text/foo.txt").ruby_encoding
    assert_equal "UTF-16LE", fixture_blob_memory("Data/utf16le").encoding
    assert_equal "UTF-16LE", fixture_blob_memory("Data/utf16le").ruby_encoding
    assert_equal "UTF-16LE", fixture_blob_memory("Data/utf16le-windows").encoding
    assert_equal "UTF-16LE", fixture_blob_memory("Data/utf16le-windows").ruby_encoding
    assert_equal "ISO-2022-KR", sample_blob_memory("Text/ISO-2022-KR.txt").encoding
    assert_equal "binary", sample_blob_memory("Text/ISO-2022-KR.txt").ruby_encoding
    assert_nil fixture_blob_memory("Binary/dog.o").encoding
  end

  def test_binary
    assert fixture_blob_memory("Binary/git.deb").binary?
    assert fixture_blob_memory("Binary/hello.pbc").binary?
    assert fixture_blob_memory("Binary/linguist.gem").binary?
    assert fixture_blob_memory("Binary/octocat.ai").binary?
    assert fixture_blob_memory("Binary/octocat.png").binary?
    assert fixture_blob_memory("Binary/zip").binary?
    assert !fixture_blob_memory("Data/README").binary?
    assert !sample_blob_memory("Ruby/foo.rb").binary?
    assert !sample_blob_memory("Perl/script.pl").binary?
  end

  def test_all_binary
    Samples.each do |sample|
      blob = sample_blob_memory(sample[:path])
      assert ! (blob.likely_binary? || blob.binary?), "#{sample[:path]} is a binary file"
    end
  end

  def test_text
    assert fixture_blob_memory("Data/README").text?
    assert fixture_blob_memory("Data/md").text?
    assert sample_blob_memory("Shell/script.sh").text?
    assert fixture_blob_memory("Data/txt").text?
  end

  def test_image
    assert fixture_blob_memory("Binary/octocat.png").image?
    assert !fixture_blob_memory("Binary/octocat.ai").image?
    assert !fixture_blob_memory("Binary/octocat.psd").image?
  end

  def test_solid
    assert fixture_blob_memory("Binary/cube.stl").solid?
    assert fixture_blob_memory("Data/cube.stl").solid?
  end

  def test_csv
    assert sample_blob_memory("CSV/cars.csv").csv?
  end

  def test_pdf
    assert fixture_blob_memory("Binary/foo.pdf").pdf?
  end

  def test_viewable
    assert fixture_blob_memory("Data/README").viewable?
    assert sample_blob_memory("Ruby/foo.rb").viewable?
    assert sample_blob_memory("Perl/script.pl").viewable?
    assert !fixture_blob_memory("Binary/linguist.gem").viewable?
    assert !fixture_blob_memory("Binary/octocat.ai").viewable?
    assert !fixture_blob_memory("Binary/octocat.png").viewable?
  end

  def test_generated
    assert !fixture_blob_memory("Data/README").generated?

    # Generated .NET Docfiles
    assert sample_blob_memory("XML/net_docfile.xml").generated?

    # Long line
    assert !sample_blob_memory("JavaScript/uglify.js").generated?

    # Inlined JS, but mostly code
    assert !sample_blob_memory("JavaScript/json2_backbone.js").generated?

    # Minified JS
    assert !sample_blob_memory("JavaScript/jquery-1.6.1.js").generated?
    assert sample_blob_memory("JavaScript/jquery-1.6.1.min.js").generated?
    assert sample_blob_memory("JavaScript/jquery-1.4.2.min.js").generated?

    # Composer generated composer.lock file
    assert sample_blob_memory("JSON/filenames/composer.lock").generated?

    # PEG.js-generated parsers
    assert sample_blob_memory("JavaScript/parser.js").generated?

    # Generated PostScript
    assert !sample_blob_memory("PostScript/sierpinski.ps").generated?

    # These examples are too basic to tell
    assert !sample_blob_memory("JavaScript/hello.js").generated?

    assert sample_blob_memory("JavaScript/intro-old.js").generated?
    assert sample_blob_memory("JavaScript/classes-old.js").generated?

    assert sample_blob_memory("JavaScript/intro.js").generated?
    assert sample_blob_memory("JavaScript/classes.js").generated?

    assert sample_blob_memory("JavaScript/ccalc-lex.js").generated?
    assert sample_blob_memory("JavaScript/ccalc-parse.js").generated?

    # Protocol Buffer generated code
    assert sample_blob_memory("C++/protocol-buffer.pb.h").generated?
    assert sample_blob_memory("C++/protocol-buffer.pb.cc").generated?
    assert sample_blob_memory("Java/ProtocolBuffer.java").generated?
    assert sample_blob_memory("Python/protocol_buffer_pb2.py").generated?
    assert sample_blob_memory("Go/api.pb.go").generated?
    assert sample_blob_memory("Go/embedded.go").generated?
    assert sample_blob_memory("JavaScript/proto.js").generated?

    # Apache Thrift generated code
    assert sample_blob_memory("Python/gen-py-linguist-thrift.py").generated?
    assert sample_blob_memory("Go/gen-go-linguist-thrift.go").generated?
    assert sample_blob_memory("Java/gen-java-linguist-thrift.java").generated?
    assert sample_blob_memory("JavaScript/gen-js-linguist-thrift.js").generated?
    assert sample_blob_memory("Ruby/gen-rb-linguist-thrift.rb").generated?
    assert sample_blob_memory("Objective-C/gen-cocoa-linguist-thrift.m").generated?
    assert sample_blob_memory("PHP/ThriftGenerated.php").generated?

    # Generated JNI
    assert sample_blob_memory("C/jni_layer.h").generated?

    # Minified CSS
    assert !sample_blob_memory("CSS/bootstrap.css").generated?
    assert sample_blob_memory("CSS/bootstrap.min.css").generated?

    # Generated VCR
    assert sample_blob_memory("YAML/vcr_cassette.yml").generated?

    # Generated by Zephir
    assert !sample_blob_memory("Zephir/Router.zep").generated?

    # Go vendored dependencies
    refute sample_blob("vendor/vendor.json").generated?
    assert sample_blob("vendor/github.com/kr/s3/sign.go").generated?
    refute fixture_blob("go/food_vendor/candy.go").generated?

    # Cython-generated C/C++
    assert sample_blob_memory("C/sgd_fast.c").generated?
    assert sample_blob_memory("C++/wrapper_inner.cpp").generated?

    # Unity3D-generated metadata
    assert sample_blob_memory("Unity3D Asset/Tiles.meta").generated?

    # Racc-generated Ruby
    assert sample_blob_memory("Ruby/racc.rb").generated?

    # protobuf/grpc-plugin C++
    assert sample_blob_memory("C++/hello.grpc.pb.h").generated?
    assert sample_blob_memory("C++/grpc.pb.cc").generated?
  end

  def test_vendored
    assert !fixture_blob_memory("Data/README").vendored?
  end

  def test_language
    Samples.each do |sample|
      blob = sample_blob_memory(sample[:path])
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

        blob = fixture_blob_memory(filepath)
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
    assert !sample_blob_memory("JavaScript/jquery-1.6.1.min.js").safe_to_colorize?
  end

  def test_empty
    blob = Struct.new(:data) { include Linguist::BlobHelper }

    assert blob.new("").empty?
    assert blob.new(nil).empty?
    refute blob.new(" ").empty?
    refute blob.new("nope").empty?
  end

  def test_include_in_language_stats
    generated = sample_blob_memory("CSS/bootstrap.min.css")
    assert_predicate generated, :generated?
    refute_predicate generated, :include_in_language_stats?

    data = sample_blob_memory("Ant Build System/filenames/ant.xml")
    assert_equal :data, data.language.type
    refute_predicate data, :include_in_language_stats?

    prose = sample_blob_memory("Markdown/tender.md")
    assert_equal :prose, prose.language.type
    refute_predicate prose, :include_in_language_stats?

    included = sample_blob_memory("HTML/pages.html")
    assert_predicate included, :include_in_language_stats?
  end
end
