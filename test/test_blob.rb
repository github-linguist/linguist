require 'linguist/blob'

require 'test/unit'
require 'mime/types'

class TestBlob < Test::Unit::TestCase
  include Linguist

  class FixtureBlob
    def initialize(name)
      @name = name
      @path = File.expand_path("../fixtures/blob/#{name}", __FILE__)
    end

    def name
      @name
    end

    def data
      File.read(@path)
    end

    def size
      File.size(@path)
    end
  end

  def blob(name)
    Blob.new(FixtureBlob.new(name))
  end

  def test_name
    assert_equal "foo.rb", blob("foo.rb").name
  end

  def test_pathname
    assert_equal Pathname.new("foo.rb"), blob("foo.rb").pathname
  end

  def test_mime_type
    assert_equal "text/plain", blob("grit.rb").mime_type
    assert_equal "application/xml", blob("bar.xml").mime_type
    assert_equal "text/plain", blob("dog.o").mime_type
  end

  def test_special_mime_type
    assert_equal "text/plain; charset=utf-8", blob("grit.rb").special_mime_type
    assert_equal "text/plain; charset=utf-8", blob("bar.xml").special_mime_type
    assert_equal "application/octet-stream", blob("dog.o").special_mime_type
  end

  def test_disposition
    assert_equal "attachment; filename=foo.bin", blob("foo.bin").disposition
    assert_equal "attachment; filename=linguist.gem", blob("pkg/linguist.gem").disposition
    assert_equal "attachment; filename=foo+bar.jar", blob("foo bar.jar").disposition
    assert_equal "inline", blob("foo.txt").disposition
    assert_equal "inline", blob("grit.rb").disposition
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

  def test_binary
    assert blob("linguist.gem").binary?
    assert blob("git.deb").binary?
    assert blob("git.exe").binary?
    assert !blob("file.txt").binary?
    assert !blob("octocat.png").binary?
  end

  def test_file
    assert blob("octocat.png").file?
    assert blob("linguist.gem").file?
  end

  def test_text
    assert blob("file.txt").text?
    assert blob("file.json").text?
  end

  def test_image
    assert blob("octocat.png").image?
    assert blob("octocat.jpg").image?
    assert blob("octocat.jpeg").image?
    assert blob("octocat.gif").image?
    assert !blob("octocat.psd").image?
  end

  def test_language
    assert_equal Language['Ruby'], blob("foo.rb").language
    assert_equal Language['Ruby'], blob("script.rb").language
    assert_equal Language['Text'], blob("octocat.png").language
  end

  def test_lexer
    assert_equal 'ruby', blob("grit.rb").lexer
    assert_equal 'text', blob("README").lexer
    assert_equal 'diff', blob("dude-thing-okay--001.patch").lexer
    assert_equal 'scheme', blob("dude.el").lexer
    assert_equal 'javascript', blob("dude.js").lexer
    assert_equal 'ruby', blob("Capfile").lexer

    assert_equal 'ruby', blob("Rakefile").lexer
    assert_equal 'ruby', blob("subdir/Rakefile").lexer
  end

  def test_shebang_script
    assert_equal 'sh', blob("script.sh").shebang_script
    assert_equal 'bash', blob("script.bash").shebang_script
    assert_equal 'zsh', blob("script.zsh").shebang_script
    assert_equal 'perl', blob("script.pl").shebang_script
    assert_equal 'ruby', blob("script.rb").shebang_script
    assert_equal 'ruby', blob("script2.rb").shebang_script
    assert_equal 'python', blob("script.py").shebang_script
    assert_equal 'node', blob("script.js").shebang_script
    assert_equal 'groovy', blob("script.groovy").shebang_script
    assert_equal 'macruby', blob("script.mrb").shebang_script
    assert_equal 'rake', blob("script.rake").shebang_script
    assert_equal 'foo', blob("script.foo").shebang_script
    assert_equal nil, blob("foo.rb").shebang_script
  end

  def test_shebang_language
    assert_equal Language['Shell'], blob("script.sh").shebang_language
    assert_equal Language['Shell'], blob("script.bash").shebang_language
    assert_equal Language['Shell'], blob("script.zsh").shebang_language
    assert_equal Language['Perl'], blob("script.pl").shebang_language
    assert_equal Language['Ruby'], blob("script.rb").shebang_language
    assert_equal Language['Python'], blob("script.py").shebang_language
    assert_equal Language['JavaScript'], blob("script.js").shebang_language
    assert_equal Language['Java'], blob("script.groovy").shebang_language
    assert_equal Language['Ruby'], blob("script.mrb").shebang_language
    assert_equal Language['Ruby'], blob("script.rake").shebang_language
    assert_equal nil, blob("script.foo").shebang_language
    assert_equal nil, blob("foo.rb").shebang_language
  end
end
