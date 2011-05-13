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

    def mime_type
      guesses = ::MIME::Types.type_for(name)
      orginal_type = guesses.first ? guesses.first.simplified : 'text/plain'
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
    assert_equal Pathname.new("foo.rb"), blob("foo.rb").name
  end

  def test_mime_type
    assert_equal "application/ruby", blob("foo.rb").mime_type
  end

  def test_data
    assert_equal "module Foo\nend\n", blob("foo.rb").data
  end

  def test_size
    assert_equal 15, blob("foo.rb").size
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
end
