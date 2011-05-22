require 'linguist/repository'

require 'test/unit'

class TestRepository < Test::Unit::TestCase
  include Linguist

  class FixtureBlob
    def initialize(name, path)
      @name = name
      @path = path
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

  def repo(base_path)
    paths = Dir["#{base_path}/**/*"].inject({}) do |h, path|
      if File.file?(path)
        name = path.sub("#{base_path}/", '')
        h[name] = Blob.new(FixtureBlob.new(name, path))
      end
      h
    end
    Repository.new(paths)
  end

  def linguist_repo
    repo(File.expand_path("../..", __FILE__))
  end

  def test_lookup_path
    assert linguist_repo['lib/linguist.rb']
    assert_equal Language['Ruby'], linguist_repo['lib/linguist.rb'].language
  end

  def test_linguist_language
    assert_equal Language['Ruby'], linguist_repo.language
  end

  def test_linguist_languages
    assert linguist_repo.languages[Language['Ruby']] > 30_000
    assert linguist_repo.languages[Language['Python']] < 1000
  end

  def test_linguist_size
    assert linguist_repo.size > 30_000
  end
end
