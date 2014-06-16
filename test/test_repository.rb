require 'linguist/repository'

require 'test/unit'

class TestRepository < Test::Unit::TestCase
  include Linguist

  def linguist_repo
    r = Rugged::Repository.new(File.expand_path("../../.git", __FILE__))
    Linguist::Repository.new(r, '31921838cdc252536ec07668f73d4b64d8022750')
  end

  def test_linguist_language
    # assert_equal Language['Ruby'], linguist_repo.language
  end

  def test_linguist_languages
    # assert linguist_repo.languages[Language['Ruby']] > 10_000
  end

  def test_linguist_size
    assert linguist_repo.size > 30_000
  end

  def test_linguist_breakdown
    assert linguist_repo.breakdown_by_file.has_key?("Ruby")
    assert linguist_repo.breakdown_by_file["Ruby"].include?("bin/linguist")
    assert linguist_repo.breakdown_by_file["Ruby"].include?("lib/linguist/language.rb")
  end
end
