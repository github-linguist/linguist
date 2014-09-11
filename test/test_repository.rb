require 'linguist/repository'
require 'test/unit'

class TestRepository < Test::Unit::TestCase
  def rugged_repository
    @rugged ||= Rugged::Repository.new(File.expand_path("../../.git", __FILE__))
  end

  def master_oid
    'd40b4a33deba710e2f494db357c654fbe5d4b419'
  end

  def linguist_repo(oid = master_oid)
    Linguist::Repository.new(rugged_repository, oid)
  end

  def test_linguist_language
    assert_equal 'Ruby', linguist_repo.language
  end

  def test_linguist_languages
    assert linguist_repo.languages['Ruby'] > 10_000
  end

  def test_linguist_size
    assert linguist_repo.size > 30_000
  end

  def test_linguist_breakdown
    assert linguist_repo.breakdown_by_file.has_key?("Ruby")
    assert linguist_repo.breakdown_by_file["Ruby"].include?("bin/linguist")
    assert linguist_repo.breakdown_by_file["Ruby"].include?("lib/linguist/language.rb")
  end

  def test_incremental_stats
    old_commit = '3d7364877d6794f6cc2a86b493e893968a597332'
    old_repo = linguist_repo(old_commit)

    assert old_repo.languages['Ruby'] > 10_000
    assert old_repo.size > 30_000

    new_repo = Linguist::Repository.incremental(rugged_repository, master_oid, old_commit, old_repo.cache)

    assert new_repo.languages['Ruby'] > old_repo.languages['Ruby']
    assert new_repo.size > old_repo.size

    assert_equal linguist_repo.cache, new_repo.cache
  end

  def test_git_attributes
    # See https://github.com/github/linguist/blob/525304738ebdb7ab3b7d2bf9a7514cc428faa273/.gitattributes
    #
    # It looks like this:
    # test/*.rb linguist-ignore
    # lib/linguist.rb linguist-lang=Java
    attr_commit = '525304738ebdb7ab3b7d2bf9a7514cc428faa273'
    repo = linguist_repo(attr_commit)

    assert repo.breakdown_by_file.has_key?("Java")
    assert repo.breakdown_by_file["Java"].include?("lib/linguist.rb")

    assert repo.breakdown_by_file.has_key?("Ruby")
    assert !repo.breakdown_by_file["Ruby"].empty?
    repo.breakdown_by_file["Ruby"].each do |file|
      assert !file.start_with?("test/")
    end
  end
end
