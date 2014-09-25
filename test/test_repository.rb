require 'linguist/repository'
require 'linguist/lazy_blob'
require 'test/unit'
require 'pry'
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

  def test_repo_git_attributes
    # See https://github.com/github/linguist/blob/b533b682d5d4012ca42f4fc998b45169ec41fe33/.gitattributes
    #
    # It looks like this:
    # Gemfile linguist-vendored=true
    # lib/linguist.rb linguist-language=Java
    # test/*.rb linguist-language=Java
    # Rakefile linguist-generated
    # test/fixtures/* linguist-vendored=false


    attr_commit = 'b533b682d5d4012ca42f4fc998b45169ec41fe33'
    repo = linguist_repo(attr_commit)

    assert repo.breakdown_by_file.has_key?("Java")
    assert repo.breakdown_by_file["Java"].include?("lib/linguist.rb")

    assert repo.breakdown_by_file.has_key?("Ruby")
    assert !repo.breakdown_by_file["Ruby"].empty?
  end


  def test_linguist_generated?
    attr_commit = 'b533b682d5d4012ca42f4fc998b45169ec41fe33'
    file = Linguist::LazyBlob.new(rugged_repository, attr_commit, 'Rakefile')


    # check we're getting the correct assignment back from .gitattributes
    assert file.result_for_key('linguist-generated')
    # overridden in .gitattributes
    assert file.linguist_generated?
    # from lib/linguist/generated.rb
    assert !file.generated?
  end

  def test_linguist_override_vendored?
    attr_commit = 'b533b682d5d4012ca42f4fc998b45169ec41fe33'
    override_vendored = Linguist::LazyBlob.new(rugged_repository, attr_commit, 'Gemfile')

    # check we're getting the correct assignment back from .gitattributes
    assert override_vendored.result_for_key('linguist-vendored')
    # overridden .gitattributes
    assert override_vendored.linguist_vendored?
    # from lib/linguist/vendor.yml
    assert !override_vendored.vendored?
  end

  def test_linguist_override_unvendored?
    attr_commit = 'b533b682d5d4012ca42f4fc998b45169ec41fe33'

    # lib/linguist/vendor.yml defines this as vendored.
    override_unvendored = Linguist::LazyBlob.new(rugged_repository, attr_commit, 'test/fixtures/foo.rb')

    # check we're getting the correct assignment back from .gitattributes
    assert !override_unvendored.result_for_key('linguist-vendored')
    # overridden .gitattributes
    assert !override_unvendored.linguist_vendored?
    # from lib/linguist/vendor.yml
    assert override_unvendored.vendored?
  end
end
