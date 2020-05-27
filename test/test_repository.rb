require_relative "./helper"

class TestRepository < Minitest::Test
  def rugged_repository
    @rugged ||= Rugged::Repository.new(File.expand_path("../../.git", __FILE__))
  end

  def master_oid
    '7dbcffcf982e766fc711e633322de848f2b60ba5'
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
    assert linguist_repo.breakdown_by_file["Ruby"].include?("bin/github-linguist")
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
    # See https://github.com/github/linguist/blob/72a89fc9dcd3585250056ab591f9d7e2411d5fa1/.gitattributes
    #
    # It looks like this:
    # Gemfile linguist-vendored=true
    # lib/linguist.rb linguist-language=Java
    # test/*.rb linguist-language=Java
    # Rakefile linguist-generated
    # test/fixtures/** linguist-vendored=false
    # README.md linguist-documentation=false
    # samples/Arduino/* linguist-documentation
    # samples/Markdown/*.md linguist-detectable=true
    # samples/HTML/*.html linguist-detectable=false
    # samples/CSS/bootstrap.css -linguist-vendored
    # samples/CSS/bootstrap.min.css -linguist-generated
    # LICENSE -linguist-documentation
    # samples/CoffeeScript/browser.coffee -linguist-detectable

    attr_commit = '72a89fc9dcd3585250056ab591f9d7e2411d5fa1'
    repo = linguist_repo(attr_commit)

    assert repo.breakdown_by_file.has_key?("Java")
    assert repo.breakdown_by_file["Java"].include?("lib/linguist.rb")

    assert repo.breakdown_by_file.has_key?("Ruby")
    assert !repo.breakdown_by_file["Ruby"].empty?

    # Ensures the filename that contains unicode char is UTF-8 encoded and invalid chars scrubbed
    assert repo.breakdown_by_file.has_key?("Perl")
    assert repo.breakdown_by_file["Perl"].include?("test/fixtures/ba�r/file_ã.pl")
    assert_equal "UTF-8", repo.breakdown_by_file["Perl"].first.encoding.to_s
    assert repo.breakdown_by_file["Perl"].first.valid_encoding?
  end

  def test_commit_with_git_attributes_data
    # Before we had any .gitattributes data
    old_commit = '4a017d9033f91b2776eb85275463f9613cc371ef'
    old_repo = linguist_repo(old_commit)

    # With some .gitattributes data
    attr_commit = '7ee006cbcb2d7261f9e648510a684ee9ac64126b'
    # It's incremental but should bust the cache
    new_repo = Linguist::Repository.incremental(rugged_repository, attr_commit, old_commit, old_repo.cache)

    assert new_repo.breakdown_by_file["Java"].include?("lib/linguist.rb")
  end

  def test_linguist_override_vendored?
    attr_commit = '72a89fc9dcd3585250056ab591f9d7e2411d5fa1'
    linguist_repo(attr_commit).read_index

    override_vendored = Linguist::LazyBlob.new(rugged_repository, attr_commit, 'Gemfile')

    # overridden .gitattributes
    assert override_vendored.vendored?
  end

  def test_linguist_override_unvendored?
    attr_commit = '01d6b9c637a7a6581fe456c600725b68f355b295'
    linguist_repo(attr_commit).read_index

    # lib/linguist/vendor.yml defines this as vendored.
    override_unvendored = Linguist::LazyBlob.new(rugged_repository, attr_commit, 'test/fixtures/foo.rb')
    # test -linguist-vendored attribute method
    override_unvendored_minus = Linguist::LazyBlob.new(rugged_repository, attr_commit, 'samples/CSS/bootstrap.css')

    # overridden .gitattributes
    refute override_unvendored.vendored?
    refute override_unvendored_minus.vendored?
  end

  def test_linguist_override_documentation?
    attr_commit = "01d6b9c637a7a6581fe456c600725b68f355b295"
    linguist_repo(attr_commit).read_index

    readme = Linguist::LazyBlob.new(rugged_repository, attr_commit, "README.md")
    arduino = Linguist::LazyBlob.new(rugged_repository, attr_commit, "samples/Arduino/hello.ino")
    # test -linguist-documentation attribute method
    minus = Linguist::LazyBlob.new(rugged_repository, attr_commit, "LICENSE")

    # overridden by .gitattributes
    refute_predicate readme, :documentation?
    assert_predicate arduino, :documentation?
    refute_predicate minus, :documentation?
  end

  def test_linguist_override_generated?
    attr_commit = "01d6b9c637a7a6581fe456c600725b68f355b295"
    linguist_repo(attr_commit).read_index

    rakefile = Linguist::LazyBlob.new(rugged_repository, attr_commit, "Rakefile")
    # test  -linguist-generated attribute method
    minus = Linguist::LazyBlob.new(rugged_repository, attr_commit, "samples/CSS/bootstrap.min.css")
    # overridden .gitattributes
    assert rakefile.generated?
    refute minus.generated?
  end

  def test_linguist_override_detectable?
    attr_commit = "01d6b9c637a7a6581fe456c600725b68f355b295"
    linguist_repo(attr_commit).read_index

    # markdown is overridden by .gitattributes to be detectable, html to not be detectable
    markdown = Linguist::LazyBlob.new(rugged_repository, attr_commit, "samples/Markdown/tender.md")
    html = Linguist::LazyBlob.new(rugged_repository, attr_commit, "samples/HTML/pages.html")
    # test  -linguist-detectable attribute method
    minus = Linguist::LazyBlob.new(rugged_repository, attr_commit, "samples/CoffeeScript/browser.coffee")

    assert_predicate markdown, :detectable?
    refute_predicate html, :detectable?
    refute_predicate minus, :detectable?
  end
end
