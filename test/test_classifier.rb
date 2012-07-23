require 'linguist/classifier'
require 'linguist/language'
require 'linguist/sample'
require 'linguist/tokenizer'
require 'linguist/md5'

require 'test/unit'

class TestClassifier < Test::Unit::TestCase
  include Linguist

  def samples_path
    File.expand_path("../../samples", __FILE__)
  end

  def fixture(name)
    File.read(File.join(samples_path, name))
  end

  def test_instance_freshness
    serialized = Linguist::MD5.hexdigest(Classifier.instance.to_hash)
    latest     = Linguist::MD5.hexdigest(Linguist::Sample.classifier.to_hash)

    # Just warn, it shouldn't scare people off by breaking the build.
    if serialized != latest
      warn "Classifier database is out of date. Run `bundle exec rake classifier`."
    end
  end

  def test_classify
    classifier = Classifier.new
    classifier.train "Ruby", fixture("ruby/foo.rb")
    classifier.train "Objective-C", fixture("objective-c/Foo.h")
    classifier.train "Objective-C", fixture("objective-c/Foo.m")

    results = classifier.classify(fixture("objective-c/hello.m"))
    assert_equal "Objective-C", results.first[0]

    tokens  = Tokenizer.tokenize(fixture("objective-c/hello.m"))
    results = classifier.classify(tokens)
    assert_equal "Objective-C", results.first[0]
  end

  def test_restricted_classify
    classifier = Classifier.new
    classifier.train "Ruby", fixture("ruby/foo.rb")
    classifier.train "Objective-C", fixture("objective-c/Foo.h")
    classifier.train "Objective-C", fixture("objective-c/Foo.m")

    results = classifier.classify(fixture("objective-c/hello.m"), ["Objective-C"])
    assert_equal "Objective-C", results.first[0]

    results = classifier.classify(fixture("objective-c/hello.m"), ["Ruby"])
    assert_equal "Ruby", results.first[0]
  end

  def test_instance_classify_empty
    results = Classifier.instance.classify("")
    assert results.first[1] < 0.5, results.first.inspect
  end

  def test_instance_classify_nil
    assert_equal [], Classifier.instance.classify(nil)
  end

  def test_verify
    data = Classifier.instance.to_hash

    assert_equal data['languages_total'], data['languages'].inject(0) { |n, (_, c)| n += c }
    assert_equal data['tokens_total'], data['language_tokens'].inject(0) { |n, (_, c)| n += c }
    assert_equal data['tokens_total'], data['tokens'].inject(0) { |n, (_, ts)| n += ts.inject(0) { |m, (_, c)| m += c } }
  end

  def test_gc
    Classifier.instance.gc
  end

  def test_classify_ambiguous_languages
    Sample.each do |sample|
      language = Linguist::Language.find_by_alias(sample[:language])
      next unless language.overrides.any?

      extname   = File.extname(sample[:path])
      languages = Language.all.select { |l| l.extensions.include?(extname) }.map(&:name)
      next unless languages.length > 1

      results = Classifier.instance.classify(File.read(sample[:path]), languages)
      assert_equal language.name, results.first[0], "#{sample[:path]}\n#{results.inspect}"
    end
  end
end
