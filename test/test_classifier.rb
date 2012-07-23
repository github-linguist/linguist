require 'linguist/classifier'
require 'linguist/language'
require 'linguist/samples'
require 'linguist/tokenizer'

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
    # Just warn, it shouldn't scare people off by breaking the build.
    if Samples.outdated?
      warn "Classifier database is out of date. Run `bundle exec rake classifier`."
    end
  end

  def test_classify
    db = {}
    Classifier.train! db, "Ruby", fixture("ruby/foo.rb")
    Classifier.train! db, "Objective-C", fixture("objective-c/Foo.h")
    Classifier.train! db, "Objective-C", fixture("objective-c/Foo.m")

    results = Classifier.classify(db, fixture("objective-c/hello.m"))
    assert_equal "Objective-C", results.first[0]

    tokens  = Tokenizer.tokenize(fixture("objective-c/hello.m"))
    results = Classifier.classify(db, tokens)
    assert_equal "Objective-C", results.first[0]
  end

  def test_restricted_classify
    db = {}
    Classifier.train! db, "Ruby", fixture("ruby/foo.rb")
    Classifier.train! db, "Objective-C", fixture("objective-c/Foo.h")
    Classifier.train! db, "Objective-C", fixture("objective-c/Foo.m")

    results = Classifier.classify(db, fixture("objective-c/hello.m"), ["Objective-C"])
    assert_equal "Objective-C", results.first[0]

    results = Classifier.classify(db, fixture("objective-c/hello.m"), ["Ruby"])
    assert_equal "Ruby", results.first[0]
  end

  def test_instance_classify_empty
    results = Classifier.classify(Samples::DATA, "")
    assert results.first[1] < 0.5, results.first.inspect
  end

  def test_instance_classify_nil
    assert_equal [], Classifier.classify(Samples::DATA, nil)
  end

  def test_verify
    data = Samples::DATA

    assert_equal data['languages_total'], data['languages'].inject(0) { |n, (_, c)| n += c }
    assert_equal data['tokens_total'], data['language_tokens'].inject(0) { |n, (_, c)| n += c }
    assert_equal data['tokens_total'], data['tokens'].inject(0) { |n, (_, ts)| n += ts.inject(0) { |m, (_, c)| m += c } }
  end

  def test_classify_ambiguous_languages
    Samples.each do |sample|
      language = Linguist::Language.find_by_alias(sample[:language])
      next unless language.overrides.any?

      extname   = File.extname(sample[:path])
      languages = Language.all.select { |l| l.extensions.include?(extname) }.map(&:name)
      next unless languages.length > 1

      results = Classifier.classify(Samples::DATA, File.read(sample[:path]), languages)
      assert_equal language.name, results.first[0], "#{sample[:path]}\n#{results.inspect}"
    end
  end
end
