require 'linguist/classifier'
require 'linguist/language'
require 'linguist/sample'
require 'linguist/tokenizer'

require 'test/unit'

class TestClassifier < Test::Unit::TestCase
  include Linguist

  def fixtures_path
    File.expand_path("../fixtures", __FILE__)
  end

  def fixture(name)
    File.read(File.join(fixtures_path, name))
  end

  def test_instance_freshness
    # Just warn, it shouldn't scare people off by breaking the build.
    unless Classifier.instance.eql?(Linguist::Sample.classifier)
      warn "Classifier database is out of date. Run `bundle exec rake classifier`."
    end
  end

  def test_classify
    classifier = Classifier.new
    classifier.train Language["Ruby"], fixture("ruby/foo.rb")
    classifier.train Language["Objective-C"], fixture("objective-c/Foo.h")
    classifier.train Language["Objective-C"], fixture("objective-c/Foo.m")

    results = classifier.classify(fixture("objective-c/hello.m"))
    assert_equal Language["Objective-C"], results.first[0]

    tokens  = Tokenizer.new(fixture("objective-c/hello.m")).tokens
    results = classifier.classify(tokens)
    assert_equal Language["Objective-C"], results.first[0]
  end

  def test_restricted_classify
    classifier = Classifier.new
    classifier.train Language["Ruby"], fixture("ruby/foo.rb")
    classifier.train Language["Objective-C"], fixture("objective-c/Foo.h")
    classifier.train Language["Objective-C"], fixture("objective-c/Foo.m")

    results = classifier.classify(fixture("objective-c/hello.m"), [Language["Objective-C"]])
    assert_equal Language["Objective-C"], results.first[0]

    results = classifier.classify(fixture("objective-c/hello.m"), [Language["Ruby"]])
    assert_equal Language["Ruby"], results.first[0]
  end

  def test_instance_classify_empty
    results = Classifier.instance.classify("")
    assert results.first[1] < 0.5, results.first.inspect
  end

  def test_verify
    assert Classifier.instance.verify
  end

  def test_gc
    Classifier.instance.gc
  end

  def test_classify_ambiguous_languages
    Sample.each do |sample|
      # TODO: These tests are pending
      next if sample.path =~ /hello.h/
      next if sample.path =~ /MainMenuViewController.h/

      next unless sample.language.overrides.any?

      extname   = File.extname(sample.path)
      languages = Language.all.select { |l| l.extensions.include?(extname) }
      next unless languages.length > 1

      results = Classifier.instance.classify(sample.data, languages)
      assert_equal sample.language, results.first[0], "#{sample.path}\n#{results.inspect}"
    end
  end
end
