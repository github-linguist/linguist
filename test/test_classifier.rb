require_relative "./helper"

class TestClassifier < Minitest::Test
  include Linguist

  def fixture(name)
    File.read(File.join(samples_path, name))
  end

  def test_classify
    db = {}
    Classifier.train! db, "Ruby", fixture("Ruby/foo.rb")
    Classifier.train! db, "Objective-C", fixture("Objective-C/Foo.h")
    Classifier.train! db, "Objective-C", fixture("Objective-C/Foo.m")

    results = Classifier.classify(db, fixture("Objective-C/hello.m"))
    assert_equal "Objective-C", results.first[0]

    tokens  = Tokenizer.tokenize(fixture("Objective-C/hello.m"))
    results = Classifier.classify(db, tokens)
    assert_equal "Objective-C", results.first[0]
  end

  def test_restricted_classify
    db = {}
    Classifier.train! db, "Ruby", fixture("Ruby/foo.rb")
    Classifier.train! db, "Objective-C", fixture("Objective-C/Foo.h")
    Classifier.train! db, "Objective-C", fixture("Objective-C/Foo.m")

    results = Classifier.classify(db, fixture("Objective-C/hello.m"), ["Objective-C"])
    assert_equal "Objective-C", results.first[0]

    results = Classifier.classify(db, fixture("Objective-C/hello.m"), ["Ruby"])
    assert_equal "Ruby", results.first[0]
  end

  def test_instance_classify_empty
    results = Classifier.classify(Samples.cache, "")
    assert results.first[1] < 0.5, results.first.inspect
  end

  def test_instance_classify_nil
    assert_equal [], Classifier.classify(Samples.cache, nil)
  end

  def test_classify_ambiguous_languages
    # Failures are reasonable in some cases, such as when a file is fully valid in more than one language.
    allowed_failures = {
      "#{samples_path}/C++/rpc.h" => ["C", "C++"],
    }

    # Skip extensions with catch-all rule
    skip_extensions = Set.new
    Heuristics.all.each do |h|
      rules = h.instance_variable_get(:@rules)
      if rules[-1]['pattern'].is_a? AlwaysMatch
        skip_extensions |= Set.new(h.extensions)
      end
    end

    Samples.each do |sample|
      next if skip_extensions.include? sample[:extname]

      language  = Linguist::Language.find_by_name(sample[:language])
      languages = Language.find_by_filename(sample[:path]).map(&:name)
      next if languages.length == 1

      languages = Language.find_by_extension(sample[:path]).map(&:name)
      next if languages.length <= 1

      results = Classifier.classify(Samples.cache, File.read(sample[:path]), languages)

      if allowed_failures.has_key? sample[:path]
        assert allowed_failures[sample[:path]].include?(results.first[0]), "#{sample[:path]}\n#{results.inspect}"
      else
        assert_equal language.name, results.first[0], "#{sample[:path]}\n#{results.inspect}"
      end
    end
  end

  def test_classify_empty_languages
    assert_equal [], Classifier.classify(Samples.cache, fixture("Ruby/foo.rb"), [])
  end
end
