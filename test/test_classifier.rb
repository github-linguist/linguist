require 'linguist/classifier'
require 'linguist/language'

require 'test/unit'

class TestClassifier < Test::Unit::TestCase
  include Linguist

  def fixtures_path
    File.expand_path("../fixtures", __FILE__)
  end

  def fixture(name)
    File.read(File.join(fixtures_path, name))
  end

  def test_truth
    classifier = Classifier.new
    classifier.train Language["Ruby"], fixture("ruby/foo.rb")
    classifier.train Language["Objective-C"], fixture("objective-c/Foo.h")
    classifier.train Language["Objective-C"], fixture("objective-c/Foo.m")

    results = classifier.classify(fixture("objective-c/hello.m"))
    assert_equal Language["Objective-C"], results.first[0]
  end
end
