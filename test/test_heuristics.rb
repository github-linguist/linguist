require 'linguist/heuristics'
require 'linguist/language'
require 'linguist/samples'

require 'test/unit'

class TestHeuristcs < Test::Unit::TestCase
  include Linguist

  def samples_path
    File.expand_path("../../samples", __FILE__)
  end

  def fixture(name)
    File.read(File.join(samples_path, name))
  end

  def test_find_by_heuristics
    languages = ["C++", "Objective-C"]
    results = Heuristics.find_by_heuristics(fixture("Objective-C/StyleViewController.h"), languages)
    assert_equal Language["Objective-C"], results.first
  end

  def test_detect_still_works_if_nothing_matches
    match = Language.detect("Hello.m", fixture("Objective-C/hello.m"))
    assert_equal Language["Objective-C"], match
  end
end
