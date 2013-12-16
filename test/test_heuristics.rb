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
  
  # Only calling out '.h' filenames as these are the ones causing issues
  def all_h_fixtures(language_name)
    Dir.glob("#{samples_path}/#{language_name}/*.h")
  end

  def test_obj_c_by_heuristics
    languages = ["C++", "Objective-C"]
    all_h_fixtures("Objective-C").each do |fixture|
      results = Heuristics.find_by_heuristics(fixture("Objective-C/#{File.basename(fixture)}"), languages)
      assert_equal Language["Objective-C"], results.first
    end
  end

  def test_detect_still_works_if_nothing_matches
    match = Language.detect("Hello.m", fixture("Objective-C/hello.m"))
    assert_equal Language["Objective-C"], match
  end
end
