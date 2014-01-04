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
      results = Heuristics.disambiguate_c(fixture("Objective-C/#{File.basename(fixture)}"), languages)
      assert_equal Language["Objective-C"], results.first
    end
  end

  def test_cpp_by_heuristics
    languages = ["C++", "Objective-C"]
    results = Heuristics.disambiguate_c(fixture("C++/render_adapter.cpp"), languages)
    assert_equal Language["C++"], results.first
  end

  def test_detect_still_works_if_nothing_matches
    match = Language.detect("Hello.m", fixture("Objective-C/hello.m"))
    assert_equal Language["Objective-C"], match
  end
  
  def test_pl_prolog_by_heuristics
    languages = ["Perl", "Prolog"]
    results = Heuristics.disambiguate_pl(fixture("Prolog/turing.pl"), languages)
    assert_equal Language["Prolog"], results.first
  end
  
  def test_pl_perl_by_heuristics
    languages = ["Perl", "Prolog"]
    results = Heuristics.disambiguate_pl(fixture("Perl/perl-test.t"), languages)
    assert_equal Language["Perl"], results.first
  end

  def test_ts_typescript_by_heuristics
    languages = ["TypeScript", "XML"]
    results = Heuristics.disambiguate_ts(fixture("TypeScript/classes.ts"), languages)
    assert_equal Language["TypeScript"], results.first
  end

  def test_ts_xml_by_heuristics
    languages = ["TypeScript", "XML"]
    results = Heuristics.disambiguate_ts(fixture("XML/pt_BR.xml"), languages)
    assert_equal Language["XML"], results.first
  end
end
