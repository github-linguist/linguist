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

  def all_fixtures(language_name, file="*")
    Dir.glob("#{samples_path}/#{language_name}/#{file}")
  end

  def test_obj_c_by_heuristics
    languages = ["C++", "Objective-C"]
    # Only calling out '.h' filenames as these are the ones causing issues
    all_fixtures("Objective-C", "*.h").each do |fixture|
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
  
  def test_ecl_prolog_by_heuristics
    languages = ["ECL", "Prolog"]
    results = Heuristics.disambiguate_ecl(fixture("Prolog/or-constraint.ecl"), languages)
    assert_equal Language["Prolog"], results.first
  end
  
  def test_ecl_ecl_by_heuristics
    languages = ["ECL", "Prolog"]
    results = Heuristics.disambiguate_ecl(fixture("ECL/sample.ecl"), languages)
    assert_equal Language["ECL"], results.first
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

  def test_cl_by_heuristics
    languages = ["Common Lisp", "OpenCL"]
    languages.each do |language|
      all_fixtures(language).each do |fixture|
        results = Heuristics.disambiguate_cl(fixture("#{language}/#{File.basename(fixture)}"), languages)
        assert_equal Language[language], results.first
      end
    end
  end

  def test_objc_m_matlab_heuristics
    languages = ["Objective-C", "M", "Matlab"]
    objc, m, matlab = Language["Objective-C"], Language["M"], Language["Matlab"]

    assert_equal [], Heuristics.disambiguate_objc("", languages)

    assert_equal [objc], Heuristics.disambiguate_objc("#import", languages)
    assert_equal [objc, matlab], Heuristics.disambiguate_objc("% #import", languages) #Matlab comment
    assert_equal [objc, matlab], Heuristics.disambiguate_objc("'#import'", languages) #Matlab string
    assert_equal [objc, matlab], Heuristics.disambiguate_objc("""
      %{
        #import
      %}
      """.strip, languages) #Matlab multi-line comment
    assert_equal [objc, m], Heuristics.disambiguate_objc("; #import", languages) #M comment
    assert_equal [objc, m], Heuristics.disambiguate_objc('"#import"', languages) #M string
    assert_equal [objc, m], Heuristics.disambiguate_objc("3#import", languages) #M modulo

    assert_equal [objc, matlab], Heuristics.disambiguate_objc("@import", languages) #Matlab function handle
    assert_equal [objc], Heuristics.disambiguate_objc("@import Foundation;", languages)
    assert_equal [objc, matlab], Heuristics.disambiguate_objc("% @import Foundation;", languages) #Matlab comment
    assert_equal [objc, matlab], Heuristics.disambiguate_objc("'@import Foundation;'", languages) #Matlab string
    assert_equal [objc, matlab], Heuristics.disambiguate_objc("""
      %{
        @import Foundation;
      %}
      """.strip, languages) #Matlab multi-line comment
    assert_equal [objc, m], Heuristics.disambiguate_objc("; @import Foundation;", languages) #M comment
    assert_equal [objc, m], Heuristics.disambiguate_objc('"@import Foundation;"', languages) #M string
  end
end
