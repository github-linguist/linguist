require 'linguist/heuristics'
require 'linguist/language'
require 'linguist/samples'
require 'linguist/file_blob'

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

  # Candidate languages = ["C++", "Objective-C"]
  def test_obj_c_by_heuristics
    # Only calling out '.h' filenames as these are the ones causing issues
    all_fixtures("Objective-C", "*.h").each do |fixture|
      results = Heuristics.disambiguate_c(fixture("Objective-C/#{File.basename(fixture)}"))
      assert_equal Language["Objective-C"], results.first
    end
  end

  # Candidate languages = ["C++", "Objective-C"]
  def test_cpp_by_heuristics
    results = Heuristics.disambiguate_c(fixture("C++/render_adapter.cpp"))
    assert_equal Language["C++"], results.first
  end

  def test_detect_still_works_if_nothing_matches
    blob = Linguist::FileBlob.new(File.join(samples_path, "Objective-C/hello.m"))
    match = Language.detect(blob)
    assert_equal Language["Objective-C"], match
  end

  # Candidate languages = ["Perl", "Prolog"]
  def test_pl_prolog_by_heuristics
    results = Heuristics.disambiguate_pl(fixture("Prolog/turing.pl"))
    assert_equal Language["Prolog"], results.first
  end

  # Candidate languages = ["Perl", "Prolog"]
  def test_pl_perl_by_heuristics
    results = Heuristics.disambiguate_pl(fixture("Perl/perl-test.t"))
    assert_equal Language["Perl"], results.first
  end

  # Candidate languages = ["ECL", "Prolog"]
  def test_ecl_prolog_by_heuristics
    results = Heuristics.disambiguate_ecl(fixture("Prolog/or-constraint.ecl"))
    assert_equal Language["Prolog"], results.first
  end

  # Candidate languages = ["ECL", "Prolog"]
  def test_ecl_ecl_by_heuristics
    results = Heuristics.disambiguate_ecl(fixture("ECL/sample.ecl"))
    assert_equal Language["ECL"], results.first
  end

  # Candidate languages = ["IDL", "Prolog"]
  def test_pro_prolog_by_heuristics
    results = Heuristics.disambiguate_pro(fixture("Prolog/logic-problem.pro"))
    assert_equal Language["Prolog"], results.first
  end

  # Candidate languages = ["IDL", "Prolog"]
  def test_pro_idl_by_heuristics
    results = Heuristics.disambiguate_pro(fixture("IDL/mg_acosh.pro"))
    assert_equal Language["IDL"], results.first
  end

  # Candidate languages = ["AGS Script", "AsciiDoc"]
  def test_asc_asciidoc_by_heuristics
    results = Heuristics.disambiguate_asc(fixture("AsciiDoc/list.asc"))
    assert_equal Language["AsciiDoc"], results.first
  end

  # Candidate languages = ["TypeScript", "XML"]
  def test_ts_typescript_by_heuristics
    results = Heuristics.disambiguate_ts(fixture("TypeScript/classes.ts"))
    assert_equal Language["TypeScript"], results.first
  end

  # Candidate languages = ["TypeScript", "XML"]
  def test_ts_xml_by_heuristics
    results = Heuristics.disambiguate_ts(fixture("XML/pt_BR.xml"))
    assert_equal Language["XML"], results.first
  end

  def test_cl_by_heuristics
    languages = ["Common Lisp", "OpenCL"]
    languages.each do |language|
      all_fixtures(language).each do |fixture|
        results = Heuristics.disambiguate_cl(fixture("#{language}/#{File.basename(fixture)}"))
        assert_equal Language[language], results.first
      end
    end
  end

  def test_f_by_heuristics
    languages = ["FORTRAN", "Forth"]
    languages.each do |language|
      all_fixtures(language).each do |fixture|
        results = Heuristics.disambiguate_f(fixture("#{language}/#{File.basename(fixture)}"))
        assert_equal Language[language], results.first
      end
    end
  end

  # Candidate languages = ["Hack", "PHP"]
  def test_hack_by_heuristics
    results = Heuristics.disambiguate_hack(fixture("Hack/funs.php"))
    assert_equal Language["Hack"], results.first
  end

  # Candidate languages = ["Scala", "SuperCollider"]
  def test_sc_supercollider_by_heuristics
    results = Heuristics.disambiguate_sc(fixture("SuperCollider/WarpPreset.sc"))
    assert_equal Language["SuperCollider"], results.first
  end

  # Candidate languages = ["Scala", "SuperCollider"]
  def test_sc_scala_by_heuristics
    results = Heuristics.disambiguate_sc(fixture("Scala/node11.sc"))
    assert_equal Language["Scala"], results.first
  end
end
