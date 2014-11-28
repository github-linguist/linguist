require_relative "./helper"

class TestHeuristcs < Test::Unit::TestCase
  include Linguist

  def samples_path
    File.expand_path("../../samples", __FILE__)
  end

  def fixture(name)
    File.read(File.join(samples_path, name))
  end

  def file_blob(name)
    path = File.exist?(name) ? name : File.join(samples_path, name)
    FileBlob.new(path)
  end

  def all_fixtures(language_name, file="*")
    Dir.glob("#{samples_path}/#{language_name}/#{file}")
  end

  def test_detect_still_works_if_nothing_matches
    blob = Linguist::FileBlob.new(File.join(samples_path, "Objective-C/hello.m"))
    match = Language.detect(blob)
    assert_equal Language["Objective-C"], match
  end

  # Candidate languages = ["Perl", "Prolog"]
  def test_pl_prolog_perl_by_heuristics
    assert_heuristics({
      "Prolog" => "Prolog/turing.pl",
      "Perl" => "Perl/perl-test.t",
    })
  end

  # Candidate languages = ["ECL", "Prolog"]
  def test_ecl_prolog_by_heuristics
    results = Heuristics.call(file_blob("Prolog/or-constraint.ecl"), [Language["ECL"], Language["Prolog"]])
    assert_equal [Language["Prolog"]], results
  end

  # Candidate languages = ["ECL", "Prolog"]
  def test_ecl_prolog_by_heuristics
    assert_heuristics({
      "ECL" => "ECL/sample.ecl",
      "Prolog" => "Prolog/or-constraint.ecl"
    })
  end

  # Candidate languages = ["IDL", "Prolog"]
  def test_pro_prolog_idl_by_heuristics
    assert_heuristics({
      "Prolog" => "Prolog/logic-problem.pro",
      "IDL" => "IDL/mg_acosh.pro"
    })
  end

  # Candidate languages = ["AGS Script", "AsciiDoc"]
  def test_asc_asciidoc_by_heuristics
    assert_heuristics({
      "AsciiDoc" => "AsciiDoc/list.asc",
      "AGS Script" => nil
    })
  end

  def test_cl_by_heuristics
    assert_heuristics({
      "Common Lisp" => all_fixtures("Common Lisp"),
      "OpenCL" => all_fixtures("OpenCL")
    })
  end

  def test_f_by_heuristics
    assert_heuristics({
      "FORTRAN" => all_fixtures("FORTRAN"),
      "Forth" => all_fixtures("Forth")
    })
  end

  # Candidate languages = ["Hack", "PHP"]
  def test_hack_by_heuristics
    assert_heuristics({
      "Hack" => "Hack/funs.php",
      "PHP" => "PHP/Model.php"
    })
  end

  # Candidate languages = ["Scala", "SuperCollider"]
  def test_sc_supercollider_scala_by_heuristics
    assert_heuristics({
      "SuperCollider" => "SuperCollider/WarpPreset.sc",
      "Scala" => "Scala/node11.sc"
    })
  end

  def test_fs_by_heuristics
    assert_heuristics({
      "F#" => all_fixtures("F#"),
      "Forth" => all_fixtures("Forth"),
      "GLSL" => all_fixtures("GLSL")
    })
  end

  def assert_heuristics(hash)
    candidates = hash.keys.map { |l| Language[l] }

    hash.each do |language, blobs|
      Array(blobs).each do |blob|
        result = Heuristics.call(file_blob(blob), candidates)
        assert_equal [Language[language]], result
      end
    end
  end
end
