require_relative "./helper"

class TestHeuristcs < Minitest::Test
  include Linguist

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

  def test_no_match
    language = []
    results = Heuristics.call(file_blob("JavaScript/namespace.js"), language)
    assert_equal [], results
  end

  # Candidate languages = ["C++", "Objective-C"]
  def test_obj_c_by_heuristics
    # Only calling out '.h' filenames as these are the ones causing issues
    assert_heuristics({
      "Objective-C" => all_fixtures("Objective-C", "*.h"),
      "C++" => ["C++/render_adapter.cpp", "C++/ThreadedQueue.h"],
      "C" => nil
    })
  end

  def test_c_by_heuristics
    languages = [Language["C++"], Language["Objective-C"], Language["C"]]
    results = Heuristics.call(file_blob("C/ArrowLeft.h"), languages)
    assert_equal [], results
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
      "Perl" => ["Perl/perl-test.t", "Perl/use5.pl"]
    })
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

  def test_fr_by_heuristics
    assert_heuristics({
      "Frege" => all_fixtures("Frege"),
      "Forth" => all_fixtures("Forth"),
      "Text" => all_fixtures("Text")
    })
  end

  def test_bb_by_heuristics
    assert_heuristics({
      "BitBake" => all_fixtures("BitBake"),
      "BlitzBasic" => all_fixtures("BlitzBasic")
    })
  end

  def test_lsp_by_heuristics
    assert_heuristics({
      "Common Lisp" => all_fixtures("Common Lisp"),
      "NewLisp" => all_fixtures("NewLisp")
    })
  end

  def test_cs_by_heuristics
    assert_heuristics({
      "C#" => all_fixtures("C#", "*.cs"),
      "Smalltalk" => all_fixtures("Smalltalk", "*.cs")
    })
  end

  def assert_heuristics(hash)
    candidates = hash.keys.map { |l| Language[l] }

    hash.each do |language, blobs|
      Array(blobs).each do |blob|
        result = Heuristics.call(file_blob(blob), candidates)
        assert_equal [Language[language]], result, "Failed for #{blob}"
      end
    end
  end

  def test_ls_by_heuristics
    assert_heuristics({
      "LiveScript" => "LiveScript/hello.ls",
      "LoomScript" => "LoomScript/HelloWorld.ls"
    })
  end

  def test_ts_by_heuristics
    assert_heuristics({
      "TypeScript" => all_fixtures("TypeScript", "*.ts"),
      "XML" => all_fixtures("XML", "*.ts")
    })
  end
end
