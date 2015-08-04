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

  def assert_heuristics(hash)
    candidates = hash.keys.map { |l| Language[l] }

    hash.each do |language, blobs|
      Array(blobs).each do |blob|
        result = Heuristics.call(file_blob(blob), candidates)
        assert_equal [Language[language]], result, "Failed for #{blob}"
      end
    end
  end

  # Candidate languages = ["C++", "Objective-C"]
  def test_obj_c_by_heuristics
    # Only calling out '.h' filenames as these are the ones causing issues
    assert_heuristics({
      "Objective-C" => all_fixtures("Objective-C", "*.h"),
      "C++" => ["C++/scanner.h", "C++/qscicommand.h", "C++/v8.h", "C++/gdsdbreader.h"],
      "C" => nil
    })
  end

  def test_detect_still_works_if_nothing_matches
    blob = Linguist::FileBlob.new(File.join(samples_path, "Objective-C/hello.m"))
    match = Language.detect(blob)
    assert_equal Language["Objective-C"], match
  end

  # Candidate languages = ["Perl", "Perl6", "Prolog"]
  def test_pl_prolog_perl_by_heuristics
    assert_heuristics({
      "Prolog" => all_fixtures("Prolog", "*.pl"),
      "Perl" => ["Perl/oo1.pl", "Perl/oo2.pl", "Perl/oo3.pl", "Perl/fib.pl", "Perl/use5.pl"],
      "Perl6" => all_fixtures("Perl6", "*.pl")
    })
  end

  # Candidate languages = ["Perl", "Perl6"]
  def test_pm_perl_by_heuristics
    assert_heuristics({
      "Perl" => all_fixtures("Perl", "*.pm"),
      "Perl6" => all_fixtures("Perl6", "*.pm")
    })
  end

  # Candidate languages = ["ECL", "Prolog"]
  def test_ecl_prolog_by_heuristics
    assert_heuristics({
      "ECL" => all_fixtures("ECL", "*.ecl"),
      "Prolog" => all_fixtures("Prolog", "*.ecl")
    })
  end

  # Candidate languages = ["IDL", "Prolog", "QMake", "INI"]
  def test_pro_by_heuristics
    assert_heuristics({
      "Prolog" => all_fixtures("Prolog", "*.pro"),
      "IDL" => all_fixtures("IDL", "*.pro"),
      "INI" => all_fixtures("INI", "*.pro"),
      "QMake" => all_fixtures("QMake", "*.pro")
    })
  end

  # Candidate languages = ["AGS Script", "AsciiDoc", "Public Key"]
  def test_asc_by_heuristics
    assert_heuristics({
      "AsciiDoc" => all_fixtures("AsciiDoc", "*.asc"),
      "AGS Script" => all_fixtures("AGS Script", "*.asc"),
      "Public Key" => all_fixtures("Public Key", "*.asc")
    })
  end

  def test_cl_by_heuristics
    assert_heuristics({
      "Common Lisp" => all_fixtures("Common Lisp", "*.cl"),
      "OpenCL" => all_fixtures("OpenCL", "*.cl")
    })
  end

  def test_f_by_heuristics
    assert_heuristics({
      "FORTRAN" => all_fixtures("FORTRAN", "*.f") + all_fixtures("FORTRAN", "*.for"),
      "Forth" => all_fixtures("Forth", "*.f") + all_fixtures("Forth", "*.for")
    })
  end

  # Candidate languages = ["Hack", "PHP"]
  def test_hack_by_heuristics
    assert_heuristics({
      "Hack" => all_fixtures("Hack", "*.php"),
      "PHP" => all_fixtures("PHP", "*.php")
    })
  end

  # Candidate languages = ["Scala", "SuperCollider"]
  def test_sc_supercollider_scala_by_heuristics
    assert_heuristics({
      "SuperCollider" => all_fixtures("SuperCollider", "*.sc"),
      "Scala" => all_fixtures("Scala", "*.sc")
    })
  end

  def test_fs_by_heuristics
    assert_heuristics({
      "F#" => all_fixtures("F#", "*.fs"),
      "Forth" => all_fixtures("Forth", "*.fs"),
      "GLSL" => all_fixtures("GLSL", "*.fs")
    })
  end

  def test_fr_by_heuristics
    assert_heuristics({
      "Frege" => all_fixtures("Frege", "*.fr"),
      "Forth" => all_fixtures("Forth", "*.fr"),
      "Text" => all_fixtures("Text", "*.fr")
    })
  end

  def test_bb_by_heuristics
    assert_heuristics({
      "BitBake" => all_fixtures("BitBake", "*.bb"),
      "BlitzBasic" => all_fixtures("BlitzBasic", "*.bb")
    })
  end

  def test_lsp_by_heuristics
    assert_heuristics({
      "Common Lisp" => all_fixtures("Common Lisp", "*.lsp") + all_fixtures("Common Lisp", "*.lisp"),
      "NewLisp" => all_fixtures("NewLisp", "*.lsp") + all_fixtures("NewLisp", "*.lisp")
    })
  end

  def test_cs_by_heuristics
    assert_heuristics({
      "C#" => all_fixtures("C#", "*.cs"),
      "Smalltalk" => all_fixtures("Smalltalk", "*.cs")
    })
  end

  def test_ls_by_heuristics
    assert_heuristics({
      "LiveScript" => all_fixtures("LiveScript", "*.ls"),
      "LoomScript" => all_fixtures("LoomScript", "*.ls")
    })
  end

  def test_ts_by_heuristics
    assert_heuristics({
      "TypeScript" => all_fixtures("TypeScript", "*.ts"),
      "XML" => all_fixtures("XML", "*.ts")
    })
  end

  def test_ch_by_heuristics
    assert_heuristics({
      "xBase" => all_fixtures("xBase", ".ch")
    })
  end
end
