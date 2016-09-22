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
    Dir.glob("#{samples_path}/#{language_name}/#{file}") -
      ["#{samples_path}/#{language_name}/filenames"]
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

  def test_detect_still_works_if_nothing_matches
    blob = Linguist::FileBlob.new(File.join(samples_path, "Objective-C/hello.m"))
    match = Linguist.detect(blob)
    assert_equal Language["Objective-C"], match
  end

  # Candidate languages = ["AGS Script", "AsciiDoc", "Public Key"]
  def test_asc_by_heuristics
    assert_heuristics({
      "AsciiDoc" => all_fixtures("AsciiDoc", "*.asc"),
      "AGS Script" => all_fixtures("AGS Script", "*.asc"),
      "Public Key" => all_fixtures("Public Key", "*.asc")
    })
  end

  def test_bb_by_heuristics
    assert_heuristics({
      "BitBake" => all_fixtures("BitBake", "*.bb"),
      "BlitzBasic" => all_fixtures("BlitzBasic", "*.bb")
    })
  end

  def test_ch_by_heuristics
    assert_heuristics({
      "xBase" => all_fixtures("xBase", ".ch")
    })
  end

  def test_cl_by_heuristics
    assert_heuristics({
      "Common Lisp" => all_fixtures("Common Lisp", "*.cl"),
      "OpenCL" => all_fixtures("OpenCL", "*.cl")
    })
  end

  def test_cs_by_heuristics
    assert_heuristics({
      "C#" => all_fixtures("C#", "*.cs"),
      "Smalltalk" => all_fixtures("Smalltalk", "*.cs")
    })
  end

  # Candidate languages = ["ECL", "ECLiPSe"]
  def test_ecl_by_heuristics
    assert_heuristics({
      "ECL" => all_fixtures("ECL", "*.ecl"),
      "ECLiPSe" => all_fixtures("ECLiPSe", "*.ecl")
    })
  end
  
  def test_es_by_heuristics
    assert_heuristics({
      "Erlang" => all_fixtures("Erlang", "*.es"),
      "JavaScript" => all_fixtures("JavaScript", "*.es")
    })
  end

  def test_f_by_heuristics
    assert_heuristics({
      "FORTRAN" => all_fixtures("FORTRAN", "*.f") + all_fixtures("FORTRAN", "*.for"),
      "Forth" => all_fixtures("Forth", "*.f") + all_fixtures("Forth", "*.for")
    })
  end

  def test_fr_by_heuristics
    assert_heuristics({
      "Frege" => all_fixtures("Frege", "*.fr"),
      "Forth" => all_fixtures("Forth", "*.fr"),
      "Text" => all_fixtures("Text", "*.fr")
    })
  end

  def test_fs_by_heuristics
    assert_heuristics({
      "F#" => all_fixtures("F#", "*.fs"),
      "Forth" => all_fixtures("Forth", "*.fs"),
      "GLSL" => all_fixtures("GLSL", "*.fs")
    })
  end

  # Candidate languages = ["Hack", "PHP"]
  def test_hack_by_heuristics
    assert_heuristics({
      "Hack" => all_fixtures("Hack", "*.php"),
      "PHP" => all_fixtures("PHP", "*.php")
    })
  end

  # Candidate languages = ["Assembly", "C++", "HTML", "PAWN", "PHP",
  #                        "POV-Ray SDL", "Pascal", "SQL", "SourcePawn"]
  def test_inc_by_heuristics
    assert_heuristics({
      "PHP" => all_fixtures("PHP", "*.inc"),
      "POV-Ray SDL" => all_fixtures("POV-Ray SDL", "*.inc")
    })
  end

  def test_ls_by_heuristics
    assert_heuristics({
      "LiveScript" => all_fixtures("LiveScript", "*.ls"),
      "LoomScript" => all_fixtures("LoomScript", "*.ls")
    })
  end

  def test_lsp_by_heuristics
    assert_heuristics({
      "Common Lisp" => all_fixtures("Common Lisp", "*.lsp") + all_fixtures("Common Lisp", "*.lisp"),
      "NewLisp" => all_fixtures("NewLisp", "*.lsp") + all_fixtures("NewLisp", "*.lisp")
    })
  end

  # Candidate languages = ["C++", "Objective-C"]
  def test_obj_c_by_heuristics
    # Only calling out '.h' filenames as these are the ones causing issues
    assert_heuristics({
      "Objective-C" => all_fixtures("Objective-C", "*.h"),
      "C++" => ["C++/scanner.h", "C++/protocol-buffer.pb.h", "C++/v8.h", "C++/gdsdbreader.h"],
      "C" => nil
    })
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

  # Candidate languages = ["Pod", "Perl"]
  def test_pod_by_heuristics
    assert_heuristics({
      "Perl" => all_fixtures("Perl", "*.pod"),
      "Pod" => all_fixtures("Pod", "*.pod")
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

  def test_r_by_heuristics
    assert_heuristics({
      "R" => all_fixtures("R", "*.r") + all_fixtures("R", "*.R"),
      "Rebol" => all_fixtures("Rebol", "*.r")
    })
  end

  # Candidate languages = ["Scala", "SuperCollider"]
  def test_sc_supercollider_scala_by_heuristics
    assert_heuristics({
      "SuperCollider" => all_fixtures("SuperCollider", "*.sc"),
      "Scala" => all_fixtures("Scala", "*.sc")
    })
  end

  # Candidate languages = ["SQL", "PLpgSQL", "SQLPL", "PLSQL"]
  def test_sql_by_heuristics
    assert_heuristics({
      "SQL" => ["SQL/create_stuff.sql", "SQL/db.sql", "SQL/dual.sql"],
      "PLpgSQL" => all_fixtures("PLpgSQL", "*.sql"),
      "SQLPL" => ["SQLPL/trigger.sql"],
      "PLSQL" => all_fixtures("PLSQL", "*.sql")
    })
  end

  # Candidate languages = ["Perl", "Perl6"]
  def test_t_perl_by_heuristics
    assert_heuristics({
      "Perl" => all_fixtures("Perl", "*.t"),
      "Perl6" => ["Perl6/01-dash-uppercase-i.t", "Perl6/01-parse.t", "Perl6/advent2009-day16.t",
                  "Perl6/basic-open.t", "Perl6/calendar.t", "Perl6/for.t", "Perl6/hash.t",
                  "Perl6/listquote-whitespace.t"]
    })
  end

  def test_ts_by_heuristics
    assert_heuristics({
      "TypeScript" => all_fixtures("TypeScript", "*.ts"),
      "XML" => all_fixtures("XML", "*.ts")
    })
  end
end
