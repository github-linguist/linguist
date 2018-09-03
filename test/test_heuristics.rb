require_relative "./helper"

class TestHeuristics < Minitest::Test
  include Linguist

  def fixture(name)
    File.read(File.join(samples_path, name))
  end

  def file_blob(name)
    path = File.exist?(name) ? name : File.join(samples_path, name)
    FileBlob.new(path)
  end

  def all_fixtures(language_name, file="*")
    fixs = Dir.glob("#{samples_path}/#{language_name}/#{file}") -
             ["#{samples_path}/#{language_name}/filenames"]
    fixs.reject { |f| File.symlink?(f) }
  end

  def test_no_match
    language = []
    results = Heuristics.call(file_blob("JavaScript/namespace.js"), language)
    assert_equal [], results
  end

  def test_symlink_empty
    assert_equal [], Heuristics.call(file_blob("Markdown/symlink.md"), [Language["Markdown"]])
  end

  def assert_heuristics(hash)
    candidates = hash.keys.map { |l| Language[l] }

    hash.each do |language, blobs|
      Array(blobs).each do |blob|
        result = Heuristics.call(file_blob(blob), candidates)
        if language.nil?
          assert_equal [], result, "Failed for #{blob}"
        else
          assert_equal [Language[language]], result, "Failed for #{blob}"
        end
      end
    end
  end

  def test_detect_still_works_if_nothing_matches
    blob = Linguist::FileBlob.new(File.join(samples_path, "Objective-C/hello.m"))
    match = Linguist.detect(blob)
    assert_equal Language["Objective-C"], match
  end

  def test_1to9_by_heuristics
    assert_heuristics({
      "Roff" => all_fixtures("Roff", "*.{1..9}"),
      "Text" => all_fixtures("Text", "*.{1..9}")
    })
  end

  def test_as_by_heuristics
    assert_heuristics({
      "ActionScript" => all_fixtures("ActionScript", "*.as"),
      "AngelScript" => all_fixtures("AngelScript", "*.as")
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

  def test_bb_by_heuristics
    assert_heuristics({
      "BitBake" => all_fixtures("BitBake", "*.bb"),
      "BlitzBasic" => all_fixtures("BlitzBasic", "*.bb")
    })
  end

  def test_builds_by_heuristics
    assert_heuristics({
      "Text" => all_fixtures("Text", "*.builds"),
      "XML" => all_fixtures("XML", "*.builds")
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

  def test_cls_by_heuristics
    assert_heuristics({
      "TeX" => all_fixtures("TeX", "*.cls"),
      nil => all_fixtures("Apex", "*.cls"),
      nil => all_fixtures("OpenEdge ABL", "*.cls"),
      nil => all_fixtures("Visual Basic", "*.cls"),
    })
  end

  def test_cs_by_heuristics
    assert_heuristics({
      "C#" => all_fixtures("C#", "*.cs"),
      "Smalltalk" => all_fixtures("Smalltalk", "*.cs")
    })
  end

  def test_d_by_heuristics
    assert_heuristics({
      "D" => all_fixtures("D", "*.d"),
      "DTrace" => all_fixtures("DTrace", "*.d"),
      "Makefile" => all_fixtures("Makefile", "*.d"),
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
      "Fortran" => all_fixtures("Fortran", "*.f") + all_fixtures("Fortran", "*.for"),
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

  def test_gml_by_heuristics
      assert_heuristics({
        "Game Maker Language" => all_fixtures("Game Maker Language", "*.gml"),
        "Graph Modeling Language" => all_fixtures("Graph Modeling Language", "*.gml"),
        "XML" => all_fixtures("XML", "*.gml")
      })
  end

  def test_gs_by_heuristics
    assert_heuristics({
      "Gosu" => all_fixtures("Gosu", "*.gs")
    })
  end

  # Candidate languages = ["C++", "Objective-C"]
  def test_h_by_heuristics
    # Only calling out '.h' filenames as these are the ones causing issues
    assert_heuristics({
      "Objective-C" => all_fixtures("Objective-C", "*.h"),
      "C++" => ["C++/scanner.h", "C++/protocol-buffer.pb.h", "C++/v8.h", "C++/gdsdbreader.h"],
      "C" => nil
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

  def test_l_by_heuristics
    assert_heuristics({
      "Common Lisp" => all_fixtures("Common Lisp", "*.l"),
      "Lex" => all_fixtures("Lex", "*.l"),
      "Roff" => all_fixtures("Roff", "*.l"),
      "PicoLisp" => all_fixtures("PicoLisp", "*.l")
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

  def test_m_by_heuristics
    assert_heuristics({
      "Objective-C" => all_fixtures("Objective-C", "*.m") - all_fixtures("Objective-C", "cocoa_monitor.m"),
      "Mercury" => all_fixtures("Mercury", "*.m"),
      "MUF" => all_fixtures("MUF", "*.m"),
      "M" => all_fixtures("M", "MDB.m"),
      "Mathematica" => all_fixtures("Mathematica", "*.m") - all_fixtures("Mathematica", "Problem12.m"),
      "Matlab" => all_fixtures("Matlab", "create_ieee_paper_plots.m"),
      "Limbo" => all_fixtures("Limbo", "*.m"),
      nil => ["Objective-C/cocoa_monitor.m"]
    })
  end

  def test_md_by_heuristics
    assert_heuristics({
      "Markdown" => all_fixtures("Markdown", "*.md"),
      "GCC Machine Description" => all_fixtures("GCC Machine Description", "*.md")
    })
  end

  def test_ml_by_heuristics
    ambiguous = [
      "#{samples_path}/OCaml/date.ml",
      "#{samples_path}/OCaml/common.ml",
      "#{samples_path}/OCaml/sigset.ml",
    ]
    assert_heuristics({
      "OCaml" => all_fixtures("OCaml", "*.ml") - ambiguous,
      "Standard ML" => all_fixtures("Standard ML", "*.ml") - ambiguous,
      nil => ambiguous
    })
  end

  def test_mod_by_heuristics
    assert_heuristics({
      "Modula-2" => all_fixtures("Modula-2", "*.mod"),
      "XML" => all_fixtures("XML", "*.mod")
    })
  end

  def test_ms_by_heuristics
    assert_heuristics({
      "Roff" => all_fixtures("Roff", "*.ms"),
      "Unix Assembly" => all_fixtures("Unix Assembly", "*.ms"),
      "MAXScript" => all_fixtures("MAXScript", "*.ms")
    })
  end

  def test_n_by_heuristics
    assert_heuristics({
      "Roff" => all_fixtures("Roff", "*.n"),
      "Nemerle" => all_fixtures("Nemerle", "*.n")
    })
  end

  def test_ncl_by_heuristics
    ambiguous = [
      "#{samples_path}/Text/LIDARLite.ncl"
    ]
    assert_heuristics({
      "NCL" => all_fixtures("Roff", "*.ncl"),
      "XML" => all_fixtures("XML", "*.ncl"),
      "Text" => all_fixtures("Text", "*.ncl") - ambiguous,
      nil => ambiguous
    })
  end

  def test_nl_by_heuristics
    assert_heuristics({
      "NewLisp" => all_fixtures("NewLisp", "*.nl"),
      "NL" => all_fixtures("NL", "*.nl")
    })
  end

  # Candidate languages = ["Hack", "PHP"]
  def test_php_by_heuristics
    assert_heuristics({
      "Hack" => all_fixtures("Hack", "*.php"),
      "PHP" => all_fixtures("PHP", "*.php")
    })
  end

  # Candidate languages = ["Perl", "Perl 6", "Prolog"]
  def test_pl_prolog_perl_by_heuristics
    assert_heuristics({
      "Prolog" => all_fixtures("Prolog", "*.pl"),
      "Perl" => ["Perl/oo1.pl", "Perl/oo2.pl", "Perl/oo3.pl", "Perl/fib.pl", "Perl/use5.pl"],
      "Perl 6" => all_fixtures("Perl 6", "*.pl")
    })
  end

  # Candidate languages = ["Perl", "Perl 6", "XPM"]
  def test_pm_by_heuristics
    assert_heuristics({
      "Perl" => all_fixtures("Perl", "*.pm"),
      "Perl 6" => all_fixtures("Perl 6", "*.pm"),
      "XPM" => all_fixtures("XPM", "*.pm")
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

  def test_props_by_heuristics
    assert_heuristics({
      "INI" => all_fixtures("INI", "*.props"),
      "XML" => all_fixtures("XML", "*.props")
    })
  end

  def test_q_by_heuristics
    assert_heuristics({
      "q" => all_fixtures("q", "*.q"),
      "HiveQL" => all_fixtures("HiveQL", "*.q")
    })
  end

  def test_r_by_heuristics
    assert_heuristics({
      "R" => all_fixtures("R", "*.r") + all_fixtures("R", "*.R"),
      "Rebol" => all_fixtures("Rebol", "*.r")
    })
  end

  def test_rno_by_heuristics
    assert_heuristics({
      "RUNOFF" => all_fixtures("RUNOFF", "*.rno"),
      "Roff" => all_fixtures("Roff", "*.rno")
    })
  end

  def test_rpy_by_heuristics
    assert_heuristics({
      "Python" => all_fixtures("Python", "*.rpy"),
      "Ren'Py" => all_fixtures("Ren'Py", "*.rpy")
    })
  end

  def test_rs_by_heuristics
    assert_heuristics({
      "Rust" => all_fixtures("Rust", "*.rs"),
      "RenderScript" => all_fixtures("RenderScript", "*.rs")
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

  def test_srt_by_heuristics
    assert_heuristics({
      "SubRip Text" => all_fixtures("SubRip Text", "*.srt")
    })
  end

  # Candidate languages = ["Perl", "Perl 6", "Turing"]
  def test_t_perl_by_heuristics
    # Turing not fully covered.
    assert_heuristics({
      "Turing" => all_fixtures("Turing", "*.t"),
      "Perl" => all_fixtures("Perl", "*.t"),
      "Perl 6" => ["Perl 6/01-dash-uppercase-i.t", "Perl 6/01-parse.t", "Perl 6/advent2009-day16.t",
                   "Perl 6/basic-open.t", "Perl 6/calendar.t", "Perl 6/for.t", "Perl 6/hash.t",
                   "Perl 6/listquote-whitespace.t"]
    })
  end

  def test_toc_by_heuristics
    assert_heuristics({
      "TeX" => all_fixtures("TeX", "*.toc"),
      "World of Warcraft Addon Data" => all_fixtures("World of Warcraft Addon Data", "*.toc")
    })
  end

  def test_ts_by_heuristics
    assert_heuristics({
      "TypeScript" => all_fixtures("TypeScript", "*.ts"),
      "XML" => all_fixtures("XML", "*.ts")
    })
  end

  def test_tst_by_heuristics
    assert_heuristics({
      "GAP" => all_fixtures("GAP", "*.tst"),
      "Scilab" => all_fixtures("Scilab", "*.tst")
    })
  end

  def test_tsx_by_heuristics
    assert_heuristics({
      "TypeScript" => all_fixtures("TypeScript", "*.tsx"),
      "XML" => all_fixtures("XML", "*.tsx")
    })
  end

  def test_w_by_heuristics
    assert_heuristics({
      "CWeb" => all_fixtures("CWeb", "*.w"),
      "OpenEdge ABL" => all_fixtures("OpenEdge ABL", "*.w")
    })
  end

  def test_x_by_heuristics
    # Logos not fully covered
    assert_heuristics({
      "Linked Script" => all_fixtures("Linked Script", "*.x"),
      "RPC" => all_fixtures("RPC", "*.x")
    })
  end

  def test_yy_by_heuristics
    assert_heuristics({
      "JSON" => all_fixtures("JSON", "*.yy"),
      "Yacc" => all_fixtures("Yacc", "*.yy")
    })
  end
end
