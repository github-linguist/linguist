require_relative "./helper"

class TestHeuristics < Minitest::Test
  include Linguist

  def fixture(name)
    File.read(File.join(samples_path, name))
  end

  def file_blob(name, alt_name=nil)
    path = File.exist?(name) ? name : File.join(samples_path, name)
    blob = FileBlob.new(path)
    if !alt_name.nil?
      blob.instance_variable_set("@path", alt_name)
    end
    blob
  end

  def all_fixtures(language_name, file="*")
    fixs = Dir.glob("#{samples_path}/#{language_name}/#{file}") -
             ["#{samples_path}/#{language_name}/filenames"]
    fixs = fixs.reject { |f| File.symlink?(f) }
    assert !fixs.empty?, "no fixtures for #{language_name} #{file}"
    fixs
  end

  def test_no_match
    language = []
    results = Heuristics.call(file_blob("JavaScript/namespace.js"), language)
    assert_equal [], results
  end

  def test_symlink_empty
    assert_equal [], Heuristics.call(file_blob("Markdown/symlink.md"), [Language["Markdown"]])
  end

  # alt_name is a file name that will be used instead of the file name of the
  # original sample. This is used to force a sample to go through a specific
  # heuristic even if its extension doesn't match.
  def assert_heuristics(hash, alt_name=nil)
    candidates = hash.keys.map { |l| Language[l] }

    hash.each do |language, blobs|
      blobs = Array(blobs)
      assert blobs.length >= 1, "Expected at least 1 blob for #{language}"
      blobs.each do |blob|
        result = Heuristics.call(file_blob(blob, alt_name), candidates)
        if language.nil?
          expected = []
        elsif language.is_a?(Array)
          expected = language.map{ |l| Language[l] }
        else
          expected = [Language[language]]
        end
        assert_equal expected, result, "Failed for #{blob}"
      end
    end
  end

  def test_detect_still_works_if_nothing_matches
    blob = Linguist::FileBlob.new(File.join(samples_path, "Objective-C/hello.m"))
    match = Linguist.detect(blob)
    assert_equal Language["Objective-C"], match
  end

  def test_all_extensions_are_listed
    Heuristics.all.all? do |rule|
      rule.languages.each do |lang|
        unlisted = rule.extensions.reject { |ext| lang.extensions.include? ext }
        assert_equal [], unlisted, (<<~EOF).chomp
          The extension '#{unlisted.first}' is not assigned to #{lang.name}.
          Add it to `languages.yml` or update the heuristic which uses it
        EOF
      end
    end
  end

  def test_al_by_heuristics
    assert_heuristics({
      "AL" => all_fixtures("AL", "*.al"),
      "Perl" => all_fixtures("Perl", "*.al")
    })
  end
  
  def test_as_by_heuristics
    assert_heuristics({
      "ActionScript" => all_fixtures("ActionScript", "*.as"),
      nil => all_fixtures("AngelScript", "*.as")
    })
  end

  def test_asc_by_heuristics
    assert_heuristics({
      "AsciiDoc" => all_fixtures("AsciiDoc"),
      "AGS Script" => all_fixtures("AGS Script"),
      "Public Key" => all_fixtures("Public Key")
    }, "test.asc")
  end

  def test_asm_by_heuristics
    assert_heuristics({
      "Motorola 68K Assembly" => all_fixtures("Motorola 68K Assembly", "*.asm"),
      # Assembly lacks a heuristic
      nil => all_fixtures("Assembly", "*.asm")
    })
  end

  def test_asy_by_heuristics
    assert_heuristics({
      "Asymptote" => all_fixtures("Asymptote", "*.asy"),
      "LTspice Symbol" => all_fixtures("LTspice Symbol", "*.asy")
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
      nil => all_fixtures("Text"),
      "XML" => all_fixtures("XML", "*.builds")
    }, "test.builds")
  end

  def test_ch_by_heuristics
    assert_heuristics({
      "xBase" => all_fixtures("xBase", "*.ch"),
      # Missing heuristic for Charity
      nil => all_fixtures("Charity", "*.ch")
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
      "ObjectScript" => all_fixtures("ObjectScript", "*.cls"),
      # Missing heuristics
      nil => all_fixtures("Apex", "*.cls") + all_fixtures("OpenEdge ABL", "*.cls") + all_fixtures("VBA", "*.cls"),
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
      "D" => all_fixtures("D"),
      "DTrace" => all_fixtures("DTrace"),
      "Makefile" => all_fixtures("Makefile"),
    }, "test.d")
  end

  def test_dsp_heuristics
    assert_heuristics({
      "Faust" => all_fixtures("Faust", "*.dsp"),
      "Microsoft Developer Studio Project" => all_fixtures("Microsoft Developer Studio Project"),
    })
  end

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

  def test_gd_by_heuristics
    assert_heuristics({
      "GAP" => all_fixtures("GAP", "*.gd"),
      "GDScript" => all_fixtures("GDScript", "*.gd")
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
    ambiguous = [
      "#{samples_path}/Genie/Class.gs",
      "#{samples_path}/Genie/Hello.gs",
    ]
    assert_heuristics({
      "GLSL" => all_fixtures("GLSL", "*.gs"),
      "Genie" => all_fixtures("Genie", "*.gs") - ambiguous,
      "Gosu" => all_fixtures("Gosu", "*.gs"),
    })
    assert_heuristics({
      nil => all_fixtures("JavaScript")
    }, alt_name="test.gs")
  end

  # Candidate languages = ["C++", "Objective-C"]
  def test_h_by_heuristics
    ambiguous = [
        "#{samples_path}/C++/Field.h",
        "#{samples_path}/C++/rpc.h",
    ]
    assert_heuristics({
      "Objective-C" => all_fixtures("Objective-C", "*.h"),
      "C++" => all_fixtures("C++", "*.h") - ambiguous,
      # no heuristic for C
      nil => all_fixtures("C", "*.h")
    })
  end

  # Candidate languages = ["C++", "Hack"]
  def test_hh_by_heuristics
    assert_heuristics({
      "Hack" => all_fixtures("Hack", "*.hh"),
    })
  end

  def test_i_by_heuristics
    assert_heuristics({
      "Motorola 68K Assembly" => all_fixtures("Motorola 68K Assembly", "*.i"),
      "SWIG" => all_fixtures("SWIG", "*.i"),
      # No heuristic defined for Assembly
      nil => all_fixtures("Assembly", "*.i")
    })
  end

  def test_ice_by_heuristics
    assert_heuristics({
      "Slice" => all_fixtures("Slice", "*.ice"),
      "JSON" => all_fixtures("JSON", "*.ice")
    })
  end

  # Candidate languages = ["Assembly", "C++", "HTML", "Motorola 68K Assembly", "PAWN",
  #                        "PHP", "POV-Ray SDL", "Pascal", "SQL", "SourcePawn"]
  def test_inc_by_heuristics
    assert_heuristics({
      "Motorola 68K Assembly" => all_fixtures("Motorola 68K Assembly", "*.inc"),
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
    ambiguous = all_fixtures("Objective-C", "cocoa_monitor.m")
    assert_heuristics({
      "Objective-C" => all_fixtures("Objective-C", "*.m") - ambiguous,
      "Mercury" => all_fixtures("Mercury", "*.m"),
      "MUF" => all_fixtures("MUF", "*.m"),
      "M" => all_fixtures("M", "MDB.m"),
      "Mathematica" => all_fixtures("Mathematica", "*.m") - all_fixtures("Mathematica", "Problem12.m"),
      "MATLAB" => all_fixtures("MATLAB", "create_ieee_paper_plots.m"),
      "Limbo" => all_fixtures("Limbo", "*.m"),
      nil => ambiguous
    })
  end

  def test_mask_by_heuristics
    assert_heuristics({
      "Unity3D Asset" => all_fixtures("Unity3D Asset", "*.mask")
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
      "#{samples_path}/Standard ML/Foo.sig",
    ]
    assert_heuristics({
      "OCaml" => all_fixtures("OCaml") - ambiguous,
      "Standard ML" => all_fixtures("Standard ML") - ambiguous,
      nil => ambiguous
    }, "test.ml")
  end

  def test_mod_by_heuristics
    assert_heuristics({
      "Modula-2" => all_fixtures("Modula-2", "*.mod"),
      "XML" => all_fixtures("XML", "*.mod"),
      ["Linux Kernel Module", "AMPL"] => all_fixtures("Linux Kernel Module", "*.mod"),
      ["Linux Kernel Module", "AMPL"] => all_fixtures("AMPL", "*.mod"),
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
      "XML" => all_fixtures("XML", "*.ncl") - ambiguous,
      "Text" => all_fixtures("Text", "*.ncl") - ambiguous,
      # Missing heuristic for NCL
      nil => all_fixtures("NCL", "*.ncl") + ambiguous
    })
  end

  def test_nl_by_heuristics
    assert_heuristics({
      "NewLisp" => all_fixtures("NewLisp", "*.nl"),
      "NL" => all_fixtures("NL", "*.nl")
    })
  end

  def test_numbers_by_heuristics
    1.upto(9) do |n|
      assert_heuristics({
        "Roff Manpage" => Dir.glob("#{fixtures_path}/Generic/#{n}/Roff Manpage/*"),
        "Roff" => Dir.glob("#{fixtures_path}/Generic/#{n}/Roff/*"),
        nil => Dir.glob("#{fixtures_path}/Generic/#{n}/nil/*")
      })
    end
  end

  def test_p_by_heuristics
    assert_heuristics({
      "Gnuplot" => all_fixtures("Gnuplot"),
      "OpenEdge ABL" => all_fixtures("OpenEdge ABL")
    }, alt_name="test.p")
  end

  # Candidate languages = ["Hack", "PHP"]
  def test_php_by_heuristics
    assert_heuristics({
      "Hack" => all_fixtures("Hack", "*.php"),
      "PHP" => all_fixtures("PHP", "*.php")
    })
  end

  # Candidate languages = ["Perl", "Raku", "Prolog"]
  def test_pl_prolog_perl_by_heuristics
    assert_heuristics({
      "Prolog" => all_fixtures("Prolog", "*.pl"),
      "Perl" => ["Perl/oo1.pl", "Perl/oo2.pl", "Perl/oo3.pl", "Perl/fib.pl", "Perl/use5.pl"],
      "Raku" => all_fixtures("Raku", "*.pl")
    })
  end

  def test_plist_by_heuristics
    assert_heuristics({
      "OpenStep Property List" => all_fixtures("OpenStep Property List", "*.plist"),
      "XML Property List" => all_fixtures("XML Property List", "*.plist")
    })
  end

  # Candidate languages = ["Perl", "Raku", "XPM"]
  def test_pm_by_heuristics
    assert_heuristics({
      "Perl" => all_fixtures("Perl", "*.pm"),
      "Raku" => all_fixtures("Raku", "*.pm"),
      "X PixMap" => all_fixtures("X PixMap")
    }, "test.pm")
  end

  # Candidate languages = ["Pascal", "Puppet"]
  def test_pp_by_heuristics
    assert_heuristics({
      "Pascal" => all_fixtures("Pascal", "*.pp"),
      "Puppet" => all_fixtures("Puppet", "*.pp") - ["#{samples_path}/Puppet/stages-example.pp", "#{samples_path}/Puppet/hiera_include.pp"]
    })
  end

  # Candidate languages = ["IDL", "Proguard", "Prolog", "QMake", "INI"]
  def test_pro_by_heuristics
    assert_heuristics({
      "Proguard" => all_fixtures("Proguard", "*.pro"),
      "Prolog" => all_fixtures("Prolog", "*.pro"),
      "IDL" => all_fixtures("IDL", "*.pro"),
      "INI" => all_fixtures("INI", "*.pro"),
      "QMake" => all_fixtures("QMake", "*.pro")
    })
  end

  # Candidate languages = ["INI", "Java Properties"]
  def test_properties_by_heuristics
    assert_heuristics({
      "INI" => all_fixtures("INI", "*.properties"),
      "Java Properties" => all_fixtures("Java Properties", "*.properties")
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

  def test_re_by_heuristics
    assert_heuristics({
      "C++" => all_fixtures("C++", "*.re"),
      "Reason" => all_fixtures("Reason", "*.re")
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

  def test_s_by_heuristics
    assert_heuristics({
      "Motorola 68K Assembly" => all_fixtures("Motorola 68K Assembly", "*.s"),
      # Unix Assembly lacks a heuristic
      nil => all_fixtures("Unix Assembly", "*.s")
    })
  end

  # Candidate languages = ["Scala", "SuperCollider"]
  def test_sc_supercollider_scala_by_heuristics
    assert_heuristics({
      "SuperCollider" => all_fixtures("SuperCollider", "*.sc"),
      "Scala" => all_fixtures("Scala", "*.sc")
    })
  end

  def test_sol_by_heuristics
    assert_heuristics({
      "Gerber Image" => Dir.glob("#{fixtures_path}/Generic/sol/Gerber Image/*"),
      "Solidity" => Dir.glob("#{fixtures_path}/Generic/sol/Solidity/*"),
      nil => Dir.glob("#{fixtures_path}/Generic/sol/nil/*")
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

  # Candidate languages = ["Perl", "Raku", "Turing"]
  def test_t_perl_by_heuristics
    # Turing not fully covered.
    assert_heuristics({
      "Turing" => all_fixtures("Turing", "*.t"),
      "Perl" => all_fixtures("Perl", "*.t"),
      "Raku" => ["Raku/01-dash-uppercase-i.t", "Raku/01-parse.t", "Raku/advent2009-day16.t",
                   "Raku/basic-open.t", "Raku/calendar.t", "Raku/for.t", "Raku/hash.t",
                   "Raku/listquote-whitespace.t"]
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
      "TSX" => all_fixtures("TSX", "*.tsx"),
      "XML" => all_fixtures("XML", "*.tsx")
    })
  end

  def test_v_by_heuristics
    assert_heuristics({
      "Coq" => all_fixtures("Coq", "*.v"),
      "V" => all_fixtures("V", "*.v"),
      "Verilog" => all_fixtures("Verilog", "*.v")
    })
  end

  def test_vba_by_heuristics
    assert_heuristics({
      "VBA" => all_fixtures("VBA", "*.vba"),
      "Vim script" => all_fixtures("Vim script", "*.vba")
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
      "DirectX 3D File" => all_fixtures("DirectX 3D File", "*.x"),
      "Linker Script" => all_fixtures("Linker Script", "*.x"),
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
