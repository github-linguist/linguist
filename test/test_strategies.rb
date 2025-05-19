require_relative "./helper"

class TestStrategies < Minitest::Test
  include Linguist

  def assert_modeline(language, blob)
    if language.nil?
      assert_nil Linguist::Strategy::Modeline.call(blob).first
    else
      assert_equal language, Linguist::Strategy::Modeline.call(blob).first
    end
  end

  def assert_interpreter(interpreter, body)
    if interpreter.nil?
      assert_nil Shebang.interpreter(body)
    else
      assert_equal interpreter, Shebang.interpreter(body)
    end
  end

  def file_blob(name)
    path = File.exist?(name) ? name : File.join(samples_path, name)
    FileBlob.new(path)
  end

  def all_xml_fixtures(file="*")
    fixs = Dir.glob("#{samples_path}/XML/#{file}") -
             ["#{samples_path}/XML/demo.hzp"] -
             ["#{samples_path}/XML/psd-data.xmp"] -
             ["#{samples_path}/XML/filenames"]
    fixs.reject { |f| File.symlink?(f) }
  end

  def assert_manpage(blob)
    languages = Linguist::Strategy::Manpage.call(blob)
    assert_equal Language["Roff Manpage"], languages[0], "#{blob} not detected as manpage"
    assert_equal Language["Roff"], languages[1], "#{blob} should include Roff as candidate language"
  end

  def assert_xml(blob)
    language = Linguist::Strategy::XML.call(file_blob(blob)).first
    assert_equal Language["XML"], language, "#{blob} not detected as XML"
  end

  def assert_all_xml(blobs)
    Array(blobs).each do |blob|
      assert_xml blob
    end
  end

  def test_manpage_strategy
    assert_manpage fixture_blob("Data/Manpages/bsdmalloc.3malloc")
    assert_manpage fixture_blob("Data/Manpages/dirent.h.0p")
    assert_manpage fixture_blob("Data/Manpages/linguist.1gh")
    assert_manpage fixture_blob("Data/Manpages/test.1.in")
    assert_manpage fixture_blob("Data/Manpages/test.2.in")
    assert_manpage fixture_blob("Data/Manpages/test.3.in")
    assert_manpage fixture_blob("Data/Manpages/test.4.in")
    assert_manpage fixture_blob("Data/Manpages/test.5.in")
    assert_manpage fixture_blob("Data/Manpages/test.6.in")
    assert_manpage fixture_blob("Data/Manpages/test.7.in")
    assert_manpage fixture_blob("Data/Manpages/test.8.in")
    assert_manpage fixture_blob("Data/Manpages/test.9.in")
    assert_manpage fixture_blob("Data/Manpages/test.man.in")
    assert_manpage fixture_blob("Data/Manpages/test.mdoc.in")
  end

  def test_modeline_strategy
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby")
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby2")
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby3")
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby4")
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby5")
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby6")
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby7")
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby8")
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby9")
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby10")
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby11")
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby12")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplus")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs1")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs2")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs3")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs4")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs5")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs6")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs7")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs8")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs9")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs10")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs11")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs12")
    assert_modeline Language["Text"], fixture_blob("Data/Modelines/fundamentalEmacs.c")
    assert_modeline Language["Prolog"], fixture_blob("Data/Modelines/not_perl.pl")
    assert_modeline Language["Smalltalk"], fixture_blob("Data/Modelines/example_smalltalk.md")
    assert_modeline Language["JavaScript"], fixture_blob("Data/Modelines/iamjs.pl")
    assert_modeline Language["JavaScript"], fixture_blob("Data/Modelines/iamjs2.pl")
    assert_modeline Language["PHP"], fixture_blob("Data/Modelines/iamphp.inc")
    assert_modeline nil, sample_blob("C++/runtime-compiler.cc")
  end

  def test_modeline_languages
    assert_equal Language["Ruby"], fixture_blob("Data/Modelines/ruby").language
    assert_equal Language["Ruby"], fixture_blob("Data/Modelines/ruby2").language
    assert_equal Language["Ruby"], fixture_blob("Data/Modelines/ruby3").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplus").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs1").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs2").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs3").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs4").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs5").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs6").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs7").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs8").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs9").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs10").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs11").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplusEmacs12").language
    assert_equal Language["Text"], fixture_blob("Data/Modelines/fundamentalEmacs.c").language
    assert_equal Language["Prolog"], fixture_blob("Data/Modelines/not_perl.pl").language
    assert_equal Language["Smalltalk"], fixture_blob("Data/Modelines/example_smalltalk.md").language
    assert_equal Language["JavaScript"], fixture_blob("Data/Modelines/iamjs.pl").language
    assert_equal Language["JavaScript"], fixture_blob("Data/Modelines/iamjs2.pl").language
    assert_equal Language["PHP"], fixture_blob("Data/Modelines/iamphp.inc").language
  end

  def test_shebangs
    assert_interpreter nil, ""
    assert_interpreter nil, "foo"
    assert_interpreter nil, "#bar"
    assert_interpreter nil, "#baz"
    assert_interpreter nil, "///"
    assert_interpreter nil, "\n\n\n\n\n"
    assert_interpreter nil, " #!/usr/sbin/ruby"
    assert_interpreter nil, "\n#!/usr/sbin/ruby"
    assert_interpreter nil, "#!"
    assert_interpreter nil, "#! "
    assert_interpreter nil, "#!/usr/bin/env"
    assert_interpreter nil, "#!/usr/bin/env osascript -l JavaScript"
    assert_interpreter nil, "#!/usr/bin/env osascript -l AppleScript"
    assert_interpreter nil, "#!/usr/bin/env osascript -l foobar"
    assert_interpreter nil, "#!/usr/bin/osascript -l JavaScript"
    assert_interpreter nil, "#!/usr/bin/osascript -l foobar"

    assert_interpreter "ruby", "#!/usr/sbin/ruby\n# bar"
    assert_interpreter "ruby", "#!/usr/bin/ruby\n# foo"
    assert_interpreter "ruby", "#!/usr/sbin/ruby"
    assert_interpreter "ruby", "#!/usr/sbin/ruby foo bar baz\n"

    assert_interpreter "Rscript", "#!/usr/bin/env Rscript\n# example R script\n#\n"
    assert_interpreter "crystal", "#!/usr/bin/env bin/crystal"
    assert_interpreter "ruby", "#!/usr/bin/env ruby\n# baz"

    assert_interpreter "bash", "#!/usr/bin/bash\n"
    assert_interpreter "sh", "#!/bin/sh"
    assert_interpreter "python", "#!/bin/python\n# foo\n# bar\n# baz"
    assert_interpreter "python2", "#!/usr/bin/python2.7\n\n\n\n"
    assert_interpreter "python3", "#!/usr/bin/python3\n\n\n\n"
    assert_interpreter "sbcl", "#!/usr/bin/sbcl --script\n\n"
    assert_interpreter "perl", "#! perl"

    assert_interpreter "ruby", "#!/bin/sh\n\n\nexec ruby $0 $@"

    assert_interpreter "sh", "#! /usr/bin/env A=003 B=149 C=150 D=xzd E=base64 F=tar G=gz H=head I=tail sh"
    assert_interpreter "python", "#!/usr/bin/env foo=bar bar=foo python -cos=__import__(\"os\");"
    assert_interpreter "osascript", "#!/usr/bin/env osascript"
    assert_interpreter "osascript", "#!/usr/bin/osascript"

    assert_interpreter "ruby", "#!/usr/bin/env -vS ruby -wKU\nputs ?t+?e+?s+?t"
    assert_interpreter "sed", "#!/usr/bin/env --split-string sed -f\ny/a/A/"
    assert_interpreter "deno", "#!/usr/bin/env -S GH_TOKEN=ghp_*** deno run --allow-net\nconsole.log(1);"
  end

  def test_xml
    no_root_tag = [
      "#{samples_path}/XML/libsomething.dll.config",
      "#{samples_path}/XML/real-estate.mjml",
      "#{samples_path}/XML/XmlIO.pluginspec",
      "#{samples_path}/XML/MainView.ux",
      "#{samples_path}/XML/MyApp.ux",
      "#{samples_path}/XML/xhtml-struct-1.mod",
      "#{samples_path}/XML/wixdemo.wixproj",
      "#{samples_path}/XML/msbuild-example.proj",
      "#{samples_path}/XML/sample.targets",
      "#{samples_path}/XML/Default.props",
      "#{samples_path}/XML/racoon.mjml",
      "#{samples_path}/XML/route-gas-works-lake-union-loop.gpx",
      "#{samples_path}/XML/some-ideas.mm",
      "#{samples_path}/XML/GMOculus.project.gmx",
      "#{samples_path}/XML/obj_control.object.gmx",
      "#{samples_path}/XML/MainView.axaml",
      "#{samples_path}/XML/Robots.slnx",
    ]
    assert_all_xml all_xml_fixtures("*") - no_root_tag

    assert_xml "test/fixtures/XML/app.config"
    assert_xml "test/fixtures/XML/AssertionIDRequestOptionalAttributes.xml.svn-base"
  end
end
