require_relative "./helper"

class TestModelines < Minitest::Test
  include Linguist

  def assert_modeline(language, blob)
    assert_equal language, Linguist::Strategy::Modeline.call(blob).first
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
    assert_modeline nil, sample_blob("C/main.c")
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
end
