require_relative "./helper"

class TestModelines < Minitest::Test
  include Linguist

  def assert_modeline(language, blob)
    assert_equal language, Linguist::Strategy::Modeline.call(blob).first
  end

  def test_modeline_strategy
    assert_modeline Language["Ruby"], fixture_blob("Data/Modelines/ruby")
    assert_modeline Language["C++"], fixture_blob("Data/Modelines/seeplusplus")
    assert_modeline Language["Prolog"], fixture_blob("Data/Modelines/not_perl.pl")
    assert_modeline Language["Smalltalk"], fixture_blob("Data/Modelines/example_smalltalk.md")
    assert_modeline Language["PHP"], fixture_blob("Data/Modelines/iamphp.inc")
    assert_modeline Language["Emacs Lisp"], fixture_blob("Data/Modelines/emacs-lisp")
  end

  def test_modeline_languages
    assert_equal Language["Ruby"], fixture_blob("Data/Modelines/ruby").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplus").language
    assert_equal Language["Prolog"], fixture_blob("Data/Modelines/not_perl.pl").language
    assert_equal Language["Smalltalk"], fixture_blob("Data/Modelines/example_smalltalk.md").language
    assert_equal Language["PHP"], fixture_blob("Data/Modelines/iamphp.inc").language
    assert_equal Language["Emacs Lisp"], fixture_blob("Data/Modelines/emacs-lisp").language
  end
end
