require 'linguist/modeline'

require 'test/unit'

class TestModeline < Test::Unit::TestCase
  include Linguist

  def test_vi_mode
    assert_equal "cpp", Modeline.extract_mode( "// vi:ft=cpp:" )
    assert_equal "cpp", Modeline.extract_mode( "// vi:filetype=cpp:" )
    assert_equal "cpp", Modeline.extract_mode( "// vim:ft=cpp" )
    assert_equal "cpp", Modeline.extract_mode( "// vim:set ft=cpp" )
    assert_equal "cpp", Modeline.extract_mode( "// ex: set ft=cpp" )
  end

  def test_emacs_mode
    assert_equal "C++", Modeline.extract_mode( "// -*- mode: C++; -*-" )
  end
end
