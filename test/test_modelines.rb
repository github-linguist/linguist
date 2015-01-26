require_relative "./helper"

class TestModelines < Minitest::Test
  include Linguist

  def fixtures_path
    File.expand_path("../fixtures", __FILE__)
  end

  def fixture_blob(name)
    name = File.join(fixtures_path, name) unless name =~ /^\//
    FileBlob.new(name, fixtures_path)
  end

  def test_modelines
    assert_equal Language["Ruby"], fixture_blob("Data/Modelines/ruby").language
    assert_equal Language["C++"], fixture_blob("Data/Modelines/seeplusplus").language
    assert_equal Language["Prolog"], fixture_blob("Data/Modelines/not_perl.pl").language
  end
end
