require_relative "./helper"

class TestFileContents < Minitest::Test

  def test_extensions
    assert_equal [".gitignore"], Linguist::FileContents.new(".gitignore", nil).extensions
  end
  
  def test_detection
    assert_equal "Ruby", Linguist::FileContents.new(__FILE__, File.read(__FILE__)).language.name
  end

  def test_name
    assert_equal ".gitignore", Linguist::FileContents.new(".gitignore", nil).name
  end

  def test_data
    assert_equal ".DS_Store", Linguist::FileContents.new(".gitignore", ".DS_Store").data
  end

end
