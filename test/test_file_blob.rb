require_relative "./helper"

class TestFileBlob < Minitest::Test
  def test_extensions
    assert_equal [".gitignore"], Linguist::FileBlob.new(".gitignore").extensions
    assert_equal [".xml"],  Linguist::FileBlob.new("build.xml").extensions
    assert_equal [".html.erb", ".erb"],  Linguist::FileBlob.new("dotted.dir/index.html.erb").extensions
  end
end
