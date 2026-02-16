require 'test/unit'
require_relative '../lib/pathname/common_prefix'

class CommonPrefixTest < Test::Unit::TestCase
  def test_returns_common_prefix
    paths = %w[
      /full/path/to/somewhere
      /full/path/to/anywhere
      /full/path/to/nowhere
      /full/path/to/somewhere/else
    ].map {|path| Pathname(path)}
    assert_equal Pathname('/full/path/to'), Pathname.common_prefix(*paths)

    paths = %w[
      /full/path/to/somewhere
      /full/path
      /full/path/to/nowhere
      /full/path/to/somewhere/else
    ].map {|path| Pathname(path)}
    assert_equal Pathname('/full/path'), Pathname.common_prefix(*paths)

    paths = %w[
      /full/path/to/somewhere
      /full/path
      /full/path/to/nowhere
      /full
    ].map {|path| Pathname(path)}
    assert_equal Pathname('/full'), Pathname.common_prefix(*paths)
  end

  def test_returns_common_prefix_with_String
    paths = %w[
      /full/path/to/somewhere
      /full/path/to/anywhere
      /full/path/to/nowhere
      /full/path/to/somewhere/else
    ]
    assert_equal Pathname('/full/path/to'), Pathname.common_prefix(*paths)
  end

  def test_returns_nil_when_no_common_prefix
    paths = %w[
      /absolute/path
      relative/path
    ].map {|path| Pathname(path)}
    assert_nil Pathname.common_prefix(*paths)
  end

  def test_returns_nil_when_empty_array_passed
    assert_nil Pathname.common_prefix(*[])
  end

  def test_returns_nil_when_no_argument_passed
    assert_nil Pathname.common_prefix
  end

  def test_accept_array_of_pathnames_and_or_path_strings
    paths = %[
        /path/to/file
        Pathname(/path/to/another/file)
    ]
    assert_nothing_raised do
      Pathname.common_prefix paths
    end
  end

  def test_flatten_argument
    paths = [
        '/path/to/file',
        '/path/to/other/file',
        ['/path/to/another/file', '/path/to/some/file/s']
    ]
    assert_equal Pathname.common_prefix(*paths), Pathname.common_prefix(paths)
  end

  def test_returns_start_with
    assert_same true, Pathname('/full/path/to/somewhere').start_with?('/full/path/to')
    assert_same true, Pathname('/full/path/to/somewhere').start_with?('/')
    assert_same false, Pathname('/full/path/to/somewhere').start_with?('/path/to')
  end
end
