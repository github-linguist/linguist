require 'linguist/overrides'

class TestOverrides < Test::Unit::TestCase
  include Linguist

  # Note this is loading the .linguist file in the root path
  def test_ignored_presence
    overrides = Overrides.new("blah.rb")
    assert !overrides.ignore_regex.nil?
  end

  def test_language_presence
    overrides = Overrides.new("blah.m")
    assert overrides.language_regexes.any?
  end

  def test_language
    assert_equal Language['Assembly'], Overrides.language_for?('test.blah')
  end

  def test_ignored
    should_be_ignored = Overrides.new("benchmark/samples/Ruby/foo.rb")
    should_not_be_ignored = Overrides.new("lib/linguist/classifier.rb")
    assert should_be_ignored.ignored_path?
    assert !should_not_be_ignored.ignored_path?
  end
end
