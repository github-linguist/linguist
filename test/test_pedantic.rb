require_relative "./helper"

class TestPedantic < Minitest::Test
  filename = File.expand_path("../../lib/linguist/languages.yml", __FILE__)
  LANGUAGES = YAML.load(File.read(filename))
  GRAMMARS = YAML.load(File.read(File.expand_path("../../grammars.yml", __FILE__)))

  def test_language_names_are_sorted
    assert_sorted LANGUAGES.keys
  end

  def test_extensions_are_sorted
    LANGUAGES.each do |name, language|
      extensions = language['extensions']
      assert_sorted extensions[1..-1].map(&:downcase) if extensions && extensions.size > 1
    end
  end

  def test_filenames_are_sorted
    LANGUAGES.each do |name, language|
      assert_sorted language['filenames'] if language['filenames']
    end
  end

  def test_grammars_are_sorted
    assert_sorted GRAMMARS.keys
  end

  def test_scopes_are_sorted
    GRAMMARS.values.each do |scopes|
      assert_sorted scopes
    end
  end

  def test_heuristics_are_sorted
    file = File.expand_path("../../lib/linguist/heuristics.rb", __FILE__)
    heuristics = open(file).each.grep(/^ *disambiguate/)
    assert_sorted heuristics
  end

  def test_heuristics_tests_are_sorted
    file = File.expand_path("../test_heuristics.rb", __FILE__)
    tests = open(file).each.grep(/^ *def test_[a-z_]+_by_heuristics/)
    assert_sorted tests
  end

  def assert_sorted(list)
    list.each_cons(2) do |previous, item|
      flunk "#{previous} should come after #{item}" if previous > item
    end
  end
end
