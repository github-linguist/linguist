require_relative "./helper"

class TestPedantic < Minitest::Test
  filename = File.expand_path("../../lib/linguist/languages.yml", __FILE__)
  LANGUAGES = YAML.load(File.read(filename))
  GRAMMARS = YAML.load(File.read(File.expand_path("../../grammars.yml", __FILE__)))
  GENERICS = YAML.load_file(File.expand_path("../../lib/linguist/generic.yml", __FILE__))
  HEURISTICS = YAML.load_file(File.expand_path("../../lib/linguist/heuristics.yml", __FILE__))

  def test_language_names_are_sorted
    assert_sorted LANGUAGES.keys
  end

  def test_nonprimary_extensions_are_sorted
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

  def test_generics_are_sorted
    assert_sorted GENERICS.keys
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
    disambiguations = HEURISTICS['disambiguations']
    assert_sorted disambiguations.map { |r| r['extensions'][0] }
    assert_sorted HEURISTICS['named_patterns'].keys
  end

  def test_heuristics_tests_are_sorted
    file = File.expand_path("../test_heuristics.rb", __FILE__)
    tests = open(file).each.grep(/^ *def test_[0-9a-z_]+_by_heuristics/)
    assert_sorted tests
  end

  def test_heuristics_tests_are_exhaustive
    file = File.expand_path("../test_heuristics.rb", __FILE__)
    tests = open(file).each.grep(/^ *def test_[0-9a-z_]+_by_heuristics/)
    tests = tests.map { |s| s.match(/test_(.*)_by_heuristic/).captures[0] }

    extensions = HEURISTICS['disambiguations'].map { |r| r['extensions'] }
    extensions = extensions.flatten(1).map { |e| e[1..-1] }

    extensions.each do |ext|
      assert tests.include?(ext), "Extension .#{ext} has no test in test_heuristics.rb"
    end
  end

  def test_submodules_are_sorted
    system(File.expand_path("../../script/sort-submodules", __FILE__) + " -t")
    assert $?.success?
  end

  def assert_sorted(list)
    list.each_cons(2) do |previous, item|
      flunk "#{previous} should come after #{item}" if previous > item
    end
  end

  def test_no_unknown_fields
    known_fields = <<~END.split("\n")
      ace_mode
      aliases
      codemirror_mime_type
      codemirror_mode
      color
      extensions
      filenames
      fs_name
      group
      interpreters
      language_id
      searchable
      tm_scope
      type
      wrap
    END
    LANGUAGES.each do |name, language|
      unknown_fields = language.keys.reject { |key| known_fields.include?(key) }
      message = "Language '#{name}' has the following unknown field(s):\n"
      message << unknown_fields.map { |key| sprintf("  - %s: %s", key, language[key]) }.sort.join("\n")
      assert unknown_fields.empty?, message
    end
  end
end
