require_relative "./helper"

class TestGrammars < Test::Unit::TestCase
  def setup
    @grammars = YAML.load(File.read(File.expand_path("../../grammars.yml", __FILE__)))
  end

  def test_no_duplicate_scopes
    scopes = @grammars.values.flatten
    duplicates = scopes.group_by { |s| s }.select { |k, v| v.length > 1 }.map(&:first)
    assert duplicates.empty?, "The following scopes appear in grammars.yml more than once:\n#{duplicates.sort.join("\n")}"
  end
end
