require_relative "./helper"

class TestBlob < Minitest::Test
  include Linguist

  def test_regexes
    vendored_paths = YAML.load_file(File.expand_path("../../lib/linguist/vendor.yml", __FILE__))
    vendored_regexp = Regexp.new(vendored_paths.join('|'))

    assert ".vscode" =~ vendored_regexp
    refute "testing-vscode-testing" =~ vendored_regexp, "Regex matched the middle of a string, but it shouldn't have"
  end
end
