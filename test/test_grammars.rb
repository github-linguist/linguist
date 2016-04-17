require_relative "./helper"
require "json"

class TestGrammars < Minitest::Test
  ROOT = File.expand_path("../..", __FILE__)

  def setup
    @grammars = YAML.load(File.read(File.join(ROOT, "grammars.yml")))
  end

  def test_no_duplicate_scopes
    scopes = @grammars.values.flatten
    duplicates = scopes.group_by { |s| s }.select { |k, v| v.length > 1 }.map(&:first)
    assert duplicates.empty?, "The following scopes appear in grammars.yml more than once:\n#{duplicates.sort.join("\n")}"
  end

  def test_local_scopes_are_in_sync
    actual = YAML.load(`"#{File.join(ROOT, "script", "convert-grammars")}" --output - --no-install --no-remote`)
    assert $?.success?, "script/convert-grammars failed"

    # We're not checking remote grammars. That can take a long time and make CI
    # flaky if network conditions are poor.
    @grammars.delete_if { |k, v| k.start_with?("http:", "https:") }

    @grammars.each do |k, v|
      assert_equal v, actual[k], "The scopes listed for #{k} in grammars.yml don't match the scopes found in that repository"
    end
  end

  def test_grammars_use_https_links
    ssh_grammars = []
    JSON.parse(File.read(File.join(ROOT, "vendor/bower.json")))["dependencies"].each do |dir, url|
      if matches = url.match(/(git@.*)/)
        submodule_link = matches.captures[0]
        ssh_grammars.push(submodule_link)
      end
    end

    msg = "The following submodules don't have an HTTPS link:\n* #{ssh_grammars.join("\n* ")}"
    assert_equal [], ssh_grammars, msg
  end
end
