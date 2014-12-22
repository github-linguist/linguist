require_relative "./helper"

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

  def test_submodules_are_in_sync
    submodules = `git config --list --file "#{File.join(ROOT, ".gitmodules")}"`.lines.grep(/\.path=/).map { |line| line.chomp.split("=", 2).last }
    # Strip off paths inside the submodule so that just the submodule path remains.
    listed_submodules = @grammars.keys.grep(/vendor\/grammars/).map { |source| source[%r{vendor/grammars/[^/]+}] }

    nonexistent_submodules = listed_submodules - submodules
    unlisted_submodules = submodules - listed_submodules

    message = ""
    unless nonexistent_submodules.empty?
      message << "The following submodules are listed in grammars.yml but don't seem to exist in the repository.\n"
      message << "Either add them using `git submodule add` or remove them from grammars.yml.\n"
      message << nonexistent_submodules.sort.join("\n")
    end
    unless unlisted_submodules.empty?
      message << "\n" unless message.empty?
      message << "The following submodules exist in the repository but aren't listed in grammars.yml.\n"
      message << "Either add them to grammars.yml or remove them from the repository using `git rm`.\n"
      message << unlisted_submodules.sort.join("\n")
    end

    assert nonexistent_submodules.empty? && unlisted_submodules.empty?, message
  end

  def test_local_scopes_are_in_sync
    actual = YAML.load(`"#{File.join(ROOT, "script", "convert-grammars")}" --output - --no-install --no-remote 2>/dev/null`)
    assert_predicate $?, :success?

    # We're not checking remote grammars. That can take a long time and make CI
    # flaky if network conditions are poor.
    @grammars.delete_if { |k, v| k.start_with?("http:", "https:") }

    @grammars.each do |k, v|
      assert_equal v, actual[k], "The scopes listed for #{k} in grammars.yml don't match the scopes found in that repository"
    end
  end
end
