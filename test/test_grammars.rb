require_relative "./helper"

class TestGrammars < Minitest::Test
  ROOT = File.expand_path("../..", __FILE__)

  LICENSE_WHITELIST = [
    # This grammar's MIT license is inside a subdirectory.
    "vendor/grammars/SublimePapyrus",
    "vendor/grammars/gap-tmbundle",

    # These grammars have no license but have been grandfathered in. New grammars
    # must have a license that allows redistribution.
    "vendor/grammars/Sublime-Lasso",
    "vendor/grammars/Sublime-REBOL",
    "vendor/grammars/x86-assembly-textmate-bundle",
  ].freeze

  def setup
    @grammars = YAML.load(File.read(File.join(ROOT, "grammars.yml")))
  end

  def test_no_duplicate_scopes
    scopes = @grammars.values.flatten
    duplicates = scopes.group_by { |s| s }.select { |k, v| v.length > 1 }.map(&:first)
    assert duplicates.empty?, "The following scopes appear in grammars.yml more than once:\n#{duplicates.sort.join("\n")}"
  end

  def test_submodules_are_in_sync
    # Strip off paths inside the submodule so that just the submodule path remains.
    listed_submodules = @grammars.keys.grep(/vendor\/grammars/).map { |source| source[%r{vendor/grammars/[^/]+}] }

    nonexistent_submodules = listed_submodules - submodule_paths
    unlisted_submodules = submodule_paths - listed_submodules

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
    actual = YAML.load(`"#{File.join(ROOT, "script", "convert-grammars")}" --output - --no-install --no-remote`)
    assert $?.success?, "script/convert-grammars failed"

    # We're not checking remote grammars. That can take a long time and make CI
    # flaky if network conditions are poor.
    @grammars.delete_if { |k, v| k.start_with?("http:", "https:") }

    @grammars.each do |k, v|
      assert_equal v, actual[k], "The scopes listed for #{k} in grammars.yml don't match the scopes found in that repository"
    end
  end

  def test_submodules_have_licenses
    categories = submodule_paths.group_by do |submodule|
      files = Dir[File.join(ROOT, submodule, "*")]
      license = files.find { |path| File.basename(path) =~ /\b(un)?licen[cs]e\b/i } || files.find { |path| File.basename(path) =~ /\bcopying\b/i }
      if license.nil?
        if readme = files.find { |path| File.basename(path) =~ /\Areadme\b/i }
          license = readme if File.read(readme) =~ /\blicen[cs]e\b/i
        end
      end
      if license.nil?
        :unlicensed
      elsif classify_license(license)
        :licensed
      else
        :unrecognized
      end
    end

    unlicensed = categories[:unlicensed] || []
    unrecognized = categories[:unrecognized] || []
    disallowed_unlicensed = unlicensed - LICENSE_WHITELIST
    disallowed_unrecognized = unrecognized - LICENSE_WHITELIST
    extra_whitelist_entries = LICENSE_WHITELIST - (unlicensed | unrecognized)

    message = ""
    if disallowed_unlicensed.any?
      message << "The following grammar submodules don't seem to have a license. All grammars must have a license that permits redistribution.\n"
      message << disallowed_unlicensed.sort.join("\n")
    end
    if disallowed_unrecognized.any?
      message << "\n\n" unless message.empty?
      message << "The following grammar submodules have an unrecognized license. Please update #{__FILE__} to recognize the license.\n"
      message << disallowed_unrecognized.sort.join("\n")
    end
    if extra_whitelist_entries.any?
      message << "\n\n" unless message.empty?
      message << "The following grammar submodules are listed in LICENSE_WHITELIST but either have a license (yay!)\n"
      message << "or have been removed from the repository. Please remove them from the whitelist.\n"
      message << extra_whitelist_entries.sort.join("\n")
    end

    assert disallowed_unlicensed.empty? && disallowed_unrecognized.empty? && extra_whitelist_entries.empty?, message
  end

  private

  def submodule_paths
    @submodule_paths ||= `git config --list --file "#{File.join(ROOT, ".gitmodules")}"`.lines.grep(/\.path=/).map { |line| line.chomp.split("=", 2).last }
  end

  def classify_license(path)
    content = File.read(path)
    if content.include?("Apache License") && content.include?("2.0")
      "Apache 2.0"
    elsif content.include?("GNU") && content =~ /general/i && content =~ /public/i
      if content =~ /version 2/i
        "GPLv2"
      elsif content =~ /version 3/i
        "GPLv3"
      end
    elsif content.include?("GPL") && content.include?("http://www.gnu.org/licenses/gpl.html")
      "GPLv3"
    elsif content.include?("Creative Commons")
      "CC"
    elsif content.include?("tidy-license.txt") || content.include?("If not otherwise specified (see below)")
      "textmate"
    elsif content =~ /^\s*[*-]\s+Redistribution/ || content.include?("Redistributions of source code")
      "BSD"
    elsif content.include?("Permission is hereby granted") || content =~ /\bMIT\b/
      "MIT"
    elsif content.include?("unlicense.org")
      "unlicense"
    elsif content.include?("http://www.wtfpl.net/txt/copying/")
      "WTFPL"
    elsif content.include?("zlib") && content.include?("license") && content.include?("2. Altered source versions must be plainly marked as such")
      "zlib"
    end
  end
end
