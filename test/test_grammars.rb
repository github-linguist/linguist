require_relative "./helper"

class TestGrammars < Minitest::Test
  ROOT = File.expand_path("../..", __FILE__)

  # List of projects that are allowed without licenses
  PROJECT_WHITELIST = [
    "vendor/grammars/factor",
    "vendor/grammars/go-tmbundle",
    "vendor/grammars/jflex.tmbundle",
    "vendor/grammars/language-csharp",
    "vendor/grammars/language-viml",
    "vendor/grammars/sublimeassembly"
  ].freeze

  # List of allowed SPDX license names
  LICENSE_WHITELIST = %w[
    apache-2.0
    bsd-2-clause
    bsd-3-clause
    isc
    mit
    mpl-2.0
    public
    textmate
    unlicense
    wtfpl
    zlib
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

  def test_submodules_have_recognized_licenses
    unrecognized = submodule_licenses.select { |k,v| v.nil? && Licensee::FSProject.new(k).license_file }
    unrecognized.reject! { |k,v| PROJECT_WHITELIST.include?(k) }
    message = "The following submodules have unrecognized licenses:\n* #{unrecognized.keys.join("\n* ")}\n"
    message << "Please ensure that the project's LICENSE file contains the full text of the license."
    assert_equal Hash.new, unrecognized, message
  end

  def test_submodules_have_licenses
    unlicensed = submodule_licenses.select { |k,v| v.nil? }.reject { |k,v| PROJECT_WHITELIST.include?(k) }
    message = "The following submodules don't have licenses:\n* #{unlicensed.keys.join("\n* ")}\n"
    message << "Please ensure that the project has a LICENSE file, and that the LICENSE file contains the full text of the license."
    assert_equal Hash.new, unlicensed, message
  end

  def test_submodules_have_approved_licenses
    unapproved = submodule_licenses.reject { |k,v| LICENSE_WHITELIST.include?(v) || PROJECT_WHITELIST.include?(k) }.map { |k,v| "#{k}: #{v}"}
    message = "The following submodules have unapproved licenses:\n* #{unapproved.join("\n* ")}\n"
    message << "The license must be added to the LICENSE_WHITELIST in /test/test_grammars.rb once approved."
    assert_equal [], unapproved, message
  end

  def test_submodules_whitelist_has_no_extra_entries
    skip("Need to work out how to handle dual-licensed entities")
    extra_whitelist_entries = PROJECT_WHITELIST - submodule_licenses.select { |k,v| v.nil? }.keys
    not_present = extra_whitelist_entries.reject { |k,v| Dir.exist?(k) }
    licensed = extra_whitelist_entries.select { |k,v| submodule_licenses[k] }

    msg = "The following whitelisted submodules don't appear to be part of the project:\n* #{not_present.join("\n* ")}"
    assert_equal [], not_present, msg

    msg = "The following whitelisted submodules actually have licenses and don't need to be whitelisted:\n* #{licensed.join("\n* ")}"
    assert_equal [], licensed, msg
  end

  def test_submodules_use_https_links
    File.open(".gitmodules", "r") do |fh|
      ssh_submodules = []
      fh.each_line do |line|
        if matches = line.match(/url = (git@.*)/)
          submodule_link = matches.captures[0]
          ssh_submodules.push(submodule_link)
        end
      end
      msg = "The following submodules don't have an HTTPS link:\n* #{ssh_submodules.join("\n* ")}"
      assert_equal [], ssh_submodules, msg
    end
  end

  private

  def submodule_paths
    @submodule_paths ||= `git config --list --file "#{File.join(ROOT, ".gitmodules")}"`.lines.grep(/\.path=/).map { |line| line.chomp.split("=", 2).last }
  end

  # Returns a hash of submodules in the form of submodule_path => license
  def submodule_licenses
    @@submodule_licenses ||= begin
      submodules = {}
      submodule_paths.each { |submodule| submodules[submodule] = submodule_license(submodule) }
      submodules
    end
  end

  # Given the path to a submodule, return its SPDX-compliant license key
  def submodule_license(submodule)
    # Prefer Licensee to detect a submodule's license
    project = Licensee::FSProject.new(submodule)
    return project.license.key if project.license

    # We know a license file exists, but Licensee wasn't able to detect the license,
    # Let's try our own more permissive regex method
    if project.license_file
      path = File.expand_path project.license_file.path, submodule
      license = classify_license(path)
      return license if license
    end

    # Neither Licensee nor our own regex was able to detect the license, let's check the readme
    files = Dir[File.join(ROOT, submodule, "*")]
    if readme = files.find { |path| File.basename(path) =~ /\Areadme\b/i }
      classify_license(readme)
    end
  end

  def classify_license(path)
    content = File.read(path)
    return unless content =~ /\blicen[cs]e\b/i
    if content.include?("Apache License") && content.include?("2.0")
      "apache-2.0"
    elsif content.include?("GNU") && content =~ /general/i && content =~ /public/i
      if content =~ /version 2/i
        "gpl-2.0"
      elsif content =~ /version 3/i
        "gpl-3.0"
      end
    elsif content.include?("GPL") && content.include?("http://www.gnu.org/licenses/gpl.html")
      "gpl-3.0"
    elsif content.include?("Creative Commons Attribution-Share Alike 3.0")
      "cc-by-sa-3.0"
    elsif content.include?("tidy-license.txt") || content.include?("If not otherwise specified (see below)") || content.include?("Permission to copy, use, modify, sell and distribute this")
      "textmate"
    elsif content.include?("Permission is hereby granted") || content =~ /\bMIT\b/
      "mit"
    elsif content.include?("This package is provided as-is and is placed in the Public Domain")
      "public"
    elsif content.include?("http://www.wtfpl.net/txt/copying/")
      "wtfpl"
    elsif content.include?("zlib") && content.include?("license") && content.include?("2. Altered source versions must be plainly marked as such")
      "zlib"
    end
  end
end
