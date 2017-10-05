require_relative "./helper"

class TestGrammars < Minitest::Test
  ROOT = File.expand_path("../..", __FILE__)

  # List of projects that are allowed without licenses
  PROJECT_WHITELIST = [
    "vendor/grammars/Sublime-Lasso",
    "vendor/grammars/blitzmax"
  ].freeze

  HASH_WHITELIST = [
    "bc12b3b4917eab9aedb87ec1305c2a4376e34fd1", # TextMate bundles
    "16c4748566b3dd996594af0410a1875b22d3a2b3", # language-yaml and atom-salt
    "ff21db2554d69d78b2220db5615b16bbba0788d3", # factor
    "b4381ebae3235e91aaf5ccab1e8e94e9ad4faef4", # jflex.tmbundle
    "da39a3ee5e6b4b0d3255bfef95601890afd80709", # SCSS.tmbundle
    "b5432a1e1055de7eeede2dddf91e009480651fd6", # jasmin-sublime
    "170b35df61879139b88379a8f1bfd86289c13599", # language-clojure
    "60e1fe192238a032341d5dd3cd80535459fc84e4", # language-coffee-script
    "94fbd554ec1837fb7c508fd7425326639c3f4103", # language-csharp
    "70fb557a431891c2d634c33fa7367feab5066fd6", # language-javascript
    "8653305b358375d0fced85dc24793b99919b11ef", # language-shellscript
    "9f0c0b0926a18f5038e455e8df60221125fc3111", # elixir-tmbundle
    "a4dadb2374282098c5b8b14df308906f5347d79a", # mako-tmbundle
    "b9b24778619dce325b651f0d77cbc72e7ae0b0a3", # Julia.tmbundle
    "e06722add999e7428048abcc067cd85f1f7ca71c", # r.tmbundle
    "50b14a0e3f03d7ca754dac42ffb33302b5882b78", # smalltalk-tmbundle
    "eafbc4a2f283752858e6908907f3c0c90188785b", # gap-tmbundle
    "22b3bf41b9e3e8c22357ee12265f149d68aae60a", # Stylus
    "c87e7e574fca543941650e5b0a144b44c02c55d8", # language-crystal
    "ace112feb693358db2970d0805f6894b745e14b5", # atom-language-purescript
    "a626362e3efd030c1d97c0faf422cf8c2dfaea54", # FreeMarker.tmbundle
    "15a394f6bc43400946570b299aee8ae264a1e3ff", # language-renpy
    "74bb588102e8f332970a0fcabe36299e0806f130", # language-less
    "2f03492b52d7dd83b4e7472f01b87c6121e5b1a4", # monkey
    "784da5ce445892bc3e26beeb6a4402bbc5ca997e", # ant.tmbundle
    "bdab9fdc21e6790b479ccb5945b78bc0f6ce2493", # language-blade
    "c9118c370411f2f049c746c0fd096554e877aea2", # atom-language-perl6
    "15a502335012f27f8a5991139298edb87a6e467d", # atom-language-rust
    "304be6184f7f344d44a1d13bddf511019624fd22", # language-css
    "8c538244ba88ef9902a4faf11a2b9acec46f2a4e", # sublime-nginx
    "82c356d6ecb143a8a20e1658b0d6a2d77ea8126f", # idl.tmbundle
    "9dafd4e2a79cb13a6793b93877a254bc4d351e74", # sublime-text-ox
    "8e111741d97ba2e27b3d18a309d426b4a37e604f", # sublime-varnish
    "23d2538e33ce62d58abda2c039364b92f64ea6bc", # sublime-angelscript
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

    assert nonexistent_submodules.empty? && unlisted_submodules.empty?, message.sub(/\.\Z/, "")
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

  def test_readme_file_is_in_sync
    current_data = File.read("#{ROOT}/vendor/README.md").to_s.sub(/\A.+?<!--.+?-->\n/ms, "")
    updated_data = `script/list-grammars --print`
    assert_equal current_data, updated_data, "Grammar list is out-of-date. Run `script/list-grammars`"
  end

  def test_submodules_have_recognized_licenses
    unrecognized = submodule_licenses.select { |k,v| v.nil? && Licensee::FSProject.new(k).license_file }
    unrecognized.reject! { |k,v| PROJECT_WHITELIST.include?(k) }
    message = "The following submodules have unrecognized licenses:\n* #{unrecognized.keys.join("\n* ")}\n"
    message << "Please ensure that the project's LICENSE file contains the full text of the license"
    assert_equal Hash.new, unrecognized, message
  end

  def test_submodules_have_licenses
    unlicensed = submodule_licenses.select { |k,v| v.nil? }.reject { |k,v| PROJECT_WHITELIST.include?(k) }
    message = "The following submodules don't have licenses:\n* #{unlicensed.keys.join("\n* ")}\n"
    message << "Please ensure that the project has a LICENSE file, and that the LICENSE file contains the full text of the license"
    assert_equal Hash.new, unlicensed, message
  end

  def test_submodules_have_approved_licenses
    unapproved = submodule_licenses.reject { |k,v| LICENSE_WHITELIST.include?(v) ||
                                                   PROJECT_WHITELIST.include?(k) ||
                                                   HASH_WHITELIST.include?(v) }
                                   .map { |k,v| "#{k}: #{v}"}
    message = "The following submodules have unapproved licenses:\n* #{unapproved.join("\n* ")}\n"
    message << "The license must be added to the LICENSE_WHITELIST in /test/test_grammars.rb once approved"
    assert_equal [], unapproved, message
  end

  def test_whitelisted_submodules_dont_have_licenses
    licensed = submodule_licenses.reject { |k,v| v.nil? }.select { |k,v| PROJECT_WHITELIST.include?(k) }
    message = "The following whitelisted submodules have a license:\n* #{licensed.keys.join("\n* ")}\n"
    message << "Please remove them from the project whitelist"
    assert_equal Hash.new, licensed, message
  end

  def test_whitelisted_hashes_dont_have_licenses
    used_hashes = submodule_licenses.values.reject { |v| v.nil? || LICENSE_WHITELIST.include?(v) }
    unused_hashes = HASH_WHITELIST - used_hashes
    message = "The following whitelisted license hashes are unused:\n* #{unused_hashes.join("\n* ")}\n"
    message << "Please remove them from the hash whitelist"
    assert_equal Array.new, unused_hashes, message
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
    @submodule_paths ||= `git config --list --file "#{File.join(ROOT, ".gitmodules")}"`.lines.grep(/\.path=/).map { |line| line.chomp.split("=", 2).last }.reject { |path| path =~ /CodeMirror/ }
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
  # If the license is unrecognized, return its hash
  def submodule_license(submodule)
    # Prefer Licensee to detect a submodule's license
    project = Licensee::FSProject.new(submodule, detect_readme: true)
    return project.license.key if project.license

    # We know a license exists, but no method was able to recognize it.
    # We return the license hash in this case, to uniquely identify it.
    if project.license_file
      return project.license_file.hash
    elsif project.readme
      return project.readme.hash
    end
  end
end
