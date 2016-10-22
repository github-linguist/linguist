require_relative "./helper"

class TestGrammars < Minitest::Test
  ROOT = File.expand_path("../..", __FILE__)

  # List of projects that are allowed without licenses
  PROJECT_WHITELIST = [
    "vendor/grammars/Sublime-Lasso",
    "vendor/grammars/ant.tmbundle",
    "vendor/grammars/sublime-spintools",
    "vendor/grammars/blitzmax"
  ].freeze

  HASH_WHITELIST = [
    "bc12b3b4917eab9aedb87ec1305c2a4376e34fd1", # TextMate bundles
    "16c4748566b3dd996594af0410a1875b22d3a2b3", # language-yaml and atom-salt
    "ebae2d87e06d3acef075d049fcfc8958c0364863", # go-tmbundle
    "ff21db2554d69d78b2220db5615b16bbba0788d3", # factor
    "b9a7428fd036eed8503995e06e989180c276b17d", # jflex.tmbundle
    "da39a3ee5e6b4b0d3255bfef95601890afd80709", # SCSS.tmbundle
    "5f772ff20ddf3dbac1ec9b6a98c5aa50ace555b2", # gradle.tmbundle
    "b5432a1e1055de7eeede2dddf91e009480651fd6", # jasmin-sublime
    "74143c4d2a5649eb179105afcb37f466558c22ce", # language-clojure
    "760471435f5ab0b9dc99a628203cd8f9156d28ce", # language-coffee-script
    "330e6d465e26bdd232aafcd3f5dba6a1d098a20e", # language-csharp
    "70fb557a431891c2d634c33fa7367feab5066fd6", # language-javascript
    "e0528c23cd967f999e058f1408ccb5b7237daaba", # language-python
    "8653305b358375d0fced85dc24793b99919b11ef", # language-shellscript
    "9f0c0b0926a18f5038e455e8df60221125fc3111", # elixir-tmbundle
    "90af581219debd4e90ef041b46c294e8b4ae6d14", # mako-tmbundle
    "b9b24778619dce325b651f0d77cbc72e7ae0b0a3", # Julia.tmbundle
    "e06722add999e7428048abcc067cd85f1f7ca71c", # r.tmbundle
    "50b14a0e3f03d7ca754dac42ffb33302b5882b78", # smalltalk-tmbundle
    "eafbc4a2f283752858e6908907f3c0c90188785b", # gap-tmbundle
    "1faa3a44cac6070f22384332434af37dfaaf2f70", # Stylus
    "c87e7e574fca543941650e5b0a144b44c02c55d8", # language-crystal
    "c78ec142ac3126cf639cfd67bd646ed8226d8b74", # atom-language-purescript
    "341d7f66806fc41d081133d6e51ade856352e056", # FreeMarker.tmbundle
    "15a394f6bc43400946570b299aee8ae264a1e3ff", # language-renpy
    "c9118c370411f2f049c746c0fd096554e877aea2", # perl6fe
    "8ccf886749c32fb7e65d4d1316a7ed0479c93dc9", # language-less
    "2f03492b52d7dd83b4e7472f01b87c6121e5b1a4", # monkey
    "9d8b5626cfe00f3c8a076173913c3b0312b5b122", # ejs-tmbundle
    "bdab9fdc21e6790b479ccb5945b78bc0f6ce2493"  # language-blade
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
    unapproved = submodule_licenses.reject { |k,v| LICENSE_WHITELIST.include?(v) ||
                                                   PROJECT_WHITELIST.include?(k) ||
                                                   HASH_WHITELIST.include?(v) }
                                   .map { |k,v| "#{k}: #{v}"}
    message = "The following submodules have unapproved licenses:\n* #{unapproved.join("\n* ")}\n"
    message << "The license must be added to the LICENSE_WHITELIST in /test/test_grammars.rb once approved."
    assert_equal [], unapproved, message
  end

  def test_whitelisted_submodules_dont_have_licenses
    licensed = submodule_licenses.reject { |k,v| v.nil? }.select { |k,v| PROJECT_WHITELIST.include?(k) }
    message = "The following whitelisted submodules have a license:\n* #{licensed.keys.join("\n* ")}\n"
    message << "Please remove them from the project whitelist."
    assert_equal Hash.new, licensed, message
  end

  def test_whitelisted_hashes_dont_have_licenses
    used_hashes = submodule_licenses.values.reject { |v| v.nil? || LICENSE_WHITELIST.include?(v) }
    unused_hashes = HASH_WHITELIST - used_hashes
    message = "The following whitelisted license hashes are unused:\n* #{unused_hashes.join("\n* ")}\n"
    message << "Please remove them from the hash whitelist."
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
