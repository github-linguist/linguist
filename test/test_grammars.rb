require_relative "./helper"

class TestGrammars < Minitest::Test
  ROOT = File.expand_path("../..", __FILE__)

  # List of projects that are allowed without licenses
  PROJECT_WHITELIST = [
    "vendor/grammars/Sublime-Lasso",      # No license file
    "vendor/grammars/blitzmax",           # No license file
    "vendor/grammars/creole",             # License filename is not LICENSE(.*)?
  ].freeze

  HASH_WHITELIST = [
    "4b4c1b4aff11b99e53091f95b24093655c0d9ed2", # Agda.tmbundle
    "a92c0e4e644206078a158d663b9f033d0d0aefbf", # ant.tmbundle
    "98eac91395c97ae090113e1b4527cae3228d563a", # bro-sublime
    "220e011c8d686129e9c4163a7c655b9d64f61e59", # elixir-tmbundle
    "55f431fb78e7b3cb6d18ef1042f70ebf4b8d838d", # factor
    "b81acf2ba52d312754bf5055845a723123bda388", # FreeMarker.tmbundle
    "ee77ce4cf9121bccc3e37ba6b98f8e7acd589aaf", # gap-tmbundle
    "4cfc7ce12de920ccc836bbab2d748151d5ba7e38", # go-tmbundle
    "6c2e34d62c08f97a3e2ece3eedc65fbd99873ff4", # idl.tmbundle
    "e68efca5a844aa78729cadcf42507013151e6605", # jflex.tmbundle
    "39f092c726491ca6a02354dbc6c3a0920bb44d4c", # mako-tmbundle
    "7821982b18bc35d6925cc16ece68d9c71f1fbba3", # moonscript-tmbundle
    "806dae0a87f95409496223dfd9a45cc57c878aeb", # PHP-Twig.tmbundle
    "0c216b112f3a4e6d5848128504d8378d8c7eee00", # r.tmbundle
    "da39a3ee5e6b4b0d3255bfef95601890afd80709", # SCSS.tmbundle
    "68539730d3cde34355f429f2267e265c1e030912", # smalltalk-tmbundle
    "4b5f67a54532ca6e49ba44cd135a510a74712e07", # Stylus
    "23d2538e33ce62d58abda2c039364b92f64ea6bc", # sublime-angelscript
    "38ce879dc8b217a080caa2ff9e2241c9505256d6", # sublime-shen
    "3df4ef028c6384b64bc59b8861d6c52093b2116d", # sublime-text-ox
    "c18bcd787325167bf1405629259c091d49064541", # Std license in README.md of many TextMate grammars like abap.tmbundle
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

  def test_readme_file_is_in_sync
    current_data = File.read("#{ROOT}/vendor/README.md").to_s.sub(/\A.+?<!--.+?-->\n/ms, "")
    updated_data = `script/list-grammars --print`
    assert_equal current_data, updated_data, "Grammar list is out-of-date. Run `script/list-grammars`"
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
    project = Licensee.project(submodule, detect_packages: true, detect_readme: true)
    return project.license.key if project.licenses.length == 1 && !project.license.pseudo_license?

    # If we have more than one license, return the first one that isn't a
    # pseudo-license (other or no-license), if any
    if project.licenses.length > 1
      first_real_license = project.licenses.reject{ |f| f.pseudo_license? }.first
      return first_real_license.key unless first_real_license.nil?
    end

    # We know a license exists, but no method was able to recognize it.
    # We return the license hash in this case, to uniquely identify it.
    if project.license_file
      return project.license_file.content_hash
    elsif project.readme
      return project.readme.content_hash
    end
  end
end
