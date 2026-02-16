# frozen_string_literal: true

require 'tmpdir'

class LicenseeCLI < Thor
  desc 'diff [PATH]', 'Compare the given license text to a known license'
  option :license, type: :string, desc: 'The SPDX ID or key of the license to compare'
  def diff(_path = nil)
    say "Comparing to #{expected_license.name}:"
    rows = []

    left = expected_license.content_normalized(wrap: 80)
    right = license_to_diff.content_normalized(wrap: 80)
    similarity = expected_license.similarity(license_to_diff)
    similarity = Licensee::ContentHelper.format_percent(similarity)

    rows << ['Input Length:', license_to_diff.length]
    rows << ['License length:', expected_license.length]
    rows << ['Similarity:', similarity]
    print_table rows

    if left == right
      say 'Exact match!', :green
      exit
    end

    Dir.mktmpdir do |dir|
      path = File.expand_path 'LICENSE', dir
      Dir.chdir(dir) do
        `git init`
        File.write(path, left)
        `git add LICENSE`
        `git commit -m 'left'`
        File.write(path, right)
        say `git diff --word-diff`
      end
    end
  end

  private

  def license_to_diff
    return options[:license_to_diff] if options[:license_to_diff]
    return project.license_file if remote? || ($stdin.tty? && project.license_file)

    @license_to_diff ||= Licensee::ProjectFiles::LicenseFile.new($stdin.read, 'LICENSE')
  end

  def expected_license
    @expected_license ||= Licensee::License.find options[:license] if options[:license]
    return @expected_license if @expected_license

    if options[:license]
      error "#{options[:license]} is not a valid license"
    else
      error 'Usage: provide a license to diff against with --license (spdx name)'
    end

    error "Valid licenses: #{Licensee::License.all(hidden: true).map(&:key).join(', ')}"
    exit 1
  end
end
