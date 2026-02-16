# frozen_string_literal: true

class LicenseeCLI < Thor
  # Methods to call when displaying information about ProjectFiles
  MATCHED_FILE_METHODS = %i[
    content_hash attribution confidence matcher license
  ].freeze

  desc 'detect [PATH]', 'Detect the license of the given project'
  option :json, type: :boolean, desc: 'Return output as JSON'
  option :packages, type: :boolean, default: true, desc: 'Detect licenses in package manager files'
  option :readme, type: :boolean, default: true, desc: 'Detect licenses in README files'
  option :confidence, type: :numeric, default: Licensee.confidence_threshold, desc: 'Confidence threshold'
  option :license, type: :string, desc: 'The SPDX ID or key of the license to compare (implies --diff)'
  option :diff, type: :boolean, desc: 'Compare the license to the closest match'
  option :ref, type: :string, desc: 'The name of the commit/branch/tag to search (github.com only)'
  def detect(_path = nil)
    Licensee.confidence_threshold = options[:confidence]

    if options[:json]
      say project.to_h.to_json
      exit !project.licenses.empty?
    end

    rows = []
    rows << if project.license
              ['License:', project.license.spdx_id]
            elsif !project.licenses.empty?
              ['Licenses:', project.licenses.map(&:spdx_id)]
            else
              ['License:', set_color('None', :red)]
            end

    rows << ['Matched files:', project.matched_files.map(&:filename).join(', ')] unless project.matched_files.empty?

    print_table rows

    project.matched_files.each do |matched_file|
      rows = []
      say "#{matched_file.filename}:"

      MATCHED_FILE_METHODS.each do |method|
        next unless matched_file.respond_to? method

        value = matched_file.public_send method
        next if value.nil?

        rows << [humanize(method, :method), humanize(value, method)]
      end
      print_table rows, indent: 2

      next unless matched_file.is_a? Licensee::ProjectFiles::LicenseFile
      next if matched_file.confidence == 100

      licenses = licenses_by_similarity(matched_file)
      next if licenses.empty?

      say '  Closest non-matching licenses:'
      rows = licenses[0...3].map do |license, similarity|
        spdx_id = license.meta['spdx-id']
        percent = Licensee::ContentHelper.format_percent(similarity)
        ["#{spdx_id} similarity:", percent]
      end
      print_table rows, indent: 4
    end

    if project.license_file && (options[:license] || options[:diff])
      license = options[:license] || closest_license_key(project.license_file)
      if license
        invoke(:diff, nil,
               license: license, license_to_diff: project.license_file)
      end
    end

    exit !project.licenses.empty?
  end

  private

  # Given a string or object, prepares it for output and human consumption
  def humanize(value, type = nil)
    case type
    when :license
      value.spdx_id
    when :matcher
      value.class
    when :confidence
      Licensee::ContentHelper.format_percent(value)
    when :method
      "#{value.to_s.tr('_', ' ').capitalize}:"
    else
      value
    end
  end

  def licenses_by_similarity(matched_file)
    matcher = Licensee::Matchers::Dice.new(matched_file)
    potential_licenses = Licensee.licenses(hidden: true).select(&:wordset)
    matcher.instance_variable_set(:@potential_licenses, potential_licenses)
    matcher.licenses_by_similarity
  end

  def closest_license_key(matched_file)
    licenses = licenses_by_similarity(matched_file)
    licenses.first.first.key unless licenses.empty?
  end
end
