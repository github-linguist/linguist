# frozen_string_literal: true

module Licensee
  module Matchers
    class Cran < Licensee::Matchers::Package
      attr_reader :file

      # While we could parse the DESCRIPTION file, prefer
      # a lenient regex for speed and security. Moar parsing moar problems.
      LICENSE_FIELD_REGEX = /^license:\s*(.+)/i
      PLUS_FILE_LICENSE_REGEX = /\s*\+\s*file\s+LICENSE$/i
      GPL_VERSION_REGEX = /^GPL(?:-([23])|\s*\(\s*>=\s*([23])\s*\))$/i

      private

      # Returns the raw license string from the `license: ` field
      # or `nil` if no license field is found
      def license_field
        return @license_field if defined? @license_field

        match = @file.content.match LICENSE_FIELD_REGEX
        @license_field = match ? match[1].downcase : nil
      end

      # returns the normalized GPL version, if the license is a GPL license
      # Otherwise, returns `nil`
      def gpl_version(license_key)
        match = license_key.match GPL_VERSION_REGEX
        match ? "gpl-#{match[1] || match[2]}.0" : nil
      end

      # Normalizes the license field value to an SPDX ID
      # Rerurns `nil` if no license is found
      def license_property
        return unless license_field

        # Remove The common + file LICENSE text
        license_key = license_field.sub(PLUS_FILE_LICENSE_REGEX, '')
        gpl_version(license_key) || license_key
      end
    end
  end
end
