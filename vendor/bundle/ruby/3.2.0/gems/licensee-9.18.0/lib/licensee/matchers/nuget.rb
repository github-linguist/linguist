# frozen_string_literal: true

module Licensee
  module Matchers
    class NuGet < Licensee::Matchers::Package
      # While we could parse the nuspec file, prefer a lenient regex for speed and security.
      # Moar parsing moar problems.
      LICENSE_REGEX = %r{
        <license\s*type\s*=\s*["']expression["']\s*>([a-z\-0-9. +()]+)</license\s*>
      }ix

      LICENSE_URL_REGEX = %r{<licenseUrl>\s*(.*)\s*</licenseUrl>}i

      NUGET_REGEX = %r{https?://licenses.nuget.org/(.*)}i
      OPENSOURCE_REGEX = %r{https?://(?:www\.)?opensource.org/licenses/(.*)}i
      SPDX_REGEX = %r{https?://(?:www\.)?spdx.org/licenses/(.*?)(?:\.html|\.txt)?$}i
      APACHE_REGEX = %r{https?://(?:www\.)?apache.org/licenses/(.*?)(?:\.html|\.txt)?$}i

      private

      def license_from_first_capture(url, pattern)
        match = url.match(pattern)
        match[1].downcase if match && match[1]
      end

      def license_from_url(url)
        license_from_first_capture(url, NUGET_REGEX) ||
          license_from_first_capture(url, OPENSOURCE_REGEX) ||
          license_from_first_capture(url, SPDX_REGEX) ||
          license_from_first_capture(url, APACHE_REGEX)&.gsub('license', 'apache')
      end

      def license_property
        # Prefer the explicit <license type="expression"> element
        match = @file.content.match LICENSE_REGEX
        return match[1].downcase if match && match[1]

        url_match = @file.content.match LICENSE_URL_REGEX
        license_from_url(url_match[1]) if url_match && url_match[1]
      end
    end
  end
end
