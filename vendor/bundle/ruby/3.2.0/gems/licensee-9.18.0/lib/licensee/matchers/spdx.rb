# frozen_string_literal: true

module Licensee
  module Matchers
    class Spdx < Licensee::Matchers::Package
      # While we could parse the LICENSE.spdx file, prefer
      # a lenient regex for speed and security. Moar parsing moar problems.
      LICENSE_REGEX = /PackageLicenseDeclared:\s*([a-z\-0-9. +()]+)\s*/i

      private

      def license_property
        match = @file.content.match LICENSE_REGEX
        match[1].downcase if match && match[1]
      end
    end
  end
end
