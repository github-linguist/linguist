# frozen_string_literal: true

module Licensee
  module Matchers
    class NpmBower < Licensee::Matchers::Package
      # While we could parse the package.json or bower.json file, prefer
      # a lenient regex for speed and security. Moar parsing moar problems.
      LICENSE_REGEX = /
        \s*["']license["']\s*:\s*['"]([a-z\-0-9.+ ()]+)['"],?\s*
      /ix

      private

      def license_property
        match = @file.content.match LICENSE_REGEX
        return unless match && match[1]
        return 'no-license' if match[1] == 'UNLICENSED'

        match[1].downcase
      end
    end
  end
end
