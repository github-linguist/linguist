# frozen_string_literal: true

module Licensee
  module Matchers
    class Cargo < Licensee::Matchers::Package
      LICENSE_REGEX = %r{
        ^\s*['"]?license['"]?\s*=\s*['"]([a-z\-0-9. +()/]+)['"]\s*
      }ix

      private

      def license_property
        match = @file.content.match LICENSE_REGEX
        match[1].downcase if match && match[1]
      end
    end
  end
end
