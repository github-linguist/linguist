# frozen_string_literal: true

module Licensee
  module Matchers
    class Gemspec < Licensee::Matchers::Package
      # a value is a string surrounded by any amount of whitespace
      # optionally ended with (non-captured) ".freeze"
      VALUE_REGEX = /\s*['"]([a-z\-0-9.]+)['"](?:\.freeze)?\s*/i

      # an array contains one or more values. all values, or array itself,
      # can be surrounded by any amount of whitespace.  do not capture
      # non-value groups
      ARRAY_REGEX = /\s*\[#{VALUE_REGEX}(?:,#{VALUE_REGEX})*\]\s*/i

      DECLARATION_REGEX = /
        ^\s*[a-z0-9_]+\.([a-z0-9_]+)\s*=#{VALUE_REGEX}$
      /ix

      LICENSE_REGEX = /
        ^\s*[a-z0-9_]+\.license\s*=#{VALUE_REGEX}$
      /ix

      LICENSE_ARRAY_REGEX = /
        ^\s*[a-z0-9_]+\.licenses\s*=#{ARRAY_REGEX}$
      /ix

      private

      def license_property
        match = @file.content.match LICENSE_REGEX
        return match[1].downcase if match && match[1]

        # check for a licenses array property
        licenses = license_array_property
        return unless licenses

        # use 'other' if array contains multiple licenses
        return 'other' unless licenses.size == 1

        licenses[0]
      end

      def license_array_property
        match = @file.content.match LICENSE_ARRAY_REGEX
        match.captures.compact.map(&:downcase) if match
      end

      def declarations
        @declarations ||= @file.content.match DECLARATION_REGEX
      end
    end
  end
end
