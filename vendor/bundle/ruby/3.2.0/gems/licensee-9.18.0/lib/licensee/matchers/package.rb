# frozen_string_literal: true

module Licensee
  module Matchers
    class Package < Licensee::Matchers::Matcher
      def match
        return @match if defined? @match
        return if license_property.nil? || license_property.to_s.empty?

        @match = Licensee.licenses(hidden: true).find do |license|
          license.key == license_property
        end
        @match ||= License.find('other')
      end

      def confidence
        90
      end

      def license_property
        raise 'Not implemented'
      end
    end
  end
end
