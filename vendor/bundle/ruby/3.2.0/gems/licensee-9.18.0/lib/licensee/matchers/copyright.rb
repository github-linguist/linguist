# frozen_string_literal: true

module Licensee
  module Matchers
    class Copyright < Licensee::Matchers::Matcher
      attr_reader :file

      COPYRIGHT_SYMBOLS = Regexp.union([/copyright/i, /\(c\)/i, "\u00A9", "\xC2\xA9"])
      MAIN_LINE_REGEX = /[_*\-\s]*#{COPYRIGHT_SYMBOLS}.*$/i
      OPTIONAL_LINE_REGEX = /[_*\-\s]*with Reserved Font Name.*$/i
      REGEX = /#{ContentHelper::START_REGEX}(#{MAIN_LINE_REGEX}#{OPTIONAL_LINE_REGEX}*)+$/i
      def match
        # NOTE: must use content, and not content_normalized here
        Licensee::License.find('no-license') if /#{REGEX}+\z/io.match?(file.content.strip)
      rescue Encoding::CompatibilityError
        nil
      end

      def confidence
        100
      end
    end
  end
end
