# frozen_string_literal: true

module Licensee
  module Matchers
    class DistZilla < Licensee::Matchers::Package
      attr_reader :file

      LICENSE_REGEX = /^license\s*=\s*([a-z\-0-9._]+)/i

      private

      def license_property
        match = file.content.match LICENSE_REGEX
        spdx_name(match[1]).downcase if match && match[1]
      end

      def spdx_name(perl_name)
        perl_name.sub('_', '-')
                 .sub('_', '.')
                 .sub('Mozilla', 'MPL')
                 .sub(/^GPL-(\d)$/, 'GPL-\1.0')
                 .sub(/^AGPL-(\d)$/, 'AGPL-\1.0')
      end
    end
  end
end
