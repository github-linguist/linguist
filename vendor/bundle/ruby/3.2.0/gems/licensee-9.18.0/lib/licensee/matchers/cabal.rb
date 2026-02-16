# frozen_string_literal: true

module Licensee
  module Matchers
    class Cabal < Licensee::Matchers::Package
      # While we could parse the cabal file, prefer
      # a lenient regex for speed and security. Moar parsing moar problems.
      LICENSE_REGEX = /^\s*license\s*:\s*([a-z\-0-9.]+)\s*$/ix
      LICENSE_CONVERSIONS = {
        'GPL-2'  => 'GPL-2.0',
        'GPL-3'  => 'GPL-3.0',
        'LGPL-3' => 'LGPL-3.0',
        'AGPL-3' => 'AGPL-3.0',
        'BSD2'   => 'BSD-2-Clause',
        'BSD3'   => 'BSD-3-Clause'
      }.freeze

      private

      def license_property
        match = @file.content.match LICENSE_REGEX
        spdx_name(match[1]).downcase if match && match[1]
      end

      def spdx_name(cabal_name)
        LICENSE_CONVERSIONS[cabal_name] || cabal_name
      end
    end
  end
end
