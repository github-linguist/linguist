# frozen_string_literal: true

module Licensee
  module ProjectFiles
    class LicenseFile < Licensee::ProjectFiles::ProjectFile
      include Licensee::ContentHelper

      # List of extensions to give preference to
      PREFERRED_EXT = %w[md markdown txt html].freeze
      PREFERRED_EXT_REGEX = /\.#{Regexp.union(PREFERRED_EXT)}\z/

      # Regex to match any extension and periods in version numbers except .spdx or .header
      LICENSE_EXT_REGEX = %r{\.(?!spdx|header)([^./]|\.\d)+\z}i

      # Regex to match any extension and periods in version numbers except a few unlikely as license
      # texts with complex filenames
      OTHER_EXT_REGEX = %r{\.(?!xml|go|gemspec)([^./]|\.\d)+\z}i

      # Regex to match any extension and periods in version numbers
      ANY_EXT_REGEX = %r{\.([^./]|\.\d)+\z}i

      # Regex to match, LICENSE, LICENCE, unlicense, etc.
      LICENSE_REGEX = /(un)?licen[sc]e/i

      # Regex to match COPYING
      COPYING_REGEX = /copying/i

      # Regex to match COPYRIGHT
      COPYRIGHT_REGEX = /copyright/i

      # Regex to match OFL.
      OFL_REGEX = /ofl/i

      # BSD + PATENTS patent file
      PATENTS_REGEX = /patents/i

      # Hash of Regex => score with which to score potential license files
      FILENAME_REGEXES = {
        /\A#{LICENSE_REGEX}\z/                                => 1.00,  # LICENSE
        /\A#{LICENSE_REGEX}#{PREFERRED_EXT_REGEX}\z/          => 0.95,  # LICENSE.md
        /\A#{COPYING_REGEX}\z/                                => 0.90,  # COPYING
        /\A#{COPYING_REGEX}#{PREFERRED_EXT_REGEX}\z/          => 0.85,  # COPYING.md
        /\A#{LICENSE_REGEX}#{LICENSE_EXT_REGEX}\z/            => 0.80,  # LICENSE.textile
        /\A#{COPYING_REGEX}#{ANY_EXT_REGEX}\z/                => 0.75,  # COPYING.textile
        /\A#{LICENSE_REGEX}[-_][^.]*#{OTHER_EXT_REGEX}?\z/    => 0.70,  # LICENSE-MIT
        /\A#{COPYING_REGEX}[-_][^.]*#{OTHER_EXT_REGEX}?\z/    => 0.65,  # COPYING-MIT
        /\A\w+[-_]#{LICENSE_REGEX}[^.]*#{OTHER_EXT_REGEX}?\z/ => 0.60,  # MIT-LICENSE-MIT
        /\A\w+[-_]#{COPYING_REGEX}[^.]*#{OTHER_EXT_REGEX}?\z/ => 0.55,  # MIT-COPYING
        /\A#{OFL_REGEX}#{PREFERRED_EXT_REGEX}/                => 0.50,  # OFL.md
        /\A#{OFL_REGEX}#{OTHER_EXT_REGEX}/                    => 0.45,  # OFL.textile
        /\A#{OFL_REGEX}\z/                                    => 0.40,  # OFL
        /\A#{COPYRIGHT_REGEX}\z/                              => 0.35,  # COPYRIGHT
        /\A#{COPYRIGHT_REGEX}#{PREFERRED_EXT_REGEX}\z/        => 0.30,  # COPYRIGHT.txt
        /\A#{COPYRIGHT_REGEX}#{OTHER_EXT_REGEX}\z/            => 0.25,  # COPYRIGHT.textile
        /\A#{COPYRIGHT_REGEX}[-_][^.]*#{OTHER_EXT_REGEX}?\z/  => 0.20,  # COPYRIGHT-MIT
        /\A#{PATENTS_REGEX}\z/                                => 0.15,  # PATENTS
        /\A#{PATENTS_REGEX}#{OTHER_EXT_REGEX}\z/              => 0.10,  # PATENTS.txt
        //                                                    => 0.00   # Catch all
      }.freeze

      # CC-NC and CC-ND are not open source licenses and should not be
      # detected as CC-BY or CC-BY-SA which are 98%+ similar
      CC_FALSE_POSITIVE_REGEX = /
        ^(creative\ commons\ )?Attribution-(NonCommercial|NoDerivatives)
      /xi

      def possible_matchers
        [Matchers::Copyright, Matchers::Exact, Matchers::Dice]
      end

      def attribution
        @attribution ||= if copyright? || license.content&.include?('[fullname]')
                           matches = Matchers::Copyright::REGEX
                                     .match(content_without_title_and_version)
                           matches[0] if matches
                         end
      end

      # Is this file likely to result in a creative commons false positive?
      def potential_false_positive?
        content.strip =~ CC_FALSE_POSITIVE_REGEX
      end

      def lgpl?
        LicenseFile.lesser_gpl_score(filename) == 1 && license&.lgpl?
      end

      def gpl?
        license&.gpl?
      end

      def license
        if matcher&.match
          matcher.match
        else
          License.find('other')
        end
      end

      def self.name_score(filename)
        FILENAME_REGEXES.find { |regex, _| filename =~ regex }[1]
      end

      # case-insensitive block to determine if the given file is LICENSE.lesser
      def self.lesser_gpl_score(filename)
        filename.casecmp('copying.lesser').zero? ? 1 : 0
      end
    end
  end
end
