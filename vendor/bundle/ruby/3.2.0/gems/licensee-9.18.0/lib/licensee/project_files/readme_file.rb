# frozen_string_literal: true

module Licensee
  module ProjectFiles
    class ReadmeFile < Licensee::ProjectFiles::LicenseFile
      EXTENSIONS = %w[md markdown mdown txt rdoc rst].freeze
      SCORES = {
        /\AREADME\z/i                                       => 1.0,
        /\AREADME\.(#{Regexp.union(EXTENSIONS).source})\z/i => 0.9
      }.freeze

      TITLE_REGEX = /licen[sc]e:?/i
      UNDERLINE_REGEX = /\n[-=]+/m
      CONTENT_REGEX = /^
          (?:                                # Header lookbehind
            [\#=]+\s#{TITLE_REGEX}\s*[\#=]*  # Start of hashes or rdoc header
          |
            #{TITLE_REGEX}#{UNDERLINE_REGEX} # Start of underlined header
          )$
          (.*?)                              # License content
          (?=^                               # Header or end of file lookahead
            (?:
              [\#=]+                         # Next hash or rdoc header
            |
              [^\n]+#{UNDERLINE_REGEX}       # Next of underlined header
            )
          |
            \z                               # End of file
          )
        /mix

      def possible_matchers
        super.push(Matchers::Reference)
      end

      def self.name_score(filename)
        SCORES.each do |pattern, score|
          return score if pattern&.match?(filename)
        end
        0.0
      end

      def self.license_content(content)
        match = CONTENT_REGEX.match(content)
        match[1].strip if match
      end
    end
  end
end
