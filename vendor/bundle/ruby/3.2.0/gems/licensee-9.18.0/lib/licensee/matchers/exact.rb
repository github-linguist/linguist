# frozen_string_literal: true

module Licensee
  module Matchers
    class Exact < Licensee::Matchers::Matcher
      def match
        return @match if defined? @match

        @match = potential_matches.find do |potential_match|
          potential_match.wordset == file.wordset
        end
      end

      def confidence
        100
      end
    end
  end
end
