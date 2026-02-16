# frozen_string_literal: true

module Licensee
  # Exposes #conditions, #permissions, and #limitation arrays of LicenseRules
  class LicenseRules < Struct.new(:conditions, :permissions, :limitations)
    include Licensee::HashHelper
    HASH_METHODS = Rule.groups

    class << self
      def from_license(license)
        from_meta(license.meta)
      end

      def from_meta(meta)
        rules = {}
        Rule.groups.each do |group|
          rules[group] = (meta[group] || []).map do |tag|
            Rule.find_by_tag_and_group(tag, group)
          end
        end
        from_hash(rules)
      end

      def from_hash(hash)
        ordered_array = hash.values_at(*members.map(&:to_s))
        new(*ordered_array)
      end
    end

    def flatten
      members.map { |m| public_send(m) }.flatten
    end

    def key?(key)
      members.include?(key.to_sym)
    end
    alias has_key? key?
  end
end
