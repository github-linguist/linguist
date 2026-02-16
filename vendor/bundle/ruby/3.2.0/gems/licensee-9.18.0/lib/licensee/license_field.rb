# frozen_string_literal: true

module Licensee
  class LicenseField < Struct.new(:name, :description)
    class << self
      # Return a single license field
      #
      # key - string representing the field's text
      #
      # Returns a LicenseField
      def find(key)
        @all.find { |f| f.key == key }
      end

      # Returns an array of strings representing all field keys
      def keys
        @keys ||= LicenseField.all.map(&:key)
      end

      # Returns an array of all known LicenseFields
      def all
        @all ||= begin
          path   = '../../vendor/choosealicense.com/_data/fields.yml'
          path   = File.expand_path path, __dir__
          fields = YAML.safe_load_file(path)
          fields.map { |field| LicenseField.from_hash(field) }
        end
      end

      # Builds a LicenseField from a hash of properties
      def from_hash(hash)
        ordered_array = hash.values_at(*members.map(&:to_s))
        new(*ordered_array)
      end

      # Given an array of keys, returns an array of coresponding LicenseFields
      def from_array(array)
        array.map { |key| LicenseField.find(key) }
      end

      # Given a license body, returns an array of included LicneseFields
      def from_content(content)
        return [] unless content

        LicenseField.from_array content.scan(FIELD_REGEX).flatten
      end
    end

    alias key name
    FIELD_REGEX = /\[(#{Regexp.union(LicenseField.keys)})\]/

    # The human-readable field name
    def label
      key.sub('fullname', 'full name').capitalize
    end
    alias to_s label

    def raw_text
      "[#{key}]"
    end
  end
end
