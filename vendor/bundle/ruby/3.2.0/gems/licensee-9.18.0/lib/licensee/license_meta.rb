# frozen_string_literal: true

module Licensee
  class LicenseMeta < Struct.new(
    :title, :spdx_id, :source, :description, :how, :conditions, :permissions,
    :limitations, :using, :featured, :hidden, :nickname, :note
  )

    # These should be in sync with choosealicense.com's collection defaults
    DEFAULTS = {
      'featured' => false,
      'hidden'   => true
    }.freeze

    PREDICATE_FIELDS = %i[featured hidden].freeze

    include Licensee::HashHelper
    HASH_METHODS = members - %i[conditions permissions limitations spdx_id]

    class << self
      # Create a new LicenseMeta from YAML
      #
      # yaml - the raw YAML string
      #
      # returns a LicenseMeta with defaults set
      def from_yaml(yaml)
        return from_hash({}) if yaml.nil? || yaml.to_s.empty?

        from_hash YAML.safe_load(yaml)
      end

      # Create a new LicenseMeta from a hash
      #
      # hash - the hash of key/value meta pairs
      #
      # returns a LicenseMeta with defaults set
      def from_hash(hash)
        hash = DEFAULTS.merge(hash)
        hash['spdx_id'] = hash.delete('spdx-id')
        ordered_array = hash.values_at(*members.map(&:to_s))
        new(*ordered_array)
      end

      # Array of symbolized helper methods to expose on the License class
      def helper_methods
        members - PREDICATE_FIELDS + PREDICATE_FIELDS.map { |f| :"#{f}?" }
      end
    end

    PREDICATE_FIELDS.each do |field|
      alias_method :"#{field}?", field
    end

    # Backward compatibalize `#["spdx-id"]` calls to avoid a breaking change
    def [](key)
      key = 'spdx_id' if key == 'spdx-id'
      super
    end

    def source
      "https://spdx.org/licenses/#{spdx_id}.html" if spdx_id
    end
  end
end
