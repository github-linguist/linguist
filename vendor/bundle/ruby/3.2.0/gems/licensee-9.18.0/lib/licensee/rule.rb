# frozen_string_literal: true

module Licensee
  class Rule
    attr_reader :tag, :label, :description, :group

    include Licensee::HashHelper
    HASH_METHODS = %i[tag label description].freeze

    def initialize(tag: nil, label: nil, description: nil, group: nil)
      @tag = tag
      @label = label
      @description = description
      @group = group
    end

    def inspect
      "#<Licensee::Rule @tag=\"#{tag}\">"
    end

    class << self
      def all
        @all ||= raw_rules.map do |group, rules|
          rules.map do |rule|
            Rule.new(
              tag:         rule['tag'],
              label:       rule['label'],
              description: rule['description'],
              group:       group
            )
          end
        end.flatten
      end

      def find_by_tag_and_group(tag, group = nil)
        Rule.all.find { |r| r.tag == tag && (group.nil? || r.group == group) }
      end
      alias find_by_tag find_by_tag_and_group

      def file_path
        dir = File.dirname(__FILE__)
        File.expand_path '../../vendor/choosealicense.com/_data/rules.yml', dir
      end

      def raw_rules
        YAML.safe_load_file(Rule.file_path)
      end

      def groups
        Rule.raw_rules.keys
      end
    end
  end
end
