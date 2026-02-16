require 'mocha/debug'
require 'mocha/detection/test_unit'
require 'mocha/integration/test_unit/adapter'

module Mocha
  module Integration
    module TestUnit
      def self.activate
        target = Detection::TestUnit.testcase
        return false unless target

        test_unit_version = Gem::Version.new(Detection::TestUnit.version)
        Debug.puts "Detected Test::Unit version: #{test_unit_version}"

        unless TestUnit::Adapter.applicable_to?(test_unit_version)
          raise 'Versions of test-unit earlier than v2.5.1 are not supported.'
        end

        unless target < TestUnit::Adapter
          Debug.puts "Applying #{TestUnit::Adapter.description}"
          target.send(:include, TestUnit::Adapter)
        end

        true
      end
    end
  end
end
