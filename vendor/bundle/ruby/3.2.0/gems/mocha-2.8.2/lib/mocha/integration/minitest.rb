require 'mocha/debug'
require 'mocha/detection/minitest'
require 'mocha/integration/minitest/adapter'

module Mocha
  module Integration
    module Minitest
      def self.activate
        target = Detection::Minitest.testcase
        return false unless target

        minitest_version = Gem::Version.new(Detection::Minitest.version)
        Debug.puts "Detected Minitest version: #{minitest_version}"

        unless Minitest::Adapter.applicable_to?(minitest_version)
          raise 'Versions of minitest earlier than v3.3.0 are not supported.'
        end

        unless target < Minitest::Adapter
          Debug.puts "Applying #{Minitest::Adapter.description}"
          target.send(:include, Minitest::Adapter)
        end

        true
      end
    end
  end
end
