module Mocha
  class Central
    class Null < self
      def initialize(&block)
        super
        @raise_not_initialized_error = block
      end

      def stub(*)
        @raise_not_initialized_error.call
      end

      def unstub(*)
        @raise_not_initialized_error.call
      end
    end

    attr_accessor :stubba_methods

    def initialize
      self.stubba_methods = []
    end

    def stub(method)
      return if stubba_methods.detect { |m| m.matches?(method) }
      method.stub
      stubba_methods.push(method)
    end

    def unstub(method)
      return unless (existing = stubba_methods.detect { |m| m.matches?(method) })
      existing.unstub
      stubba_methods.delete(existing)
    end

    def unstub_all
      while stubba_methods.any?
        unstub(stubba_methods.first)
      end
    end
  end
end
