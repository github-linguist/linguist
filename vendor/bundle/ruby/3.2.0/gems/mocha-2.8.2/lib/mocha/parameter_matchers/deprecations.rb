require 'mocha/deprecation'
require 'set'

module Mocha
  module ParameterMatchers
    module Deprecations
      # @private
      def const_missing(name)
        if ParameterMatchers.access_deprecated?(name)
          mod = ParameterMatchers.const_get(name)
          Deprecation.warning(
            "Referencing #{name} outside its namespace is deprecated. Use fully-qualified #{mod} instead."
          )
          mod
        else
          super(name)
        end
      end
    end

    # @private
    @classes_with_access_deprecated = Set.new

    # @private
    def self.provide_deprecated_access_to(name)
      @classes_with_access_deprecated.add(name)
    end

    # @private
    def self.access_deprecated?(name)
      @classes_with_access_deprecated.include?(name)
    end

    # @private
    def self.define_deprecated_matcher_method(name)
      define_method(name) do |*args|
        Deprecation.warning(
          "Calling #{ParameterMatchers}##{name} is deprecated. Use #{Methods}##{name} instead."
        )
        Methods.instance_method(name).bind(self).call(*args)
      end
    end
  end
end
