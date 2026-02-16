require 'mocha/api'

module Mocha
  module Integration
    module MonkeyPatcher
      def self.apply(mod, run_method_patch)
        if mod < Mocha::API
          Debug.puts "Mocha::API already included in #{mod}"
        else
          mod.send(:include, Mocha::API)
        end
        if mod.method_defined?(:run_before_mocha)
          Debug.puts "#{mod}#run_before_mocha method already defined"
        elsif mod.method_defined?(:run)
          mod.send(:alias_method, :run_before_mocha, :run)
          mod.send(:remove_method, :run)
          mod.send(:include, run_method_patch)
        else
          raise "Unable to monkey-patch #{mod}, because it does not define a `#run` method"
        end
      end
    end
  end
end
