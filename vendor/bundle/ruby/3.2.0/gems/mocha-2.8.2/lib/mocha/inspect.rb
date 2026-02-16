require 'date'

module Mocha
  module Inspect
    module ObjectMethods
      def mocha_inspect
        address = __id__ * 2
        address += 0x100000000 if address < 0
        inspect =~ /#</ ? "#<#{self.class}:0x#{Kernel.format('%<address>x', address: address)}>" : inspect
      end
    end

    module ArrayMethods
      def mocha_inspect(wrapped = true)
        unwrapped = collect(&:mocha_inspect).join(', ')
        wrapped ? "[#{unwrapped}]" : unwrapped
      end
    end

    module HashMethods
      def mocha_inspect
        if Hash.ruby2_keywords_hash?(self)
          collect do |key, value|
            case key
            when Symbol
              "#{key}: #{value.mocha_inspect}"
            else
              "#{key.mocha_inspect} => #{value.mocha_inspect}"
            end
          end.join(', ')
        else
          unwrapped = collect { |key, value| "#{key.mocha_inspect} => #{value.mocha_inspect}" }.join(', ')
          "{#{unwrapped}}"
        end
      end
    end

    module TimeMethods
      def mocha_inspect
        "#{inspect} (#{to_f} secs)"
      end
    end

    module DateMethods
      def mocha_inspect
        to_s
      end
    end
  end
end

class Object
  include Mocha::Inspect::ObjectMethods
end

class Array
  include Mocha::Inspect::ArrayMethods
end

class Hash
  include Mocha::Inspect::HashMethods
end

class Time
  include Mocha::Inspect::TimeMethods
end

class Date
  include Mocha::Inspect::DateMethods
end
