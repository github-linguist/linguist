# frozen_string_literal: true

module Licensee
  module HashHelper
    def to_h
      hash = {}
      self.class::HASH_METHODS.each do |method|
        key = method.to_s.delete('?').to_sym
        value = public_send(method)
        hash[key] = if value.is_a?(Array)
                      value.map { |v| v.respond_to?(:to_h) ? v.to_h : v }
                    elsif value.respond_to?(:to_h) && !value.nil?
                      value.to_h
                    else
                      value
                    end
      end

      hash
    end
  end
end
