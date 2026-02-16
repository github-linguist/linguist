module Mocha
  module Debug
    OPTIONS = (ENV['MOCHA_OPTIONS'] || '').split(',').freeze

    def self.puts(message)
      warn(message) if OPTIONS.include?('debug')
    end
  end
end
