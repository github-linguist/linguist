module Mocha
  RUBY_V27_PLUS = Gem::Version.new(RUBY_VERSION.dup) >= Gem::Version.new('2.7')
  RUBY_V34_PLUS = Gem::Version.new(RUBY_VERSION.dup) >= Gem::Version.new('3.4')
end
