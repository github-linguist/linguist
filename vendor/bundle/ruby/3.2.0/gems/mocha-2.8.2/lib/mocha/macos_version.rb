module Mocha
  MACOS = /darwin/.match(RUBY_PLATFORM)
  MACOS_VERSION = MACOS && /darwin(\d+)/.match(RUBY_PLATFORM)[1].to_i
  MACOS_MOJAVE_VERSION = 18
end
