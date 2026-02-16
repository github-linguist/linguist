# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'ruby-xxhash/version'

Gem::Specification.new do |spec|
  spec.name          = "ruby-xxHash"
  spec.version       = XXhash::VERSION
  spec.authors       = ["Justin W Smith"]
  spec.email         = ["justin.w.smith@gmail.com"]
  spec.description   = %q{A pure Ruby implementation of xxhash.}
  spec.summary       = %q{A pure Ruby implementation of xxhash.}
  spec.homepage      = "https://github.com/justinwsmith/ruby-xxhash"
  spec.license       = "MIT"

  spec.files         = `git ls-files`.split($/)
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "rspec"
end
