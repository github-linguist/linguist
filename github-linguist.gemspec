require File.expand_path('../lib/linguist/version', __FILE__)

Gem::Specification.new do |s|
  s.name    = 'github-linguist'
  s.version = Linguist::VERSION
  s.summary = "GitHub Language detection"
  s.description = 'We use this library at GitHub to detect blob languages, highlight code, ignore binary files, suppress generated files in diffs, and generate language breakdown graphs.'

  s.authors  = "GitHub"
  s.homepage = "https://github.com/github/linguist"
  s.license  = "MIT"

  s.files = Dir['lib/**/*']
  s.executables << 'linguist'

  s.add_dependency 'charlock_holmes', '~> 0.7.3'
  s.add_dependency 'escape_utils',    '~> 1.0.1'
  s.add_dependency 'mime-types',      '~> 1.19'
  s.add_dependency 'pygments.rb',     '~> 0.6.0'
  s.add_dependency 'rugged',          '~> 0.21.0'

  s.add_development_dependency 'git'
  s.add_development_dependency 'json'
  s.add_development_dependency 'mocha'
  s.add_development_dependency 'pry'
  s.add_development_dependency 'rake'
  s.add_development_dependency 'yajl-ruby'
end
