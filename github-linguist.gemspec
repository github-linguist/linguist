require File.expand_path('../lib/linguist/version', __FILE__)

Gem::Specification.new do |s|
  s.name    = 'github-linguist'
  s.version = Linguist::VERSION
  s.summary = "GitHub Language detection"
  s.description = 'We use this library at GitHub to detect blob languages, highlight code, ignore binary files, suppress generated files in diffs, and generate language breakdown graphs.'

  s.authors  = "GitHub"
  s.homepage = "https://github.com/github/linguist"
  s.license  = "MIT"

  s.files = Dir['lib/**/*'] + Dir['grammars/*'] + ['LICENSE']
  s.executables = ['linguist', 'git-linguist']

  s.add_dependency 'charlock_holmes', '~> 0.7.3'
  s.add_dependency 'escape_utils',    '~> 1.1.0'
  s.add_dependency 'mime-types',      '>= 1.19'
  s.add_dependency 'rugged',          '>= 0.23.0b'

  s.add_development_dependency 'minitest', '>= 5.0'
  s.add_development_dependency 'mocha'
  s.add_development_dependency 'plist', '~>3.1'
  s.add_development_dependency 'pry'
  s.add_development_dependency 'rake'
  s.add_development_dependency 'yajl-ruby'
  s.add_development_dependency 'color-proximity', '~> 0.2.1'
  s.add_development_dependency 'licensed'
  s.add_development_dependency 'licensee', '>= 8.3.0'

end
