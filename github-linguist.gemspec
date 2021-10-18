require File.expand_path('../lib/linguist/version', __FILE__)

Gem::Specification.new do |s|
  s.name    = 'github-linguist'
  s.version = ENV['GEM_VERSION'] || Linguist::VERSION
  s.summary = "GitHub Language detection"
  s.description = 'We use this library at GitHub to detect blob languages, highlight code, ignore binary files, suppress generated files in diffs, and generate language breakdown graphs.'

  s.authors  = "GitHub"
  s.homepage = "https://github.com/github/linguist"
  s.license  = "MIT"
  s.metadata = {
    "github_repo" => "ssh://github.com/github/linguist"
  }

  s.files = Dir['lib/**/*'] + Dir['ext/**/*'] + Dir['grammars/*'] + ['LICENSE']
  s.executables = ['github-linguist', 'git-linguist']
  s.extensions = ['ext/linguist/extconf.rb']
  s.require_paths = ['lib', 'ext']

  s.add_dependency 'charlock_holmes', '~> 0.7.7'
  s.add_dependency 'escape_utils',    '~> 1.2.0'
  s.add_dependency 'mini_mime',       '~> 1.0'
  s.add_dependency 'rugged',          '>= 0.25.1'

  s.add_development_dependency 'minitest', '>= 5.14.2'
  s.add_development_dependency 'rake-compiler', '~> 0.9'
  s.add_development_dependency 'mocha'
  s.add_development_dependency 'plist', '~>3.1'
  s.add_development_dependency 'pry'
  s.add_development_dependency 'rake'
  s.add_development_dependency 'yajl-ruby'
  s.add_development_dependency 'licensed', '~> 2.0'
  s.add_development_dependency 'licensee'
  s.add_development_dependency 'bundler', '>= 1.10'
end
