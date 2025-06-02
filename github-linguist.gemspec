require File.expand_path('../lib/linguist/version', __FILE__)

Gem::Specification.new do |s|
  s.name    = 'github-linguist'
  s.version = ENV['GEM_VERSION'] || Linguist::VERSION
  s.summary = "GitHub Language detection"
  s.description = 'We use this library at GitHub to detect blob languages, highlight code, ignore binary files, suppress generated files in diffs, and generate language breakdown graphs.'

  s.authors  = "GitHub"
  s.homepage = "https://github.com/github-linguist/linguist"
  s.license  = "MIT"
  s.metadata = {
    "github_repo" => "ssh://github.com/github-linguist/linguist"
  }

  s.files = Dir['{lib,ext}/**/*', 'grammars/*', 'LICENSE'] - Dir['lib/linguist/linguist.{so,bundle}']
  s.platform = Gem::Platform::RUBY
  s.executables = ['github-linguist', 'git-linguist']
  s.extensions = ['ext/linguist/extconf.rb']
  s.require_paths = ['lib', 'ext']

  s.add_dependency 'cgi',             '>= 0'
  s.add_dependency 'charlock_holmes', '~> 0.7.7'
  s.add_dependency 'mini_mime',       '~> 1.0'
  s.add_dependency 'rugged',          '~> 1.0'

  s.add_development_dependency 'minitest', '~> 5.15'
  s.add_development_dependency 'rake-compiler', '~> 0.9'
  s.add_development_dependency 'mocha', '~> 2.1'
  s.add_development_dependency 'plist', '~>3.1'
  s.add_development_dependency 'pry', '~> 0.14'
  s.add_development_dependency 'rake', '~> 13.0'
  s.add_development_dependency 'yajl-ruby', '~> 1.4'
  s.add_development_dependency 'licensed', '~> 5.0'
  s.add_development_dependency 'licensee', '~> 9.15'
  s.add_development_dependency 'bundler', '~> 2.0'
end
