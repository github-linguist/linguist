require File.expand_path('../lib/linguist/version', __FILE__)

Gem::Specification.new do |s|
  s.name    = 'github-linguist-grammars'
  s.version = Linguist::VERSION
  s.summary = "Language grammars for use with github-linguist"

  s.authors  = "GitHub"
  s.homepage = "https://github.com/github/linguist"

  s.files = ['lib/linguist/grammars.rb'] + Dir['grammars/*']

  s.add_development_dependency 'plist', '~>3.1'
end
