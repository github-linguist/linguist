Gem::Specification.new do |s|
  s.name    = 'github-linguist'
  s.version = '2.3.4'
  s.summary = "GitHub Language detection"

  s.authors = "GitHub"

  s.files = Dir['lib/**/*']
  s.executables << 'linguist'

  s.add_dependency 'charlock_holmes', '~> 0.6.6'
  s.add_dependency 'escape_utils',    '~> 0.2.3'
  s.add_dependency 'mime-types',      '~> 1.19'
  s.add_dependency 'pygments.rb',     '>= 0.2.13'
  s.add_development_dependency 'mocha'
  s.add_development_dependency 'json'
  s.add_development_dependency 'rake'
  s.add_development_dependency 'yajl-ruby'
end
