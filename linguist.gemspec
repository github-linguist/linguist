Gem::Specification.new do |s|
  s.name    = 'linguist'
  s.version = '1.0.0'
  s.summary = "GitHub Language detection"

  s.authors = "GitHub"

  s.files = Dir['lib/**/*']
  s.executables << 'linguist'

  s.add_dependency 'charlock_holmes', '~> 0.6.6'
  s.add_dependency 'escape_utils',    '~> 0.2.3'
  s.add_dependency 'mime-types',      '~> 1.18'
  s.add_dependency 'pygments.rb',     '~> 0.2.11'
  s.add_development_dependency 'rake'
end
