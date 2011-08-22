Gem::Specification.new do |s|
  s.name    = 'linguist'
  s.version = '1.0.0'
  s.summary = "GitHub Language detection"

  s.files = Dir['lib/**/*']
  s.executables << 'linguist'

  s.add_dependency 'escape_utils', '0.2.3'
  s.add_dependency 'mime-types',   '1.16'
  s.add_dependency 'pygments.rb'
  s.add_development_dependency 'rake'
end
