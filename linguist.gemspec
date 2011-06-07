Gem::Specification.new do |s|
  s.name    = 'linguist'
  s.version = '0.0.10'
  s.summary = "GitHub Language detection"

  s.files = Dir['lib/**/*']
  s.executables << 'linguist'

  s.add_dependency 'albino',       '1.3.2'
  s.add_dependency 'escape_utils', '0.2.3'
  s.add_dependency 'mime-types',   '1.15'
  s.add_development_dependency 'rake'
end
