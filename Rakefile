require 'rake/clean'
require 'rake/testtask'

task :default => :test

Rake::TestTask.new do |t|
  t.warning = true
end


file 'lib/linguist/classifier.yml' => Dir['test/fixtures/**/*'] do |f|
  require 'linguist/sample'
  classifier = Linguist::Sample.classifier
  File.open(f.name, 'w') { |io| YAML.dump(classifier, io) }
end

CLOBBER.include 'lib/linguist/classifier.yml'

task :classifier => ['lib/linguist/classifier.yml']
