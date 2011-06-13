require 'rake/clean'
require 'rake/testtask'

task :default => :test

Rake::TestTask.new do |t|
  t.warning = true
end

CLOBBER.include 'lib/linguist/lexers.yml'

file 'lib/linguist/lexers.yml' do |f|
  # GitHub vendored pygments path
  #   vendor/python/pygments
  path = File.expand_path('../../../python/pygments', __FILE__)
  ENV['PYTHONPATH'] = path if File.directory?(path)

  sh "python ./bin/pygments-lexers > #{f.name}"
end

task :lexers => 'lib/linguist/lexers.yml'
