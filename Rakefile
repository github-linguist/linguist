require 'rake/clean'
require 'rake/testtask'

task :default => :test

Rake::TestTask.new do |t|
  t.warning = true
end

CLOBBER.include 'lib/linguist/lexers.yml'

file 'lib/linguist/lexers.yml' do |f|
  sh "./bin/pygments-lexers > #{f.name}"
end
