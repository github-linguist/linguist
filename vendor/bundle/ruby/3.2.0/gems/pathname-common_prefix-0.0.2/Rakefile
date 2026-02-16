require 'rake/testtask'
require 'rubygems/tasks'
require "steep"

task :default => [:test, :typecheck]

Rake::TestTask.new
Gem::Tasks.new

desc "Check type"
task :typecheck do
  Steep::Drivers::Check.new(stdout: $stdout, stderr: $stderr).run
end
