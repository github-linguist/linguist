# frozen_string_literal: true

require 'bundler'
Bundler::GemHelper.install_tasks

task test: :spec
task default: :spec

desc 'Run RSpec'
task :spec do
  if Process.respond_to?(:fork)
    sh('rspec-queue')
  else
    sh('rspec')
  end
end

namespace :doc do
  require 'yard'
  YARD::Rake::YardocTask.new do |task|
    task.files   = ['README.md', 'LICENSE.md', 'lib/**/*.rb']
    task.options = [
      '--output-dir', 'doc/yard',
      '--markup', 'markdown'
    ]
  end
rescue LoadError
end
