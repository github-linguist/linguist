# frozen_string_literal: true
require "bundler/gem_tasks"
require "rake/testtask"
require "rubocop/rake_task"
require "licensed"

desc "Run source setup scripts"
task :setup, [:arguments] do |task, args|
  arguments = args[:arguments].to_s.split
  force = arguments.include?("-f") ? "-f" : ""

  Dir["script/source-setup/**/*"].each do |script|
    next if File.directory?(script)

    # green
    puts "\033[32mRunning #{script}.\e[0m"

    if Bundler.with_clean_env { system(script, force) }
      # green
      puts "\033[32mCompleted #{script}.\e[0m"
    elsif $?.exitstatus == 127
      # yellow
      puts "\033[33mSkipped #{script}.\e[0m"
    else
      # red
      puts "\033[31mEncountered an error running #{script}.\e[0m"
    end

    puts
  end
end

sources = Licensed::Sources::Source.sources.map { |source| source.full_type }

namespace :test do
  sources.each do |source|
    # hidden task to set ENV and filter tests to the given source
    # see `each_source` in test/test_helper.rb
    namespace source.to_sym do
      task :env do
        ENV["SOURCE"] = source
      end
    end

    Rake::TestTask.new(source => "test:#{source}:env") do |t|
      t.description = "Run #{source} tests"
      t.libs << "test"
      t.libs << "lib"
      t.test_files = FileList["test/commands/*_test.rb", "test/sources/#{source}_test.rb"]
    end
  end

  namespace :core do
    task :env do
      ENV["SOURCE"] = ""
    end
  end

  Rake::TestTask.new(core: "test:core:env") do |t|
    t.description = "Run non-source tests"
    t.libs << "test"
    t.libs << "lib"
    t.test_files = FileList["test/**/*_test.rb"].exclude("test/fixtures/**/*_test.rb")
                                                .exclude("test/sources/*_test.rb")
                                                .exclude("test/sources/**/*_test.rb")
  end
end

Rake::TestTask.new(:test) do |t|
  t.libs << "test"
  t.libs << "lib"
  t.test_files = FileList["test/**/*_test.rb"].exclude("test/fixtures/**/*_test.rb")
end

# add rubocop task
# -S adds styleguide urls to offense messages
RuboCop::RakeTask.new do |t|
  t.options.push "-S"
end

task default: :test
