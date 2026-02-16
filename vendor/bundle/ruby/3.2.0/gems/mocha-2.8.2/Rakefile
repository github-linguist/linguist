require 'bundler'
namespace 'rubygems' do
  Bundler::GemHelper.install_tasks
end
require 'bundler/setup'

require 'rake/testtask'
begin
  # Only available with default Gemfile and in Ruby >= v2.2
  require 'rubocop/rake_task'
rescue LoadError # rubocop:disable Lint/HandleExceptions
end

desc 'Run all linters and tests'
task 'default' => ['lint', 'test', 'test:performance']

desc 'Run tests'
task 'test' do
  if (test_library = ENV['MOCHA_RUN_INTEGRATION_TESTS'])
    Rake::Task["test:integration:#{test_library}"].invoke
  else
    Rake::Task['test:units'].invoke
    Rake::Task['test:acceptance'].invoke
  end
end

namespace 'test' do # rubocop:disable Metrics/BlockLength
  desc 'Run unit tests'
  Rake::TestTask.new('units') do |t|
    t.libs << 'test'
    t.test_files = FileList['test/unit/**/*_test.rb']
    t.verbose = true
    t.warning = true
  end

  desc 'Run acceptance tests'
  Rake::TestTask.new('acceptance') do |t|
    t.libs << 'test'
    t.test_files = FileList['test/acceptance/*_test.rb']
    t.verbose = true
    t.warning = true
  end

  namespace 'integration' do
    desc 'Run Minitest integration tests (intended to be run in its own process)'
    Rake::TestTask.new('minitest') do |t|
      t.libs << 'test'
      t.test_files = FileList['test/integration/minitest_test.rb']
      t.verbose = true
      t.warning = true
    end

    desc 'Run Test::Unit integration tests (intended to be run in its own process)'
    Rake::TestTask.new('test-unit') do |t|
      t.libs << 'test'
      t.test_files = FileList['test/integration/test_unit_test.rb']
      t.verbose = true
      t.warning = true
    end
  end

  # require 'rcov/rcovtask'
  # Rcov::RcovTask.new('coverage') do |t|
  #   t.libs << 'test'
  #   t.test_files = unit_tests + acceptance_tests
  #   t.verbose = true
  #   t.warning = true
  #   t.rcov_opts << '--sort coverage'
  #   t.rcov_opts << '--xref'
  # end

  desc 'Run performance tests'
  task 'performance' do
    require File.join(File.dirname(__FILE__), 'test', 'acceptance', 'stubba_example_test')
    require File.join(File.dirname(__FILE__), 'test', 'acceptance', 'mocha_example_test')
    iterations = 1000
    puts "\nBenchmarking with #{iterations} iterations..."
    [MochaExampleTest, StubbaExampleTest].each do |test_case|
      puts "#{test_case}: #{benchmark_test_case(test_case, iterations)} seconds."
    end
  end
end

task 'lint' do
  if defined?(RuboCop::RakeTask)
    RuboCop::RakeTask.new
    Rake::Task['rubocop'].invoke
  else
    puts 'RuboCop not available - skipping linting'
  end
end

# rubocop:disable Metrics/CyclomaticComplexity,Metrics/PerceivedComplexity
def benchmark_test_case(klass, iterations)
  require 'benchmark'
  require 'mocha/detection/minitest'

  if defined?(Minitest)
    minitest_version = Gem::Version.new(Mocha::Detection::Minitest.version)
    if Gem::Requirement.new('>= 5.0.0').satisfied_by?(minitest_version)
      Minitest.seed = 1
      result = Benchmark.realtime { iterations.times { |_i| klass.run(Minitest::CompositeReporter.new) } }
      Minitest::Runnable.runnables.delete(klass)
      result
    else
      Minitest::Unit.output = StringIO.new
      Benchmark.realtime { iterations.times { |_i| Minitest::Unit.new.run([klass]) } }
    end
  else
    load 'test/unit/ui/console/testrunner.rb' unless defined?(Test::Unit::UI::Console::TestRunner)
    unless @silent_option
      begin
        load 'test/unit/ui/console/outputlevel.rb' unless defined?(Test::Unit::UI::Console::OutputLevel::SILENT)
        @silent_option = { output_level: Test::Unit::UI::Console::OutputLevel::SILENT }
      rescue LoadError
        @silent_option = Test::Unit::UI::SILENT
      end
    end
    Benchmark.realtime { iterations.times { Test::Unit::UI::Console::TestRunner.run(klass, @silent_option) } }
  end
end
# rubocop:enable Metrics/CyclomaticComplexity,Metrics/PerceivedComplexity

if ENV['MOCHA_GENERATE_DOCS']
  require 'yard'

  desc 'Remove generated documentation'
  task 'clobber_yardoc' do
    `rm -rf ./docs`
  end

  desc 'Generate documentation'
  YARD::Rake::YardocTask.new('yardoc') do |task|
    task.options = ['--title', "Mocha #{Mocha::VERSION}", '--fail-on-warning']
  end

  task 'checkout_docs_cname' do
    `git checkout docs/CNAME`
  end

  task 'checkout_docs_js' do
    `git checkout docs/js/app.js`
    `git checkout docs/js/jquery.js`
  end

  desc 'Generate documentation'
  task 'generate_docs' => %w[clobber_yardoc yardoc checkout_docs_cname checkout_docs_js]
end

task 'release' => ['default', 'rubygems:release']
