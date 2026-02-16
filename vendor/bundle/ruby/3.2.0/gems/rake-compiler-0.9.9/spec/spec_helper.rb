require 'rspec'

# Console redirection helper
require File.expand_path('../support/capture_output_helper', __FILE__)

RSpec.configure do |config|
  config.include CaptureOutputHelper
end

# Rake::Task matcher helper
RSpec::Matchers.define :have_defined do |task|
  match do |tasks|
    tasks.task_defined?(task)
  end
end
