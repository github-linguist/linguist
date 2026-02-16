begin
  require "rspec/core/rake_task"
rescue LoadError => e
  warn "RSpec gem is required, please install it (gem install rspec)."
end

if defined?(RSpec::Core::RakeTask)
  RSpec::Core::RakeTask.new(:spec)
end
