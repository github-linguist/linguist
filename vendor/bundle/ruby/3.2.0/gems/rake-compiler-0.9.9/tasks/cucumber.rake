begin
  require 'cucumber/rake/task'
rescue LoadError
  warn "Cucumber gem is required, please install it. (gem install cucumber)"
end

if defined?(Cucumber)
  namespace :cucumber do
    Cucumber::Rake::Task.new('default', 'Run features testing C extension support') do |t|
      t.profile       = 'default'
      t.cucumber_opts = '--format pretty --no-source'
    end
    Cucumber::Rake::Task.new('java', 'Run features testing Java extension support') do |t|
      t.profile       = 'java'
      t.cucumber_opts = '--format pretty --no-source'
    end

    desc 'Run all features'
    task :all => [:default, :java]
  end
  desc 'Alias for cucumber:default'
  task :cucumber => 'cucumber:default'
end
