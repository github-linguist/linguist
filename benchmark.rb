require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  # ruby '3.1.2'

  gem 'github-linguist', path: '.'
  gem 'ruby-prof'
  gem 'ruby-prof-speedscope'
end

require 'linguist'
require 'rugged'

repository = Rugged::Repository.new("../github")
counter = Linguist::BlobCounter.new(repository)

RubyProf.start
  counter.count
results = RubyProf.stop

# Save the printer output.
File.open("trace.rubyprof", "w+") do |f|
  RubyProf::SpeedscopePrinter.new(results).print(f)
end