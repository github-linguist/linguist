require 'json'
require 'rake/clean'
require 'rake/testtask'
require 'yaml'
require 'pry'

task :default => :test

Rake::TestTask.new

task :samples do
  require 'linguist/samples'
  require 'yajl'
  data = Linguist::Samples.data
  json = Yajl::Encoder.encode(data, :pretty => true)
  File.open('lib/linguist/samples.json', 'w') { |io| io.write json }
end

task :build_gem do
  languages = YAML.load_file("lib/linguist/languages.yml")
  File.write("lib/linguist/languages.json", JSON.dump(languages))
  `gem build github-linguist.gemspec`
  File.delete("lib/linguist/languages.json")
end

# Want to do: rake benchmark:compare refs=20154eb04...6ed0a05b4
# If output for 20154eb04 or 6ed0a05b4 doesn't exist then should throw error
# Classification outputs for each commit need to be generated before the comparison can be done
# With something like: rake benchmark:generate ref=20154eb04

namespace :benchmark do
  require 'git'
  benchmark_path = "benchmark/results"

  git = Git.open('.')

  desc "Compare outputs"
  task :compare do
    reference, compare = ENV['refs'].split('...')
    puts "Comparing #{reference}...#{compare}"

    # Abort if there are uncommitted changes
    abort("Uncommitted changes -- aborting") if git.status.changed.any?

    [reference, compare].each do |ref|
      abort("No output file for #{ref}, run 'rake benchmark:generate ref=#{ref}'") unless File.exist?("#{benchmark_path}/#{ref}.json")
    end
  end

  desc "Generate classification summary for given ref"
  task :generate do
    ref = ENV['ref']
    abort("Must specify a commit ref, e.g. 'rake benchmark:generate ref=08819f82'") unless ref
    abort("Unstaged changes - aborting") if git.status.changed.any?

    # Get the current branch
    # Would like to get this from the Git gem
    current_branch = `git rev-parse --abbrev-ref HEAD`.strip

    puts "Checking out #{ref}"
    git.checkout(ref)

    # RUN BENCHMARK
    # Go through benchmark/samples/LANG dirs
    # For each Language

    Rake::Task["benchmark:index"].execute(:commit => ref)

    # Checkout original branch
    git.checkout(current_branch)
  end

  desc "Build benchmark index"
  task :index, [:commit] do |t, args|

    require 'linguist/language'
    results = Hash.new
    languages = Dir.glob('benchmark/samples/*')

    languages.each do |lang|
      puts ""
      puts "Starting with #{lang}"
      results[lang] = {}
      files = Dir.glob("#{lang}/*")
      files.each do |file|
        next unless File.file?(file)
        puts "  #{file}"

        blob = Linguist::FileBlob.new(file, Dir.pwd)
        result = blob.language

        filename = File.basename(file)
        if result.nil? # No results
          results[lang][filename] = "No language"
        else
          results[lang][filename] = result.name
        end
      end
    end

    File.open("benchmark/results/#{args[:commit]}.json", "w") {|f| f.write(results.to_json) }
  end

  desc "Compare results"
  task :results do
    # Deep diffing
    require './lib/linguist/diff'

    reference, compare = ENV['refs'].split('...')

    reference_classifications_file = "benchmark/results/#{reference}.json"
    compare_classifications_file = "benchmark/results/#{compare}.json"

    # DO COMPARISON...
    abort("No result files to compare") unless (File.exist?(reference_classifications_file) && File.exist?(compare_classifications_file))
    reference_classifications = JSON.parse(File.read(reference_classifications_file))
    compare_classifications = JSON.parse(File.read(compare_classifications_file))

    # Check if samples don't match current classification
    puts ""
    puts "Potential misclassifications for #{reference}"
    reference_classifications.each do |lang, files|
      language_name = lang.split('/').last

      files.each do |name, classification|
        # FIXME Don't want to report stuff from these dirs for now
        next if ['Binary', 'Text'].include?(language_name)
        unless classification == language_name
          puts "  #{name} is classified as #{classification} but #{language_name} was expected"
        end
      end
    end

    # Check if samples don't match current classification
    # TODO DRY this up.
    puts ""
    puts "Potential misclassifications for #{compare}"
    compare_classifications.each do |lang, files|
      language_name = lang.split('/').last

      files.each do |name, classification|
        # FIXME Don't want to report stuff from these dirs for now
        next if ['Binary', 'Text'].include?(language_name)
        unless classification == language_name
          puts "  #{name} is classified as #{classification} but #{language_name} was expected"
        end
      end
    end

    puts ""
    puts "Changes between #{reference}...#{compare}"
    changes = reference_classifications.deep_diff(compare_classifications)

    # Are there any differences in the linguist classification?
    if changes.any?
      changes.each do |lang, files|
        previous_count = reference_classifications[lang].size

        # Count the number of changed classifications (language and number)
        summary = changes[lang].inject(Hash.new(0)) do |result, (key, val)|
          new_lang = val.last
          result[new_lang] += 1
          result
        end

        puts "#{lang}"

        # Work out the percentage change
        summary.each do |new_lang, count|
          percent = count / previous_count.to_f
          puts "  #{sprintf("%.2f", percent)}% change to #{new_lang} (count files)"
        end
      end
    else
      puts "  No changes"
    end
  end
end

namespace :classifier do
  LIMIT = 1_000

  desc "Run classifier against #{LIMIT} public gists"
  task :test do
    require 'linguist/classifier'
    require 'linguist/samples'

    total, correct, incorrect = 0, 0, 0
    $stdout.sync = true

    each_public_gist do |gist_url, file_url, file_language|
      next if file_language.nil? || file_language == 'Text'
      begin
        data = open(file_url).read
        guessed_language, score = Linguist::Classifier.classify(Linguist::Samples::DATA, data).first

        total += 1
        guessed_language == file_language ? correct += 1 : incorrect += 1

        print "\r\e[0K%d:%d  %g%%" % [correct, incorrect, (correct.to_f/total.to_f)*100]
        $stdout.flush
      rescue URI::InvalidURIError
      else
        break if total >= LIMIT
      end
    end
    puts ""
  end

  def each_public_gist
    require 'open-uri'
    require 'json'

    url = "https://api.github.com/gists/public"

    loop do
      resp = open(url)
      url = resp.meta['link'][/<([^>]+)>; rel="next"/, 1]
      gists = JSON.parse(resp.read)

      for gist in gists
        for filename, attrs in gist['files']
          yield gist['url'], attrs['raw_url'], attrs['language']
        end
      end
    end
  end
end
