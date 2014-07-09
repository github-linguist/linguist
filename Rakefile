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

namespace :benchmark do
  require 'git'
  require 'linguist/language'
  require 'linguist/diff'
  require 'json'

  git = Git.open('.')

  desc "Testin'"
  task :run do
    reference, compare = ENV['compare'].split('...')
    puts "Comparing #{reference}...#{compare}"
    abort("Unstaged changes - aborting") if git.status.changed.any?

    # Get the current branch
    # Would like to get this from the Git gem
    current_branch = `git rev-parse --abbrev-ref HEAD`.strip

    # Create tmp branch for reference commit
    puts "Creating branch tmp_#{reference}"
    git.branch("tmp_#{reference}").checkout
    git.reset_hard(reference)

    # RUN BENCHMARK
    # Go through benchmark/samples/LANG dirs
    # For each Language

    Rake::Task["benchmark:index"].execute(:commit => reference)

    # Create tmp branch for compare commit
    puts ""
    puts "Creating temporary branch tmp_#{compare}"
    git.branch("tmp_#{compare}").checkout
    git.reset_hard(compare)

    # RUN BENCHMARK AGAIN
    Rake::Task["benchmark:index"].execute(:commit => compare)

    git.branch(current_branch).checkout

    # CLEAN UP
    git.branch("tmp_#{reference}").delete
    git.branch("tmp_#{compare}").delete

    # COMPARE AND PRINT RESULTS
    Rake::Task["benchmark:results"].execute
  end

  desc "Build benchmark index"
  task :index, [:commit] do |t, args|
    require 'shellwords'

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
        result = IO::popen("bundle exec linguist #{Shellwords.escape(file)} --simple").read
        filename = File.basename(file)
        if result.chomp.empty? # No results
          results[lang][filename] = "No language"
        else
          results[lang][filename] = result.chomp
        end
      end
    end

    File.open("benchmark/results/#{args[:commit]}_output.json", "w") {|f| f.write(results.to_json) }
  end

  desc "Compare results"
  task :results do
    reference, compare = ENV['compare'].split('...')

    reference_classifications_file = "benchmark/results/#{reference}_output.json"
    compare_classifications_file = "benchmark/results/#{compare}_output.json"

    # DO COMPARISON...
    abort("No result files to compare") unless (File.exist?(reference_classifications_file) && File.exist?(compare_classifications_file))
    reference_classifications = JSON.parse(File.read(reference_classifications_file))
    compare_classifications = JSON.parse(File.read(compare_classifications_file))

    puts "Changes between #{reference}...#{compare}"
    puts reference_classifications.deep_diff(compare_classifications)
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
