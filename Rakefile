require 'bundler/setup'
require 'rake/clean'
require 'rake/testtask'
require 'yaml'
require 'yajl'
require 'open-uri'

task :default => :test

Rake::TestTask.new

# Extend test task to check for samples and fetch latest Ace modes
task :test => [:check_samples, :fetch_ace_modes]

desc "Check that we have samples.json generated"
task :check_samples do
  unless File.exist?('lib/linguist/samples.json')
    Rake::Task[:samples].invoke
  end
end

desc "Fetch the latest Ace modes from its GitHub repository"
task :fetch_ace_modes do
  ACE_FIXTURE_PATH = File.join('test', 'fixtures', 'ace_modes.json')

  File.delete(ACE_FIXTURE_PATH) if File.exist?(ACE_FIXTURE_PATH)

  begin
    ace_github_modes = open("https://api.github.com/repos/ajaxorg/ace/contents/lib/ace/mode").read
    File.write(ACE_FIXTURE_PATH, ace_github_modes)
  rescue OpenURI::HTTPError, SocketError
      # no internet? no problem.
  end
end

task :samples do
  require 'linguist/samples'
  json = Yajl.dump(Linguist::Samples.data, :pretty => true)
  File.write 'lib/linguist/samples.json', json
end

task :build_gem => :samples do
  languages = YAML.load_file("lib/linguist/languages.yml")
  File.write("lib/linguist/languages.json", Yajl.dump(languages))
  `gem build github-linguist.gemspec`
  File.delete("lib/linguist/languages.json")
end

task :build_grammars_gem do
  rm_rf "grammars"
  sh "script/convert-grammars"
  sh "gem", "build", "github-linguist-grammars.gemspec"
end

namespace :benchmark do
  benchmark_path = "benchmark/results"

  # $ bundle exec rake benchmark:generate CORPUS=path/to/samples
  desc "Generate results for"
  task :generate do
    ref = `git rev-parse HEAD`.strip[0,8]

    corpus = File.expand_path(ENV["CORPUS"] || "samples")

    require 'linguist/language'

    results = Hash.new
    Dir.glob("#{corpus}/**/*").each do |file|
      next unless File.file?(file)
      filename = file.gsub("#{corpus}/", "")
      results[filename] = Linguist::FileBlob.new(file).language
    end

    # Ensure results directory exists
    FileUtils.mkdir_p("benchmark/results")

    # Write results
    if `git status`.include?('working directory clean')
      result_filename = "benchmark/results/#{File.basename(corpus)}-#{ref}.json"
    else
      result_filename = "benchmark/results/#{File.basename(corpus)}-#{ref}-unstaged.json"
    end

    File.write(result_filename, results.to_json)
    puts "wrote #{result_filename}"
  end

  # $ bundle exec rake benchmark:compare REFERENCE=path/to/reference.json CANDIDATE=path/to/candidate.json
  desc "Compare results"
  task :compare do
    reference_file = ENV["REFERENCE"]
    candidate_file = ENV["CANDIDATE"]

    reference = Yajl.load(File.read(reference_file))
    reference_counts = Hash.new(0)
    reference.each { |filename, language| reference_counts[language] += 1 }

    candidate = Yajl.load(File.read(candidate_file))
    candidate_counts = Hash.new(0)
    candidate.each { |filename, language| candidate_counts[language] += 1 }

    changes = diff(reference_counts, candidate_counts)

    if changes.any?
      changes.each do |language, (before, after)|
        before_percent = 100 * before / reference.size.to_f
        after_percent = 100 * after / candidate.size.to_f
        puts "%s changed from %.1f%% to %.1f%%" % [language || 'unknown', before_percent, after_percent]
      end
    else
      puts "No changes"
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
        guessed_language, score = Linguist::Classifier.classify(Linguist::Samples.cache, data).first

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
    url = "https://api.github.com/gists/public"

    loop do
      resp = open(url)
      url = resp.meta['link'][/<([^>]+)>; rel="next"/, 1]
      gists = Yajl.load(resp.read)

      for gist in gists
        for filename, attrs in gist['files']
          yield gist['url'], attrs['raw_url'], attrs['language']
        end
      end
    end
  end
end


def diff(a, b)
  (a.keys | b.keys).each_with_object({}) do |key, diff|
    diff[key] = [a[key], b[key]] unless a[key] == b[key]
  end
end
