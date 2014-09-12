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
  benchmark_path = "benchmark/results"

  # $ rake benchmark:generate CORPUS=path/to/samples
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
    result_filename = "benchmark/results/#{File.basename(corpus)}-#{ref}.json"
    File.write(result_filename, results.to_json)
    puts "wrote #{result_filename}"
  end

  # $ rake benchmark:compare REFERENCE=path/to/reference.json CANDIDATE=path/to/candidate.json
  desc "Compare results"
  task :compare do
    reference_file = ENV["REFERENCE"]
    candidate_file = ENV["CANDIDATE"]

    reference = JSON.parse(File.read(reference_file))
    reference_counts = Hash.new(0)
    reference.each { |filename, language| reference_counts[language] += 1 }

    candidate = JSON.parse(File.read(candidate_file))
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


def diff(a, b)
  (a.keys | b.keys).each_with_object({}) do |key, diff|
    diff[key] = [a[key], b[key]] unless a[key] == b[key]
  end
end
