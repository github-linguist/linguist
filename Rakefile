require 'json'
require 'rake/clean'
require 'rake/testtask'
require 'yaml'

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
