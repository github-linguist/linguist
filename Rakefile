require 'rake/clean'
require 'rake/testtask'

task :default => :test

Rake::TestTask.new do |t|
  t.warning = true
end


file 'lib/linguist/samples.yml' => Dir['samples/**/*'] do |f|
  require 'linguist/sample'
  File.open(f.name, 'w') { |io| Linguist::Sample.serialize_to_yaml(Linguist::Sample::DATA, io) }
end

CLOBBER.include 'lib/linguist/samples.yml'

task :classifier => [:clobber, 'lib/linguist/samples.yml']

namespace :classifier do
  LIMIT = 1_000

  desc "Run classifier against #{LIMIT} public gists"
  task :test do
    require 'linguist/classifier'

    total, correct, incorrect = 0, 0, 0
    $stdout.sync = true

    each_public_gist do |gist_url, file_url, file_language|
      next if file_language.nil? || file_language == 'Text'
      begin
        data = open(file_url).read
        guessed_language, score = Linguist::Classifier.new(Sample::DATA).classify(data).first

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
