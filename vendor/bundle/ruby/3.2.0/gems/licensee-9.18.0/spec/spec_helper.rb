# frozen_string_literal: true

require 'simplecov'
SimpleCov.start

require 'licensee'
require 'open3'
require 'tmpdir'
require 'mustache'
require 'yaml'

require 'webmock/rspec'
WebMock.disable_net_connect!

RSpec.configure do |config|
  config.shared_context_metadata_behavior = :apply_to_host_groups
  config.example_status_persistence_file_path = 'spec/examples.txt'
  config.disable_monkey_patching!
  config.default_formatter = 'doc' if config.files_to_run.one?
  config.order = :random
  Kernel.srand config.seed
end

def project_root
  File.expand_path '../', File.dirname(__FILE__)
end

def fixtures_base
  File.expand_path 'spec/fixtures', project_root
end

def fixtures
  @fixtures ||= begin
    dirs = Dir["#{fixtures_base}/*"].select { |e| File.directory?(e) }
    dirs.map { |path| File.basename(path) }.sort_by { |k, _v| k }
  end
end

def fixture_path(fixture)
  File.expand_path fixture, fixtures_base
end

def fixture_contents(fixture)
  File.read fixture_path(fixture)
end

def fixture_root_files(fixture)
  Dir["#{fixture_path(fixture)}/*"]
end

def fixture_root_contents_from_api(fixture)
  fixture_root_files(fixture).map do |file|
    {
      name: File.basename(file),
      type: 'file',
      path: File.basename(file)
    }
  end.to_json
end

def fixture_licenses
  @fixture_licenses ||= YAML.load_file(fixture_path('fixtures.yml'))
end

def field_values
  {
    fullname:    'Ben Balter',
    year:        '2018',
    email:       'ben@github.invalid',
    projecturl:  'http://github.invalid/benbalter/licensee',
    login:       'benbalter',
    project:     'Licensee',
    description: 'Detects licenses'
  }
end

def sub_copyright_info(license)
  Mustache.render license.content_for_mustache, field_values
end

# Add random words to the end of a license to test similarity tollerances
def add_random_words(string, count = 5)
  words = string.dup.split
  ipsum = File.read(fixture_path('ipsum.txt')).split
  count.times do
    word = ipsum[Random.rand(ipsum.length)]
    index = Random.rand(words.length)
    words.insert(index, word)
  end
  words.join(' ')
end

# Init git dir
# NOTE: we disable gpgsign and restore it to its original setting to avoid
# Signing commits during tests and slowing down / breaking specs
def git_init(path)
  Dir.chdir path do
    `git init`
    `git config --local commit.gpgsign false`
    `git add .`
    `git commit -m 'initial commit'`
  end
end

def format_percent(float)
  "#{format('%<float>.2f', float: float)}%"
end

def meta_fields
  path = 'vendor/choosealicense.com/_data/meta.yml'
  path = File.expand_path(path, project_root)
  YAML.safe_load_file(path)
end

RSpec::Matchers.define :be_an_existing_file do
  match { |path| File.exist?(path) }
end

RSpec::Matchers.define :be_detected_as do |expected|
  match do |actual|
    @expected_as_array = [expected.content_normalized(wrap: 80)]
    @license_file = Licensee::ProjectFiles::LicenseFile.new(actual, 'LICENSE')
    @actual = @license_file.content_normalized(wrap: 80)
    return false unless @license_file.license

    values_match? expected, @license_file.license
  end

  failure_message do |actual|
    license_file = Licensee::ProjectFiles::LicenseFile.new(actual, 'LICENSE')
    license_name = expected.meta['spdx-id'] || expected.key
    similarity = expected.similarity(license_file)
    content = @license_file.content
    msg = "Expected '#{content}' to match the #{license_name} license"
    msg << " (#{format_percent(similarity)} similarity"
    msg << "using the #{license_file.matcher} matcher)"
  end

  failure_message_when_negated do |actual|
    license_file = Licensee::ProjectFiles::LicenseFile.new(actual, 'LICENSE')
    license_name = expected.meta['spdx-id'] || expected.key
    similarity = expected.similarity(license_file)

    msg = "Expected the content to *not* match the #{license_name} license"
    msg << " (#{format_percent(similarity)} similarity)"
  end

  diffable
end

def license_hashes
  @license_hashes ||= JSON.parse(fixture_contents('license-hashes.json'))
end
