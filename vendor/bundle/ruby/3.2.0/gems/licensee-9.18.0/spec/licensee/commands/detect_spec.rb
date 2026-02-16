# frozen_string_literal: true

RSpec.describe 'detect command' do
  let(:command) { ['bundle', 'exec', 'bin/licensee', 'detect'] }
  let(:arguments) { [] }
  let(:output) do
    Dir.chdir project_root do
      Open3.capture3(*[command, arguments].flatten)
    end
  end
  let(:parsed_output) { YAML.safe_load(stdout) }
  let(:stdout) { output[0] }
  let(:stderr) { output[1] }
  let(:status) { output[2] }
  let(:hash) { license_hashes['mit'] }
  let(:expected) do
    {
      'License'          => 'MIT',
      'Matched files'    => 'LICENSE.md, licensee.gemspec',
      'LICENSE.md'       => {
        'Content hash' => hash,
        'Attribution'  => 'Copyright (c) 2014-2021 Ben Balter and Licensee contributors',
        'Confidence'   => '100.00%',
        'Matcher'      => 'Licensee::Matchers::Exact',
        'License'      => 'MIT'
      },
      'licensee.gemspec' => {
        'Confidence' => '90.00%',
        'Matcher'    => 'Licensee::Matchers::Gemspec',
        'License'    => 'MIT'
      }
    }
  end

  {
    'No arguments' => [],
    'Project root' => [project_root],
    'License path' => [File.expand_path('LICENSE.md', project_root)]
  }.each do |name, args|
    context "When given #{name}" do
      let(:arguments) { args }

      it 'Returns a zero exit code' do
        expect(status.exitstatus).to be(0)
      end

      it 'returns the exected values' do
        hash = expected.dup

        if name == 'License path'
          hash.delete('licensee.gemspec')
          hash['Matched files'] = 'LICENSE.md'
        end

        expect(parsed_output).to eql(hash)
      end
    end
  end

  context 'json' do
    let(:arguments) { ['--json'] }
    let(:expected) { JSON.parse(fixture_contents('detect.json')).tap { |h| h['matched_files'][1].delete('content') } }

    it 'Returns a zero exit code' do
      expect(status.exitstatus).to be(0)
    end

    it 'returns valid JSON' do
      expect { JSON.parse(stdout) }.not_to raise_error
    end

    it 'returns the expected output' do
      msg = +'`licensee detect --json` output did not match expectations. '
      msg << 'Run `script/dump-detect-json-fixture` and verify the output.'
      expect(JSON.parse(stdout).tap { |h| h['matched_files'][1].delete('content') }).to eql(expected), msg
    end
  end

  context 'the default command' do
    let(:command) { ['bundle', 'exec', 'bin/licensee'] }

    it 'Returns a zero exit code' do
      expect(status.exitstatus).to be(0)
    end

    it 'returns the exected values' do
      expect(parsed_output).to eql(expected)
    end
  end

  context 'a non-existing command' do
    let(:command) { ['bundle', 'exec', 'bin/licensee', 'oops'] }

    it 'Returns a one exit code' do
      expect(status.exitstatus).to be(1)
    end
  end

  context 'no license match' do
    let(:arguments) { ["#{project_root}/spec/fixtures/wrk-modified-apache"] }

    it 'Returns a one exit code' do
      expect(status.exitstatus).to be(0)
    end
  end
end
