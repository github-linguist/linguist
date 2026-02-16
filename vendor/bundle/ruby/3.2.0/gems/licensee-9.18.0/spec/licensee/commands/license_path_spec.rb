# frozen_string_literal: true

RSpec.describe 'license-path command' do
  let(:command) { ['bundle', 'exec', 'bin/licensee', 'license-path'] }
  let(:project_path) { fixture_path('mit_markdown') }
  let(:arguments) { [project_path] }
  let(:output) do
    Dir.chdir project_root do
      Open3.capture3(*[command, arguments].flatten)
    end
  end
  let(:parsed_output) { YAML.safe_load(stdout) }
  let(:stdout) { output[0] }
  let(:stderr) { output[1] }
  let(:status) { output[2] }

  it 'returns the license path' do
    expect(stdout).to match(File.join(project_path, 'LICENSE.md'))
  end

  it 'Returns a zero exit code' do
    expect(status.exitstatus).to be(0)
  end
end
