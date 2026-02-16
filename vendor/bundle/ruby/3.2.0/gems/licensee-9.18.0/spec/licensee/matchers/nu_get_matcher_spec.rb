# frozen_string_literal: true

RSpec.describe Licensee::Matchers::NuGet do
  subject { described_class.new(file) }

  let(:content) { '<license type="expression">mit</license>' }
  let(:file) { Licensee::ProjectFiles::LicenseFile.new(content, 'foo.nuspec') }
  let(:mit) { Licensee::License.find('mit') }
  let(:apache2) { Licensee::License.find('apache-2.0') }
  let(:other) { Licensee::License.find('other') }

  it 'matches' do
    expect(subject.match).to eql(mit)
  end

  it 'has a confidence' do
    expect(subject.confidence).to be(90)
  end

  {
    'double quotes'      => '<license type="expression">mit</license>',
    'single quotes'      => "<license type='expression'>mit</license>",
    'whitespace'         => '<license  type = "expression" >mit</license >',
    'leading whitespace' => ' <license type="expression">mit</license>'
  }.each do |description, license_declaration|
    context "with a #{description} license element" do
      let(:content) { license_declaration }

      it 'matches' do
        expect(subject.match).to eql(mit)
      end
    end
  end

  context 'no license field' do
    let(:content) { '<file>wrongelement</file>' }

    it 'returns nil' do
      expect(subject.match).to be_nil
    end
  end

  context 'an unknown license' do
    let(:content) { '<license type="expression">foo</license>' }

    it 'returns other' do
      expect(subject.match).to eql(other)
    end
  end

  context 'a license expression' do
    let(:content) { '<license type="expression">BSD-2-Clause OR MIT</license>' }

    it 'returns other' do
      expect(subject.match).to eql(other)
    end
  end

  {
    'nuget'            => '<licenseUrl>https://licenses.nuget.org/Apache-2.0</licenseUrl>',
    'nuget (http)'     => '<licenseUrl>http://licenses.nuget.org/Apache-2.0</licenseUrl>',
    'opensource'       => '<licenseUrl>https://opensource.org/licenses/Apache-2.0</licenseUrl>',
    'opensource (www)' => '<licenseUrl>http://www.opensource.org/licenses/Apache-2.0</licenseUrl>',
    'spdx'             => '<licenseUrl>https://spdx.org/licenses/Apache-2.0</licenseUrl>',
    'spdx (www)'       => '<licenseUrl>http://www.spdx.org/licenses/Apache-2.0</licenseUrl>',
    'spdx (html)'      => '<licenseUrl>https://spdx.org/licenses/Apache-2.0.html</licenseUrl>',
    'spdx (txt)'       => '<licenseUrl>https://spdx.org/licenses/Apache-2.0.txt</licenseUrl>'
  }.each do |description, license_declaration|
    context "with a #{description} licenseUrl element containing SPDX" do
      let(:content) { license_declaration }

      it 'matches' do
        expect(subject.match).to eql(apache2)
      end
    end
  end

  {
    '2.0 (https)'    => '<licenseUrl>https://apache.org/licenses/LICENSE-2.0</licenseUrl>',
    '2.0 (http/www)' => '<licenseUrl>http://www.apache.org/licenses/LICENSE-2.0</licenseUrl>',
    '2.0 (txt)'      => '<licenseUrl>https://apache.org/licenses/LICENSE-2.0.txt</licenseUrl>'
  }.each do |description, license_declaration|
    context "with an apache.org #{description} licenseUrl element" do
      let(:content) { license_declaration }

      it 'matches' do
        expect(subject.match).to eql(apache2)
      end
    end
  end
end
