# frozen_string_literal: true

RSpec.describe Licensee::Matchers::NpmBower do
  subject { described_class.new(file) }

  let(:content) { '"license": "mit"' }
  let(:file) { Licensee::ProjectFiles::LicenseFile.new(content, 'LICENSE.txt') }
  let(:mit) { Licensee::License.find('mit') }
  let(:other) { Licensee::License.find('other') }
  let(:no_license) { Licensee::License.find('no-license') }

  it 'matches' do
    expect(subject.match).to eql(mit)
  end

  it 'has a confidence' do
    expect(subject.confidence).to be(90)
  end

  {
    'double quotes'      => '"license": "mit"',
    'single quotes'      => "'license': 'mit'",
    'mixed quotes'       => "'license': \"mit\"",
    'whitespace'         => "'license' : 'mit'",
    'no whitespace'      => "'license':'mit'",
    'leading whitespace' => " 'license':'mit'"
  }.each do |description, license_declaration|
    context "with a #{description} declaration" do
      let(:content) { license_declaration }

      it 'matches' do
        expect(subject.match).to eql(mit)
      end
    end
  end

  context 'no license field' do
    let(:content) { 'foo: bar' }

    it 'returns nil' do
      expect(subject.match).to be_nil
    end
  end

  context 'an unknown license' do
    let(:content) { "'license': 'foo'" }

    it 'returns other' do
      expect(subject.match).to eql(other)
    end
  end

  context 'a license expression' do
    let(:content) { "'license': '(MIT OR Apache-2.0 OR AGPL-3.0+)'" }

    it 'returns other' do
      expect(subject.match).to eql(other)
    end
  end

  context 'UNLICENSED' do
    let(:content) { "'license': 'UNLICENSED'" }

    it 'returns none' do
      expect(subject.match).to eql(no_license)
    end
  end
end
