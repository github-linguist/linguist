# frozen_string_literal: true

RSpec.describe Licensee::Matchers::Spdx do
  subject { described_class.new(file) }

  let(:content) { 'PackageLicenseDeclared: MIT' }
  let(:file) do
    Licensee::ProjectFiles::LicenseFile.new(content, 'LICENSE.spdx')
  end
  let(:mit) { Licensee::License.find('mit') }
  let(:other) { Licensee::License.find('other') }

  it 'matches' do
    expect(subject.match).to eql(mit)
  end

  it 'has a confidence' do
    expect(subject.confidence).to be(90)
  end

  context 'no license field' do
    let(:content) { 'foo: bar' }

    it 'returns nil' do
      expect(subject.match).to be_nil
    end
  end

  context 'an unknown license' do
    let(:content) { 'PackageLicenseDeclared: xyz' }

    it 'returns other' do
      expect(subject.match).to eql(other)
    end
  end

  context 'a license expression' do
    let(:content) { 'PackageLicenseDeclared: (MIT OR Apache-2.0)' }

    it 'returns other' do
      expect(subject.match).to eql(other)
    end
  end
end
