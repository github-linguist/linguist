# frozen_string_literal: true

RSpec.describe Licensee::Matchers::Cabal do
  subject { described_class.new(file) }

  let(:content) { 'license: mit' }
  let(:file) { Licensee::ProjectFiles::LicenseFile.new(content, 'LICENSE.txt') }
  let(:mit) { Licensee::License.find('mit') }
  let(:no_license) { Licensee::License.find('no-license') }

  it 'matches' do
    expect(subject.match).to eql(mit)
  end

  it 'has a confidence' do
    expect(subject.confidence).to be(90)
  end

  {
    'whitespace'         => 'license : mit',
    'no whitespace'      => 'license:mit',
    'leading whitespace' => ' license:mit'
  }.each do |description, license_declaration|
    context "with a #{description} declaration" do
      let(:content) { license_declaration }

      it 'matches' do
        expect(subject.match).to eql(mit)
      end
    end
  end

  context 'non-standard license format' do
    let(:content) { "license: #{cabal_license}" }

    context 'GPL-3' do
      let(:cabal_license) { 'GPL-3' }

      it 'returns GPL-3.0' do
        expect(subject.match).to eql(Licensee::License.find('GPL-3.0'))
      end
    end

    context 'GPL-2' do
      let(:cabal_license) { 'GPL-2' }

      it 'returns GPL-2.0' do
        expect(subject.match).to eql(Licensee::License.find('GPL-2.0'))
      end
    end

    context 'LGPL-2.1' do
      let(:cabal_license) { 'LGPL-2.1' }

      it 'returns LGPL-2.1' do
        expect(subject.match).to eql(Licensee::License.find('LGPL-2.1'))
      end
    end

    context 'LGPL-3' do
      let(:cabal_license) { 'LGPL-3' }

      it 'returns LGPL-3.0' do
        expect(subject.match).to eql(Licensee::License.find('LGPL-3.0'))
      end
    end

    context 'AGPL-3' do
      let(:cabal_license) { 'AGPL-3' }

      it 'returns AGPL-3.0' do
        expect(subject.match).to eql(Licensee::License.find('AGPL-3.0'))
      end
    end

    context 'BSD2' do
      let(:cabal_license) { 'BSD2' }

      it 'returns BSD-2-Clause' do
        expect(subject.match).to eql(Licensee::License.find('BSD-2-Clause'))
      end
    end

    context 'BSD3' do
      let(:cabal_license) { 'BSD3' }

      it 'returns BSD-3-Clause' do
        expect(subject.match).to eql(Licensee::License.find('BSD-3-Clause'))
      end
    end

    context 'MIT' do
      let(:cabal_license) { 'MIT' }

      it 'returns MIT' do
        expect(subject.match).to eql(Licensee::License.find('MIT'))
      end
    end

    context 'ISC' do
      let(:cabal_license) { 'ISC' }

      it 'returns ISC' do
        expect(subject.match).to eql(Licensee::License.find('ISC'))
      end
    end

    context 'MPL-2.0' do
      let(:cabal_license) { 'MPL-2.0' }

      it 'returns MPL-2.0' do
        expect(subject.match).to eql(Licensee::License.find('MPL-2.0'))
      end
    end

    context 'Apache-2.0' do
      let(:cabal_license) { 'Apache-2.0' }

      it 'returns Apache-2.0' do
        expect(subject.match).to eql(Licensee::License.find('Apache-2.0'))
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
    let(:content) { 'license: foo' }

    it 'returns other' do
      expect(subject.match).to eql(Licensee::License.find('other'))
    end
  end
end
