# frozen_string_literal: true

RSpec.describe Licensee::LicenseRules do
  subject { mit.rules }

  let(:mit) { Licensee::License.find('mit') }

  Licensee::Rule.groups.each do |group|
    context "the #{group} rule group" do
      it 'responds as a hash key string' do
        expect(subject[group]).to be_a(Array)
      end

      it 'responds as a hash key symbol' do
        expect(subject[group.to_sym]).to be_a(Array)
      end

      it 'responds as a method' do
        expect(subject.public_send(group.to_sym)).to be_a(Array)
      end
    end
  end

  context 'created from a license' do
    subject { described_class.from_license(mit) }

    it 'exposes the rules' do
      expect(subject.permissions.first.label).to eql('Commercial use')
    end
  end

  context 'created from a meta' do
    subject { described_class.from_meta(mit.meta) }

    it 'exposes the rules' do
      expect(subject.permissions.first.label).to eql('Commercial use')
    end
  end

  context 'created from a hash' do
    subject { described_class.from_hash(hash) }

    let(:hash) { { 'permissions' => Licensee::Rule.all } }

    it 'exposes the rules' do
      expect(subject.permissions.first.label).to eql('Commercial use')
    end
  end

  context 'to_h' do
    let(:hash) { subject.to_h }
    let(:expected) do
      {
        conditions:  subject.conditions.map(&:to_h),
        permissions: subject.permissions.map(&:to_h),
        limitations: subject.limitations.map(&:to_h)
      }
    end

    it 'Converts to a hash' do
      expect(hash).to eql(expected)
    end
  end
end
