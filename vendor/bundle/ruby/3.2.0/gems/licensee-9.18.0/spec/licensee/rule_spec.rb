# frozen_string_literal: true

RSpec.describe Licensee::Rule do
  subject do
    described_class.new(
      description: 'description',
      tag:         'tag',
      label:       'label',
      group:       'group'
    )
  end

  let(:groups) { %w[permissions conditions limitations] }

  it 'stores properties' do
    expect(subject.tag).to eql('tag')
    expect(subject.label).to eql('label')
    expect(subject.description).to eql('description')
    expect(subject.group).to eql('group')
  end

  it 'loads the groups' do
    expect(described_class.groups).to eql(groups)
  end

  it 'loads the raw rules' do
    groups.each do |key|
      expect(described_class.raw_rules).to have_key(key)
    end
  end

  it 'determines the file path' do
    path = described_class.file_path
    expect(File.exist?(path)).to be(true)
  end

  it 'loads a rule by tag' do
    rule = described_class.find_by_tag('commercial-use')
    expect(rule).to be_a(described_class)
    expect(rule.tag).to eql('commercial-use')
  end

  it 'loads a rule by tag and group' do
    rule = described_class.find_by_tag_and_group('patent-use', 'limitations')
    expect(rule).to be_a(described_class)
    expect(rule.tag).to eql('patent-use')
    expect(rule.description).to include('does NOT grant')

    rule = described_class.find_by_tag_and_group('patent-use', 'permissions')
    expect(rule).to be_a(described_class)
    expect(rule.tag).to eql('patent-use')
    expect(rule.description).to include('an express grant of patent rights')
  end

  it 'loads all rules' do
    expect(described_class.all.count).to be(17)
    rule = described_class.all.first
    expect(rule).to be_a(described_class)
    expect(rule.tag).to eql('commercial-use')
  end

  context 'to_h' do
    let(:hash) { described_class.all.first.to_h }
    let(:description) do
      'The licensed material and derivatives may be used for commercial purposes.'
    end
    let(:expected) do
      {
        tag:         'commercial-use',
        label:       'Commercial use',
        description: description
      }
    end

    it 'Converts to a hash' do
      expect(hash).to eql(expected)
    end
  end
end
