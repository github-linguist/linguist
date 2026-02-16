# frozen_string_literal: true

class HashHelperSpecFixture
  include Licensee::HashHelper
  HASH_METHODS = %w[string array rule rules nil_value].freeze

  def string
    'foo'
  end

  def array
    [1, 2, 3]
  end

  def rule
    rules.first
  end

  def rules
    Licensee::Rule.all
  end

  def baz
    'baz'
  end

  def nil_value
    nil
  end
end

RSpec.describe Licensee::HashHelper do
  let(:fixture) { HashHelperSpecFixture.new }
  let(:hash) { fixture.to_h }
  let(:expected) do
    {
      string:    'foo',
      array:     [1, 2, 3],
      rule:      Licensee::Rule.all.first.to_h,
      rules:     Licensee::Rule.all.map(&:to_h),
      nil_value: nil
    }
  end

  it 'calls to_h recursively' do
    expect(hash).to eql(expected)
  end

  it 'includes hash methods' do
    expect(hash).to have_key(:string)
    expect(hash).to have_key(:array)
    expect(hash).to have_key(:rule)
    expect(hash).to have_key(:rules)
    expect(hash).to have_key(:nil_value)
  end

  it 'does not expose other methods' do
    expect(hash).not_to have_key(:baz)
  end

  it 'calls to_h recursively' do
    expect(hash[:rule]).to be_a(Hash)
  end

  it 'returns normal values' do
    expect(hash[:string]).to eql('foo')
  end

  it 'returns normal arrays' do
    expect(hash[:array]).to eql([1, 2, 3])
  end

  it 'calls to_h on array elements' do
    expect(hash[:rules].first).to be_a(Hash)
  end

  it 'returns nil values' do
    expect(hash[:nil_value]).to be_nil
  end
end
