require 'spec_helper'

describe ReverseMarkdown::Converters::Em do
  let(:converter) { ReverseMarkdown::Converters::Em.new }

  it 'returns an empty string if the node is empty' do
    input = node_for('<em></em>')
    expect(converter.convert(input)).to eq ''
  end

  it 'returns just the content if the em tag is nested in another em' do
    input = node_for('<em><em>foo</em></em>')
    expect(converter.convert(input.children.first, already_italic: true)).to eq 'foo'
  end

  it 'moves border whitespaces outside of the delimiters tag' do
    input = node_for("<em> \n foo </em>")
    expect(converter.convert(input)).to eq " _foo_ "
  end

  it 'splits markers at paragraph breaks' do
    # Issue #95: <br><br> inside em creates a paragraph break
    # Markers must be split so markdown renders correctly
    result = ReverseMarkdown.convert('<em>hello<br><br>world</em>')
    expect(result).to include('_hello_')
    expect(result).to include('_world_')
  end
end
