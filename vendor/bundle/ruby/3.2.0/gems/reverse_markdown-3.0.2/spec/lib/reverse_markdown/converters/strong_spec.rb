require 'spec_helper'

describe ReverseMarkdown::Converters::Strong do
  let(:converter) { ReverseMarkdown::Converters::Strong.new }

  it 'returns an empty string if the node is empty' do
    input = node_for('<strong></strong>')
    expect(converter.convert(input)).to eq ''
  end

  it 'returns just the content if the strong tag is nested in another strong' do
    input = node_for('<strong><strong>foo</strong></strong>')
    expect(converter.convert(input.children.first, already_strong: true)).to eq 'foo'
  end

  it 'moves border whitespaces outside of the delimiters tag' do
    input = node_for("<strong> \n foo </strong>")
    expect(converter.convert(input)).to eq " **foo** "
  end

  it 'splits markers at paragraph breaks' do
    # Issue #95: <br><br> inside strong creates a paragraph break
    # Markers must be split so markdown renders correctly
    result = ReverseMarkdown.convert('<strong>hello<br><br>world</strong>')
    expect(result).to include('**hello**')
    expect(result).to include('**world**')
  end
end
