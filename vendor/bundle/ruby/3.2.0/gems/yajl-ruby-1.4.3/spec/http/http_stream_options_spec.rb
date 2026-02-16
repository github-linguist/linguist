require File.expand_path(File.dirname(__FILE__) + '/../spec_helper.rb')
require 'yajl/http_stream'
require 'socket'

describe "Passing options to HttpStream instance methods" do
  before(:all) do
    @stream = Yajl::HttpStream.new
  end

  it "should not create a new socket it one is provided" do
    expect(TCPSocket).not_to receive(:new)
    options = {:socket => :my_provided_socket}

    @stream.send(:initialize_socket, URI.parse("http://google.com"), options)

    expect(options[:socket]).to eq(:my_provided_socket)
  end

  it "should create a new socket if one is not provided" do
    expect(TCPSocket).to receive(:new).with("google.com", 80).and_return( :tcp_socket )
    options = {}

    @stream.send(:initialize_socket, URI.parse("http://google.com"), options)

    expect(options[:socket]).to eq(:tcp_socket)
  end
end
