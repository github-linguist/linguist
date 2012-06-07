require 'linguist/tokenizer'

require 'test/unit'

class TestTokenizer < Test::Unit::TestCase
  include Linguist

  def fixtures_path
    File.expand_path("../fixtures", __FILE__)
  end

  def tokenize(name)
    data = File.read(File.join(fixtures_path, name))
    Tokenizer.new(data).tokens
  end

  def test_skip_strings
    assert_equal %w(print), Tokenizer.new('print ""').tokens
    assert_equal %w(print), Tokenizer.new('print "Josh"').tokens
    assert_equal %w(print), Tokenizer.new("print 'Josh'").tokens
    assert_equal %w(print), Tokenizer.new('print "Hello \"Josh\""').tokens
    assert_equal %w(print), Tokenizer.new("print 'Hello \\'Josh\\''").tokens
  end

  def test_skip_comments
    assert_equal %w(foo #), Tokenizer.new("foo # Comment").tokens
    assert_equal %w(foo # bar), Tokenizer.new("foo # Comment\nbar").tokens
    assert_equal %w(foo //), Tokenizer.new("foo // Comment").tokens
    assert_equal %w(foo /* */), Tokenizer.new("foo /* Comment */").tokens
    assert_equal %w(foo /* */), Tokenizer.new("foo /* \nComment\n */").tokens
    assert_equal %w(foo <!-- -->), Tokenizer.new("foo <!-- Comment -->").tokens
    assert_equal %w(foo {- -}), Tokenizer.new("foo {- Comment -}").tokens
  end

  def test_c_tokens
    assert_equal %w(#include <stdio.h> int main \( \) { printf \( \) ; return 0 ; }), tokenize("c/hello.c")
    assert_equal %w(#ifndef HELLO_H #define HELLO_H void hello \( \) ; #endif), tokenize("c/hello.h")
  end

  def test_cpp_tokens
    assert_equal %w(class Bar { protected char name ; public void hello \( \) ; }), tokenize("cpp/bar.h")
    assert_equal %w(#include <iostream> using namespace std ; int main \( \) { cout << << endl ; }), tokenize("cpp/hello.cpp")
  end

  def test_objective_c_tokens
    assert_equal %w(#import <Foundation/Foundation.h> @interface Foo NSObject { } @end), tokenize("objective-c/Foo.h")
    assert_equal %w(#import @implementation Foo @end), tokenize("objective-c/Foo.m")
    assert_equal %w(#import <Cocoa/Cocoa.h> int main \( int argc char argv \) { NSLog \( @ \) ; return 0 ; }), tokenize("objective-c/hello.m")
  end

  def test_javascript_tokens
    assert_equal %w( \( function \( \) { console.log \( \) ; } \) .call \( this \) ;), tokenize("javascript/hello.js")
  end

  def test_ruby_tokens
    assert_equal %w(module Foo end), tokenize("ruby/foo.rb")
    assert_equal %w(# /usr/bin/env ruby puts), tokenize("ruby/script.rb")
    assert_equal %w(task default do puts end), tokenize("ruby/Rakefile")
  end
end
