require 'linguist/tokenizer'

require 'test/unit'

class TestTokenizer < Test::Unit::TestCase
  include Linguist

  def samples_path
    File.expand_path("../../samples", __FILE__)
  end

  def tokenize(data)
    data = File.read(File.join(samples_path, data.to_s)) if data.is_a?(Symbol)
    Tokenizer.tokenize(data)
  end

  def test_skip_string_literals
    assert_equal %w(print), tokenize('print ""')
    assert_equal %w(print), tokenize('print "Josh"')
    assert_equal %w(print), tokenize("print 'Josh'")
    assert_equal %w(print), tokenize('print "Hello \"Josh\""')
    assert_equal %w(print), tokenize("print 'Hello \\'Josh\\''")
  end

  def test_skip_number_literals
    assert_equal %w(+), tokenize('1 + 1')
    assert_equal %w(add \( \)), tokenize('add(123, 456)')
    assert_equal %w(|), tokenize('0x01 | 0x10')
  end

  def test_skip_comments
    assert_equal %w(foo #), tokenize("foo\n# Comment")
    assert_equal %w(foo # bar), tokenize("foo\n# Comment\nbar")
    assert_equal %w(foo //), tokenize("foo\n// Comment")
    assert_equal %w(foo /* */), tokenize("foo /* Comment */")
    assert_equal %w(foo /* */), tokenize("foo /* \nComment\n */")
    assert_equal %w(foo <!-- -->), tokenize("foo <!-- Comment -->")
    assert_equal %w(foo {- -}), tokenize("foo {- Comment -}")
    assert_equal %w(foo \(* *\)), tokenize("foo (* Comment *)")
    assert_equal %w(% %), tokenize("2 % 10\n% Comment")
  end

  def test_sgml_tags
    assert_equal %w(<html> </html>), tokenize("<html></html>")
    assert_equal %w(<div> id </div>), tokenize("<div id></div>")
    assert_equal %w(<div> id= </div>), tokenize("<div id=foo></div>")
    assert_equal %w(<div> id class </div>), tokenize("<div id class></div>")
    assert_equal %w(<div> id= </div>), tokenize("<div id=\"foo bar\"></div>")
    assert_equal %w(<div> id= </div>), tokenize("<div id='foo bar'></div>")
    assert_equal %w(<?xml> version=), tokenize("<?xml version=\"1.0\"?>")
  end

  def test_operators
    assert_equal %w(+), tokenize("1 + 1")
    assert_equal %w(-), tokenize("1 - 1")
    assert_equal %w(*), tokenize("1 * 1")
    assert_equal %w(/), tokenize("1 / 1")
    assert_equal %w(%), tokenize("2 % 5")
    assert_equal %w(&), tokenize("1 & 1")
    assert_equal %w(&&), tokenize("1 && 1")
    assert_equal %w(|), tokenize("1 | 1")
    assert_equal %w(||), tokenize("1 || 1")
    assert_equal %w(<), tokenize("1 < 0x01")
    assert_equal %w(<<), tokenize("1 << 0x01")
  end

  def test_c_tokens
    assert_equal %w(#ifndef HELLO_H #define HELLO_H void hello \( \) ; #endif), tokenize(:"c/hello.h")
    assert_equal %w(#include <stdio.h> int main \( \) { printf \( \) ; return ; }), tokenize(:"c/hello.c")
  end

  def test_cpp_tokens
    assert_equal %w(class Bar { protected char *name ; public void hello \( \) ; }), tokenize(:"cpp/bar.h")
    assert_equal %w(#include <iostream> using namespace std ; int main \( \) { cout << << endl ; }), tokenize(:"cpp/hello.cpp")
  end

  def test_objective_c_tokens
    assert_equal %w(#import <Foundation/Foundation.h> @interface Foo NSObject { } @end), tokenize(:"objective-c/Foo.h")
    assert_equal %w(#import @implementation Foo @end), tokenize(:"objective-c/Foo.m")
    assert_equal %w(#import <Cocoa/Cocoa.h> int main \( int argc char *argv \) { NSLog \( @ \) ; return ; }), tokenize(:"objective-c/hello.m")
  end

  def test_javascript_tokens
    assert_equal %w( \( function \( \) { console.log \( \) ; } \) .call \( this \) ;), tokenize(:"javascript/hello.js")
  end

  def test_ruby_tokens
    assert_equal %w(module Foo end), tokenize(:"ruby/foo.rb")
    assert_equal %w(# /usr/bin/env ruby puts), tokenize(:"ruby/script.rb")
    assert_equal %w(task default do puts end), tokenize(:"ruby/filenames/Rakefile")
  end
end
