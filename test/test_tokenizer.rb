require_relative "./helper"

class TestTokenizer < Minitest::Test
  include Linguist

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
    assert_equal %w(print), tokenize("print \"Hello\", \"Josh\"")
    assert_equal %w(print), tokenize("print 'Hello', 'Josh'")
    assert_equal %w(print), tokenize("print \"Hello\", \"\", \"Josh\"")
    assert_equal %w(print), tokenize("print 'Hello', '', 'Josh'")
  end

  def test_skip_number_literals
    assert_equal %w(+), tokenize('1 + 1')
    assert_equal %w(add \( \)), tokenize('add(123, 456)')
    assert_equal %w(|), tokenize('0x01 | 0x10')
    assert_equal %w(*), tokenize('500.42 * 1.0')
    assert_equal %w(), tokenize('1.23e-04')
    assert_equal %w(), tokenize('1.0f')
    assert_equal %w(), tokenize('1234ULL')
    assert_equal %w(G1 X55 Y5 F2000), tokenize('G1 X55 Y5 F2000')
  end

  def test_skip_comments
    assert_equal %w(foo), tokenize("foo\n# Comment")
    assert_equal %w(foo bar), tokenize("foo\n# Comment\nbar")
    assert_equal %w(foo), tokenize("foo\n// Comment")
    assert_equal %w(foo), tokenize("foo\n-- Comment")
    assert_equal %w(foo), tokenize("foo\n\" Comment")
    assert_equal %w(foo), tokenize("foo /* Comment */")
    assert_equal %w(foo), tokenize("foo /* \nComment\n */")
    assert_equal %w(foo), tokenize("foo <!-- Comment -->")
    assert_equal %w(foo), tokenize("foo {- Comment -}")
    assert_equal %w(foo), tokenize("foo (* Comment *)")
    assert_equal %w(%), tokenize("2 % 10\n% Comment")
    assert_equal %w(foo bar), tokenize("foo\n\"\"\"\nComment\n\"\"\"\nbar")
    assert_equal %w(foo bar), tokenize("foo\n'''\nComment\n'''\nbar")
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
    assert_equal %w(#ifndef HELLO_H #define HELLO_H void hello \( \) ; #endif), tokenize(:"C/hello.h")
    assert_equal %w(#include <stdio.h> int main \( \) { printf \( \) ; return ; }), tokenize(:"C/hello.c")
  end

  def test_cpp_tokens
    assert_equal %w(class Bar { protected char *name ; public void hello \( \) ; }), tokenize(:"C++/bar.h")
    assert_equal %w(#include <iostream> using namespace std ; int main \( \) { cout << << endl ; }), tokenize(:"C++/hello.cpp")
  end

  def test_objective_c_tokens
    assert_equal %w(#import <Foundation/Foundation.h> @interface Foo NSObject { } @end), tokenize(:"Objective-C/Foo.h")
    assert_equal %w(#import @implementation Foo @end), tokenize(:"Objective-C/Foo.m")
    assert_equal %w(#import <Cocoa/Cocoa.h> int main \( int argc char *argv [ ] \) { NSLog \( @ \) ; return ; }), tokenize(:"Objective-C/hello.m")
  end

  def test_shebang
    assert_equal "SHEBANG#!sh", tokenize(:"Shell/sh")[0]
    assert_equal "SHEBANG#!bash", tokenize(:"Shell/bash")[0]
    assert_equal "SHEBANG#!zsh", tokenize(:"Shell/zsh")[0]
    assert_equal "SHEBANG#!perl", tokenize(:"Perl/perl")[0]
    assert_equal "SHEBANG#!python", tokenize(:"Python/python")[0]
    assert_equal "SHEBANG#!ruby", tokenize(:"Ruby/ruby")[0]
    assert_equal "SHEBANG#!ruby", tokenize(:"Ruby/ruby2")[0]
    assert_equal "SHEBANG#!node", tokenize(:"JavaScript/js")[0]
    assert_equal "SHEBANG#!php", tokenize(:"PHP/php")[0]
    assert_equal "SHEBANG#!escript", tokenize(:"Erlang/factorial")[0]
    assert_equal "echo", tokenize(:"Shell/invalid-shebang.sh")[0]
  end

  def test_javascript_tokens
    assert_equal %w( \( function \( \) { console.log \( \) ; } \) .call \( this \) ;), tokenize(:"JavaScript/hello.js")
  end

  def test_json_tokens
    assert_equal %w( { [ ] { } } ), tokenize(:"JSON/product.json")
  end

  def test_ruby_tokens
    assert_equal %w(module Foo end), tokenize(:"Ruby/foo.rb")
    assert_equal %w(task default do puts end), tokenize(:"Ruby/filenames/Rakefile")
  end
end
