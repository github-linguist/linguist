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
    assert_equal %w(print ,), tokenize("print \"Hello\", \"Josh\"")
    assert_equal %w(print ,), tokenize("print 'Hello', 'Josh'")
    assert_equal %w(print , ,), tokenize("print \"Hello\", \"\", \"Josh\"")
    assert_equal %w(print , ,), tokenize("print 'Hello', '', 'Josh'")
  end

  def test_skip_number_literals
    assert_equal %w(+), tokenize('1 + 1')
    assert_equal %w(add \( , \)), tokenize('add(123, 456)')
    assert_equal %w(|), tokenize('0x01 | 0x10')
    assert_equal %w(*), tokenize('500.42 * 1.0')
    assert_equal %w(), tokenize('1.23e-04')
    assert_equal %w(), tokenize('1.0f')
    assert_equal %w(), tokenize('1234ULL')
    assert_equal %w(G1 X55 Y5 F2000), tokenize('G1 X55 Y5 F2000')
  end

  def test_comments
    assert_equal %w(COMMENT#), tokenize("#\n")
    assert_equal %w(COMMENT#), tokenize("##\n")
    assert_equal %w(foo COMMENT#), tokenize("foo\n# Comment")
    assert_equal %w(foo COMMENT#), tokenize("foo\n## Comment")
    assert_equal %w(foo COMMENT# bar), tokenize("foo\n# Comment\nbar")

    assert_equal %w(COMMENT//), tokenize("//\n")
    assert_equal %w(COMMENT//), tokenize("///\n")
    assert_equal %w(foo COMMENT//), tokenize("foo\n// Comment")
    assert_equal %w(foo COMMENT//), tokenize("foo\n/// Comment")
    assert_equal %w(foo COMMENT//!), tokenize("foo\n//! Comment")
    assert_equal %w(COMMENT//), tokenize("//***\n")

    assert_equal %w(COMMENT--), tokenize("--\n")
    assert_equal %w(foo COMMENT--), tokenize("foo\n-- Comment")

    assert_equal %w(COMMENT"), tokenize("\"\n")
    assert_equal %w(foo COMMENT"), tokenize("foo\n\" Comment")

    assert_equal %w(COMMENT;), tokenize(";\n")
    assert_equal %w(COMMENT;), tokenize(";;\n")
    assert_equal %w(foo COMMENT;), tokenize("foo\n; Comment")
    assert_equal %w(foo COMMENT;), tokenize("foo\n;; Comment")

    assert_equal %w(foo COMMENT/*), tokenize("foo /* Comment */")
    assert_equal %w(foo COMMENT/*), tokenize("foo /*Comment*/")
    assert_equal %w(foo COMMENT/*), tokenize("foo /* \nComment\n */")
    assert_equal %w(foo COMMENT/**), tokenize("foo /** Comment */")
    assert_equal %w(foo COMMENT/**), tokenize("foo /**Comment*/")
    assert_equal %w(foo COMMENT/*!), tokenize("foo /*! Comment */")
    assert_equal %w(foo COMMENT/*!), tokenize("foo /*!Comment*/")
    assert_equal %w(COMMENT/*), tokenize("/**/")
    assert_equal %w(COMMENT/*), tokenize("/*\n*\n*/")
    assert_equal %w(COMMENT/**), tokenize("/***/")
    assert_equal %w(COMMENT/**), tokenize("/****/")
    assert_equal %w(COMMENT/*!), tokenize("/*!*/")

    assert_equal %w(foo COMMENT<!--), tokenize("foo <!-- Comment -->")
    assert_equal %w(foo COMMENT<!--), tokenize("foo <!--Comment-->")
    assert_equal %w(foo COMMENT<!--), tokenize("foo<!--Comment-->")
    assert_equal %w(foo COMMENT<!--), tokenize("foo<!---->")

    assert_equal %w(foo COMMENT{-), tokenize("foo {- Comment -}")
    assert_equal %w!foo COMMENT(*!, tokenize("foo (* Comment *)")

    assert_equal %w(COMMENT%), tokenize("%\n")
    assert_equal %w(COMMENT%), tokenize("%%\n")
    assert_equal %w(% COMMENT%), tokenize("2 % 10\n% Comment")

    assert_equal %w(foo COMMENT""" bar), tokenize("foo\n\"\"\"\nComment\n\"\"\"\nbar")
    assert_equal %w(foo COMMENT''' bar), tokenize("foo\n'''\nComment\n'''\nbar")

    # Lean comments
    assert_equal %w(foo COMMENT/-), tokenize("foo /- Comment -/")
    assert_equal %w(foo COMMENT/-), tokenize("foo /-Comment-/")
    assert_equal %w(foo COMMENT/-), tokenize("foo /- \nComment\n -/")
    assert_equal %w(foo COMMENT/-), tokenize("foo /-\nComment\n-/")
    assert_equal %w(foo COMMENT/-), tokenize("foo /-- Comment -/")
    assert_equal %w(foo COMMENT/-), tokenize("foo /--Comment-/")
    assert_equal %w(foo COMMENT/-), tokenize("foo /--\nComment\n-/")
    assert_equal %w(foo COMMENT/-), tokenize("foo /-! Comment -/")
    assert_equal %w(foo COMMENT/-), tokenize("foo /-!Comment-/")
    assert_equal %w(foo COMMENT/-), tokenize("foo /-!\nComment\n-/")
    assert_equal %w(foo COMMENT/- bar), tokenize("foo /-\nComment\n-/ bar")
    assert_equal %w(foo COMMENT/- bar), tokenize("foo /-\nComment\n-/\nbar")
    assert_equal %w(foo COMMENT/- bar), tokenize("foo\n/-\nComment\n-/\nbar")
    # Nested comments are not processed correctly as it's rarely used and adds unnecessary complexity
    assert_equal %w(foo COMMENT/- comment), tokenize("foo /- Comment /- Still Comment /- And Still Comment -/ comment")
    assert_equal %w(foo COMMENT/- comment3 - / bar), tokenize("foo /- comment1 /- comment2 -/ comment3 -/ bar")
    assert_equal %w(COMMENT/-), tokenize("/-\n*\n-/")
    assert_equal %w(COMMENT/-), tokenize("/-*-/")
    assert_equal %w(COMMENT/-), tokenize("/-**-/")
    assert_equal %w(COMMENT/-), tokenize("/-!-/")
    assert_equal %w(COMMENT/-), tokenize("/--/ -/")

    # Roff comments
    assert_equal %w(COMMENT.\\" bar), tokenize(".\\\" foo\nbar")
    assert_equal %w(COMMENT.\\" bar), tokenize(". \\\" foo\nbar")
    assert_equal %w(COMMENT'\\" bar), tokenize("'\\\" foo\nbar")
    assert_equal %w(COMMENT'\\" bar), tokenize("' \\\" foo\nbar")
    assert_equal %w(COMMENT.ig), tokenize(".ig\nComment\n..")

    # DIGITAL Command Language comments
    assert_equal %w(COMMENT$!), tokenize("$! Foo")

    # Easily mistaken with comment
    assert_equal %w(* /), tokenize("1 */ 2")
  end

  def test_sgml_tags
    assert_equal %w(< html ></ html >), tokenize("<html></html>")
    assert_equal %w(< div id ></ div >), tokenize("<div id></div>")
    assert_equal %w(< div id = foo ></ div >), tokenize("<div id=foo></div>")
    assert_equal %w(< div id class ></ div >), tokenize("<div id class></div>")
    assert_equal %w(< div id = ></ div >), tokenize("<div id=\"foo bar\"></div>")
    assert_equal %w(< div id = ></ div >), tokenize("<div id='foo bar'></div>")
    assert_equal %w(<? xml version = ?>), tokenize("<?xml version=\"1.0\"?>")
    assert_equal %w(<! DOCTYPE html >), tokenize("<!DOCTYPE html>")
    assert_equal %w(< a >), tokenize("<a>")
  end

  def test_freemarker_tags
    assert_equal %w(<# a > b </# a >), tokenize("<#a>b</#a>")
    assert_equal %w(<@ a > b </@ a >), tokenize("<@a>b</@a>")
  end

  def test_operators
    assert_equal %w(+), tokenize("1 + 1")
    assert_equal %w(+), tokenize("1+1")
    assert_equal %w(-), tokenize("1 - 1")
    assert_equal %w(-), tokenize("1-1")
    assert_equal %w(*), tokenize("1 * 1")
    assert_equal %w(*), tokenize("1*1")
    assert_equal %w(a ** b), tokenize("a ** b")
    assert_equal %w(**), tokenize("1**2")
    assert_equal %w(a ** b), tokenize("a**b")
    assert_equal %w(/), tokenize("1 / 1")
    assert_equal %w(/), tokenize("1/1")
    assert_equal %w(//), tokenize("1 // 1")
    assert_equal %w(//), tokenize("1//1")
    assert_equal %w(%), tokenize("2 % 5")
    assert_equal %w(%), tokenize("2%5")
    assert_equal %w(&), tokenize("1 & 1")
    assert_equal %w(&), tokenize("1&1")
    assert_equal %w(&&), tokenize("1 && 1")
    assert_equal %w(&&), tokenize("1&&1")
    assert_equal %w(|), tokenize("1 | 1")
    assert_equal %w(|), tokenize("1|1")
    assert_equal %w(||), tokenize("1 || 1")
    assert_equal %w(||), tokenize("1||1")
    assert_equal %w(<), tokenize("1 < 0x01")
    assert_equal %w(<), tokenize("1<0x01")
    assert_equal %w(<<), tokenize("1 << 0x01")
    assert_equal %w(<<), tokenize("1<<0x01")
    assert_equal %w(<<<), tokenize("1 <<< 0x01")
    assert_equal %w(<<<), tokenize("1<<<0x01")
    assert_equal %w(>), tokenize("1 > 0x01")
    assert_equal %w(>), tokenize("1>0x01")
    assert_equal %w(>>), tokenize("1 >> 0x01")
    assert_equal %w(>>), tokenize("1>>0x01")
    assert_equal %w(>>>), tokenize("1 >>> 0x01")
    assert_equal %w(>>>), tokenize("1>>>0x01")
    assert_equal %w(a --), tokenize("a--")
    assert_equal %w(a ++), tokenize("a++")
    assert_equal %w(-- a), tokenize("--a")
    assert_equal %w(++ a), tokenize("++a")
    assert_equal %w(a -> b), tokenize("a -> b")
    assert_equal %w(a -> b), tokenize("a->b")
    assert_equal %w(a --> b), tokenize("a --> b")
    assert_equal %w(a --> b), tokenize("a-->b")

    assert_equal %w(a <- b), tokenize("a <- b")
    assert_equal %w(a <- b), tokenize("a<-b")
    assert_equal %w(a <-- b), tokenize("a <-- b")
    assert_equal %w(a <-- b), tokenize("a<--b")

    assert_equal %w(a = b), tokenize("a = b")
    assert_equal %w(a = b), tokenize("a=b")
    assert_equal %w(a == b), tokenize("a == b")
    assert_equal %w(a == b), tokenize("a==b")
    assert_equal %w(a === b), tokenize("a === b")
    assert_equal %w(a === b), tokenize("a===b")
    assert_equal %w(a !== b), tokenize("a !== b")
    assert_equal %w(a !== b), tokenize("a!==b")
    assert_equal %w(a >= b), tokenize("a>=b")
    assert_equal %w(a <= b), tokenize("a<=b")
    assert_equal %w(a <> b), tokenize("a<>b")
    assert_equal %w(a ^ b), tokenize("a ^ b")
    assert_equal %w(a ^ b), tokenize("a^b")
    assert_equal %w(~ a), tokenize("~a")

    assert_equal %w(a := b), tokenize("a:=b")
    assert_equal %w(a :== b), tokenize("a:==b")
    assert_equal %w(a += b), tokenize("a+=b")
    assert_equal %w(a -= b), tokenize("a-=b")
    assert_equal %w(a *= b), tokenize("a*=b")
    assert_equal %w(a /= b), tokenize("a/=b")
    assert_equal %w(a %= b), tokenize("a%=b")
    assert_equal %w(a ^= b), tokenize("a^=b")
    assert_equal %w(a &= b), tokenize("a&=b")
    assert_equal %w(a |= b), tokenize("a|=b")
    assert_equal %w(a ~= b), tokenize("a~=b")
    assert_equal %w(a =~ b), tokenize("a=~b")
    assert_equal %w(a !~ b), tokenize("a!~b")

    # Regexps/Globs
    assert_equal %w(.*), tokenize(".*")
    assert_equal %w(.*?), tokenize(".*?")
    assert_equal %w(.**), tokenize(".**")
    assert_equal %w(.+), tokenize(".+")
    assert_equal %w(.+?), tokenize(".+?")
    assert_equal %w(.++), tokenize(".++")
    assert_equal %w((?: a )), tokenize("(?:a)")

    assert_equal %w([[ a ]]), tokenize("[[a]]")
    assert_equal %w([[ a ]]), tokenize("[[ a ]]")

    # Edge cases
    assert_equal %w(- ! # $ % & * + , . : ; <=>), tokenize("-!#$%&*+,.:;<=>")
    assert_equal %w(- ! # $ % & ? @ \\ ^ _ ` | ~), tokenize("-!#$%&?@\\^_`|~")
    assert_equal %w(- ! # $ % & * + , . : ; <=>), tokenize("-!#$%&*+,.:;<=>")
    assert_equal %w(- / ! # $ % & * + , . : ; <>), tokenize("-/!#$%&*+,.:;<>")
  end

  def test_c_tokens
    assert_equal %w(#ifndef HELLO_H #define HELLO_H void hello \(\) ; #endif), tokenize(:"C/hello.h")
    assert_equal %w(#include < stdio .h > int main \(\) { printf \( \) ; return ; }), tokenize(:"C/hello.c")
  end

  def test_cpp_tokens
    assert_equal %w(class Bar { protected : char * name ; public : void hello \(\) ; }), tokenize(:"C++/bar.h")
    assert_equal %w(#include < iostream > using namespace std ; int main \(\) { cout << << endl ; }), tokenize(:"C++/hello.cpp")
  end

  def test_lua_tokens
    assert_equal %w({...}), tokenize("{...}")
  end

  def test_objective_c_tokens
    assert_equal %w(#import < Foundation / Foundation .h > @interface Foo : NSObject { } @end), tokenize(:"Objective-C/Foo.h")
    assert_equal %w(#import @implementation Foo @end), tokenize(:"Objective-C/Foo.m")
    assert_equal %w(#import < Cocoa / Cocoa .h > int main \( int argc , char * argv [] \) { NSLog \( @ \) ; return ; }), tokenize(:"Objective-C/hello.m")
  end

  def test_perl_tokens
    assert_equal %w(COMMENT# COMMENT# COMMENT# package POSIX ; #line sub getchar { usage if @_ != ; CORE :: getc \( STDIN \) ; } COMMENT# ;), tokenize(:"Perl/getchar.al")
    assert_equal %w(@_), tokenize("@_")
    assert_equal %w($_), tokenize("$_")
  end

  def test_php_tokens
    assert_equal %w(<? php echo ( ) ; ?>), tokenize("<?php echo('hello world'); ?>")
    assert_equal %w(<? php COMMENT/* ?>), tokenize("<?php /* comment */ ?>")
  end

  def test_prolog_tokens
    assert_equal %w(a ( A , B ) :- f .), tokenize("a(A, B) :- f.")
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
    assert_equal %w( \( function \(\) { console .log \( \) ; } \) .call \( this \) ;), tokenize(:"JavaScript/hello.js")
  end

  def test_json_tokens
    assert_equal %w( { : , : , : , : [ , ] , : { : , : } } ), tokenize(:"JSON/product.json")
  end

  def test_ruby_tokens
    assert_equal %w(module Foo end), tokenize(:"Ruby/foo.rb")
    assert_equal %w(task : default do puts end), tokenize(:"Ruby/filenames/Rakefile")
  end

  def test_shell_tokens
    # Bash
    assert_equal %w(&>), tokenize("&>")
    assert_equal %w(|&), tokenize("|&")
    assert_equal %w(<&), tokenize("<&")
    assert_equal %w(>&), tokenize(">&")
    assert_equal %w(${ a }), tokenize("${a}")
    assert_equal %w($( a )), tokenize("$( a )")
    assert_equal %w($(( + ))), tokenize("$(( 1+1 ))")

    # Fish
    assert_equal %w(<&-), tokenize("<&-")
    assert_equal %w(&|), tokenize("&|")
  end

  def test_truncate
    assert_equal ['a'*16], tokenize('a'*100)
  end

  def test_long_token
    assert_equal ["." * 16], tokenize("." * (32*1024+1))
  end

  # This is a terrible way to test this, but it does the job.
  #
  # If this test fails, it means you've introduced a regression in the tokenizer in the form of an action that uses
  # REJECT or a rule with a trailing context which is effectively the same as REJECT. Both of these cause us problems
  # because they introduce a fixed length buffer. This fixed buffer can cause the tokenizer to crash. This also has
  # an impact on performance of the tokenizer.
  #
  # Please do not use rules with a trailing context or REJECT actions
  #
  def test_flex_no_reject
    refute File.open("ext/linguist/lex.linguist_yy.c").grep(/#define REJECT reject_used_but_not_detected/).empty?, \
      "Tokenizer should not use rules with a trailing context or REJECT actions"
  end
end
