# Changelog

## 1.1.0 (November 9th, 2011)
* fix compilation due to a "bug" in gcc-llvm on 10.7.2
* fix gemspec so ruby 1.8.6 or later is required
*

## 1.0.0 (September 13th, 2011)
* add deprecation notice for Yajl's Bzip2 support
* add deprecation notice for Yajl's Deflate support
* add deprecation notice for Yajl's Gzip support
* add deprecation notice for Yajl's JSON gem compatibility API
* add deprecation notice for Yajl::HttpStream
* change the path the extension is copied into to be 'lib/yajl'
* remove 'ext' from the loadpath

## 0.8.3 (August 16th, 2011)
* fix bug where Yajl::HttpStream wouldn't pass through a user-specified socket
* fix incorrect Ruby initialization hook method name
* Bump bundled YAJL version to 1.0.12
* fix to correctly symbolize multibyte characters on 1.9
* add `:headers` option to Yajl::HttpStream for user-specified arbitrary headers

## 0.8.2 (March 22nd, 2011)
* define RSTRING_NOT_MODIFIED for rbx to prevent string caching, making things A LOT faster (100x)

## 0.8.1 (February 11th, 2011)
* fixed a retart bug where Yajl::VERSION wasn't defined when explicitly requiring yajl/http_stream

## 0.8.0 (February 2nd, 2011)
* added a new html_safe option to Yajl::Encoder to escape '/' characters for use in the DOM
* moved away from Jeweler to a Bundler/manual gemfile management setup

## 0.7.9 (January 11th, 2011)
* moved to rspec2
* fixed some compilation warnings on 1.9.3
* brought over latest from Yajl upstream
* finally removed the deprecated Yajl::Stream methods
* moved to rake-compiler
* moved to Bundler for development
* fix memory corruption bug when using :pretty => true and a custom indent string
* fixed memory leak when exceptions were being raised during a parse

## 0.7.8 (September 27th, 2010)
* fix a bug in chunked http response regex (thanks to http://github.com/kevn for catching this)
* Make sure json compability doesn't break ActiveSupport#to_json
* fix improper usage of rb_define_method

## 0.7.7 (July 12th, 2010)
* full string encoding support for 1.9, respecting Encoding.default_internal
* refactor the #to_json function bodies into a C macro
* some misc code cleanup in the benchmark scripts

## 0.7.6 (May 1st, 2010)
* use memcmp instead of strcmp for invalid Fixnum check
* add a spec to verify unicode chars can be used as keys
* twitter examples updated
* only use -Wextra if ENV['DEBUG'] is set as gcc 3 doesn't know about it
* fix chunked http encoding parse logic to further comply with the spec (thanks to Sebastian Cohnen <sebastian.cohnen@gmx.net>)
* removed as_json checks and usage in encoder to prevent infinite loops
** In Rails a lot of objects return self from the as_json method - which is wrong IMO - and needs to be fixed before this feature will work properly

## 0.7.5 (March 23rd, 2010)
* check for existence of and use as_json method on custom objects
* bugfix with read buffer when parsing from an IO (thanks to Pavel Valodzka <pavel@valodzka.name>)
* merged in latest yajl
* enable -Wextra during compilation
* brought back ability to pass a buffer to bzip/gzip/deflate #read helper methods

## 0.7.4 (March 3rd, 2010)
* bugfix for the JSON gem compatibility API's Object#to_json method improperly encoding strings

## 0.7.3 (February 23rd, 2010)
* remove trap from HttpStream code, it's really not needed

## 0.7.2 (February 23rd, 2010)
* fixed broken to_json compatibility
* removed strlen in a few places in favor of RSTRING_LEN since ruby already knows the length of the string
* patched Yajl to more efficiently reset it's lexer (no more malloc/free)
* removed dependency on IO#eof? when parsing from an IO for full Rack-spec compatibility
* removed some various cruft code in C

## 0.7.1 (February 17th, 2010)
* revert a patch made to bundled Yajl enabling optional quoting of strings that broke binary API compatibility

## 0.7.0 (February 5th, 2010)
* ensure utf8 encoding is set on relevant strings during parse/encode in 1.9

## 0.6.9 (January 26th, 2010)
* HttpStream patches merged in from Luke Redpath <contact@lukeredpath.co.uk>
* Changed how Yajl::Parser was calling IO#read to better conform to the Rack spec and thus can be used to directly parse a rack.input stream

## 0.6.8 (January 1st, 2010)
* A couple of small performance patches
* Allow passing a string to Yajl::HttpStream methods instead of only a URI

## 0.6.7 (December 4th, 2009)
* Bump internal version constant to the proper value (doh!)
* Bring over latest from Yajl upstream

## 0.6.6 (December 1st, 2009)
* Brought over some optimizations from Macruby's use of some yajl-ruby codez
* Yajl::HttpStream now supports being killed for long-running requests, thanks to Filipe Giusti <filipegiusti@gmail.com>

## 0.6.5 (November 13th, 2009)
* optimize symbol creation while symbolize_keys is turned on
* fix for 32bit integer conversion into ruby

## 0.6.4 (November 4th, 2009)
* All specs pass on Rubinius :)
* Added Yajl.load and Yajl.dump for compatibility with other various data format API's in ruby
* Fixed a bug in Yajl::Encoder which allowed direct, unescaped encoding of NaN, Infinity and -Infinity.
  It will now properly throw a Yajl::EncodeError exception if either of these values are found unescaped.
* Update bundled Yajl library to 1.0.7
* Conditionally define RSTRING_* and RARRAY_* for older versions of ruby (1.8.5 is still the default on CentOS)
* Bugfix for JSON gem exception classes to more accurately match those of the actual JSON gem
* A few small speed optimizations
* Updated specs to not run bzip2 related examples if unable to load the bzip2 library
* Finally added UTF-8 checking specs
* Removed needless calls to ID2SYM all over the place
* Updated benchmark scripts to bring the GC into the picture a little more

## 0.6.3 (August 25th, 2009)
* Fixed a bug in the JSON gem compatibility API where strings weren't being properly escaped

## 0.6.2 (August 25th, 2009)
* Fixed a bug surfaced by an existing library providing a to_json method, and Yajl would double-quote the values provided

## 0.6.1 (August 20th, 2009)
* Fixed a bug in Yajl::HttpStream where responses contained multiple JSON strings but weren't Transfer-Encoding: chunked (thanks @dacort!)

## 0.6.0 (August 19th, 2009)
* Added POST, PUT and DELETE support to Yajl::HttpStream
** POST support initially contributed by jdg (http://github.com/jdg) - Although oortle (http://github.com/oortle) coded it up in a fork with it as well.

## 0.5.12 (July 31st, 2009)
* Add another option that can be passed to Yajl::Encoder's constructor (:terminator) to allow the caller some control over
  when a full JSON string has been generated by the encoder. More information on it's use in the README

## 0.5.11 (July 14th, 2009)
* fixing a bug Aman found with to_json on non-primitive Ruby objects and double-quoting in the JSON compat API

## 0.5.10 (July 13th, 2009)
* Bugfix for the JSON gem compatibility API's default Object#to_json helper

## 0.5.9 (July 9th, 2009)
* Bugfix for Yajl::Encoder where encoding a hash like {:a => :b} would get stuck in an infinite loop

## 0.5.8 (July 6th, 2009)
* Bugfix in Yajl::HttpStream for proper handling of the Content-type header (Rob Sharp)
* Yajl::Encoder now has an on_progress callback setter, which can be used to harness the encoder's streaming ability.
** The passed Proc/lambda will be called, and passed every chunk (currently 8kb) of the encoded JSON string as it's being encoded.
* API CHANGE WARNING: Yajl::Encoder.encode's block will now be used as (and work the same as) the on_progress callback
** This means the block will be passed chunks of the JSON string at a time, giving the caller the ability to start processing the encoded data while it's still being encoded.
* fixed grammatical error in README (Neil Berkman)
* Added some encoder examples

## 0.5.7 (June 23rd, 2009)
* You can now pass parser options (like :symbolize_keys for example) to Yajl::HttpStream.get
* Refactored spec tests a bit, DRYing up the Yajl::HttpStream specs quite a bit.
* Added a spec rake task, and spec.opts file
* Updated and renamed rcov rake task, and added rcov.opts file

## 0.5.6 (June 19th, 2009)
* Added JSON.default_options hash to the JSON gem compatibility API
* Split out the JSON gem compatibility API's parsing and encoding methods into individually includable files
** the use case here is if you *only* want parsing, or *only* want encoding
** also, if you don't include encoding it won't include the #to_json overrides which tend to cause problems in some environments.
* Removed some large benchmark test files to reduce the size of the packaged gem by 1.5MB!

## 0.5.5 (June 17th, 2009)
* Introduction of the JSON gem compatibility API
** NOTE: this isn't a 1:1 compatibility API, the goal was to be compatible with as many of the projects using the JSON gem as possible - not the JSON gem API itself
** the compatibility API must be explicitly enabled by requiring 'yajl/json_gem' in your project
** JSON.parse, JSON.generate, and the #to_json instance method extension to ruby's primitive classes are all included
* Fix Yajl::Encoder to ensure map keys are strings
* Encoding multiple JSON objects to a single stream doesn't separate by a newline character anymore
* Yajl::Encoder now checks for the existence of, and will call #to_json on any non-primitive object

## 0.5.4 (June 16th, 2009)
* Yajl::Parser's :symbolize_keys option now defaults to false
* remove use of sprintf for a little speed improvement while parsing

## 0.5.3 (June 7th, 2009)
* The IO parameter for Yajl::Encode#encode is now optional, and accepts a block
** it will return the resulting JSON string if no IO is passed to stream to
** if a block is passed, it will call and pass it the resulting JSON string
* Yajl::Parser#parse can now parse from a String as well as an IO
* Added and updated lot of in-code documentation.
** all the C code exposed to Ruby should now have comments
* Added :symbolize_keys option to the Yajl::Parser class, which defaults to true.
** Having this option enabled has shown around an 18% speedup in parsing time according to my benchmarks

## 0.5.2 (May 30th, 2009)
* Added class helper methods Yajl::Encoder.encode(obj, io) and Yajl::Parser.parse(io)
 * added tests for the above
* Updated Twitter streaming example to have a less verbose output
* Patch Yajl so encoding can continue as a stream
 * IE: multiple objects encoded onto the same IO
 * added a test for the above
* Set the internal read buffer size back down to 8kb by default
* Added an internal write buffer size (set to 8kb by default) which is used to throttle writes to the output stream
 * This is to fix a major performance bug/issue with the IO#write C method in ruby 1.9.x (I've opened a bug with them about it)
* Fixed a typo in a one-off parsing spec test
* Updated benchmarks to work properly in 1.9 (required removal ActiveSupport benchmarking for now)
* Updated spec tests to respect ridiculous differences in hash key ordering between 1.8 and 1.9


## 0.5.1 (May 25th, 2009)
* added some more tests for the new API
* inlined a couple of hot functions used in parsing for a little speedup
* updates to readme, reflecting changes in API
* version bump to push another gem build

## 0.5.0 (May 25th, 2009)
* Refactored internal API so the caller can specify initialization options for the Parser and Encoder respectively. Two new classes were introduced as a result - Yajl::Parser and Yajl::Encoder. The newly refactored codebase is cleaner, thread-safe and removed all of the hack-code that was trickled around to make things work in the previous implementation. She's much more seaworthy now cap'n!
 * Yajl::Parser.new accepts two options, :allow_comments and :check_utf8 which both default to true
 * Yajl::Encoder.new accepts two options, :pretty and :indent which default to false and "  " respectively
* cleaned up a lot of state code, that to my knowledge prevented yajl-ruby from being used in a thread-safe environment.
* added deprecated messaging to Yajl::Stream.parse and Yajl::Stream.encode - these will likely go away before 0.6.0
* fixed a bug in the chunked http response parser regarding partially received chunks
* added a Twitter Search API example showing off the HttpStream API

## 0.4.9 (May 20th, 2009)
* fixed some parser state bugs surfaced by edge cases
* added support for Chunked HTTP response bodies in Yajl::HttpStream
 * added support for passing a block to Yajl::HttpStream.get that will be used as a callback whenever a JSON object is parsed off the stream (even if there is more than one!)
* added an examples folder, and put an example using the Twitter Streaming API in there to start
* added some more spec tests, this time around Chunked parsing and continuously parsing multiple JSON strings

## 0.4.8 (May 18th, 2009)
* fixed a totally bone-head compilation problem, I created for myself ;)

## 0.4.7 (May 18th, 2009)
* Bundling Yajl sources to remove the need to install them (and CMake) separately (Thank you Lloyd!!!) This means you can now simply install the gem and be off and running
* Added some spec tests for Yajl::HttpStream
* Added some spec tests for Yajl::Stream.encode
* added some more thank you's, where credit's due - in the readme
* updated the unicode.json file to reflect a "real-life" JSON response
* reorganized spec tests into their functional areas
* added an rcov rake task to generate code coverage output

## 0.4.6 (May 17th, 2009)
* Applied a patch from benburkert (http://github.com/benburkert) to fix HTTP Basic Auth in Yajl::HttpStream.get

## 0.4.5 (May 17th, 2009)
* added Yajl::Stream.encode(hash, io)
 * generates a JSON string stream, and writes to IO
 * compressed StreamWriter helpers added as well
* fixed a pretty lame segfault in (x86_64 only?) ubuntu/linux
* changed the compiled extension to have a more specific name (yajl_ext) for easier loading
* removed forced-load of .bundle file, for the rest of the planet aside from OSX users
* added some more benchmarks to compare to other forms of serialization in Ruby
* various readme updates

## 0.4.4 (May 12th, 2009)
* NOTE: Breaking API change:
 * renamed Yajl::GzipStreamReader to Yajl::Gzip::StreamReader
* added Yajl::Bzip2::StreamReader
 * depends on the bzip2-ruby gem if you want to use it, if not Yajl::Bzip2 won't be loaded
* added Yajl::Deflate::StreamReader
 * actually uses Zlib::Inflate for stream decompression
* added parse(io) class methods to Yajl::Gzip::StreamReader and Yajl::Bzip2::StreamReader as a helper for parsing compressed streams.
* updated Yajl::HttpStream to request responses compressed as deflate and bzip2 in addition to gzip
* fixed a bug regarding parsing Integers as Floats (so 123456 would have be parsed and returned as 123456.0)
* fixed a bug which caused a segfault in ruby's GC during string replacement in Yajl::Gzip and Yajl::Bzip2's StreamReader#read methods
* added support for user-specified User-Agent strings in Yajl::HttpStream

## 0.4.3 (May 2nd, 2009)
* adding text/plain as an allowed mime-type for Yajl::HttpStream for webservers that respond with it instead of application/json (ahem...Yelp...)
* renamed specs folder to spec for no reason at all

## 0.4.2 (April 30th, 2009)
* Yajl::HttpStream is now sending "proper" http request headers
* Yajl::HttpStream will request HTTP-Basic auth if credentials are provided in the passed URI
* cleanup requires

## 0.4.1 (April 30th, 2009)
* fixed a typo in the stream.rb benchmark file
* fixed a bug in Yajl::Stream.parse that was causing "strange" Ruby malloc errors on large files, with large strings
* added Yajl::GzipStreamReader as a wrapper around Zlib::GzipReader to allow for standard IO#read behavior
 * this allows Yajl::Stream to read off of a Gzip stream directly

## 0.4.0 (April 29th, 2009)
* NOTE: Breaking API change:
 * refactored Stream parsing methods out of Yajl::Native into Yajl::Stream
 * removed Yajl::Native namespace/module
* Addition of Yajl::HttpStream module
 * This module is for streaming JSON HTTP responses directly into Yajl (as they're being received) for increased awesomeness
 * it currently supports basic get requests with Yajl::HttpStream.get(uri)
 * it also supports (and prefers) output compressed (gzip) responses
* Addition Yajl::Chunked module
 * This module is for feeding Yajl JSON pieces at a time, instead of an entire IO object
 * This works very well in environments like an EventMachine app where data is received in chunks by design
* decreased read buffer for Yajl::Stream from 8kb to 4kb

## 0.3.4 (April 24th, 2009)
* turned Unicode checks back on in the Yajl parser now that it's fixed (thanks Lloyd!)
 * this also bumps the yajl version dependency requirement to 1.0.4
* better guessing of Integer/Float from number found instead of just trying to create a BigNum no matter what
* changed extconf.rb to fail Makefile creation if yajl isn't found
* added a test to check for parsing Infinity due to a Float overflow

## 0.3.3 (April 24th, 2009)
* 1.9 compatibility

## 0.3.2 (April 24th, 2009)
* version bump: forgot to include yajl.c in the gem

## 0.3.1 (April 23rd, 2009)
* fixed borked gemspec

## 0.3.0 (April 23rd, 2009)
* slight refactor of ActiveSupport tests to better reflect how they actually exist in ActiveSupport
* typo correction in the changelog which had the years in 2008
* added some initial spec tests
 * ported some from ActiveSupport to ensure proper compatibility
 * included 57 JSON fixtures to test against, all of which pass
* changed parser config to not check for invalid unicode characters as Ruby is going to do this anyway (?). This resolves the remaining test failures around unicode.
* changed how the parser was dealing with numbers to prevent overflows
* added an exception class Yajl::ParseError which is now used in place of simply printing to STDERR upon a parsing error
* renamed a couple of JSON test files in the benchmark folder to better represent their contents
* misc README updates

## 0.2.1 (April 23rd, 2009)
* fixed parsing bug - also fixed failing ActiveSupport test failures (except for the unicode one, which is an issue in Yajl itself)

## 0.2.0 (April 22nd, 2009)
* updated gemspec and README

## 0.1.0 (April 21st, 2009)
* initial release - gemified
