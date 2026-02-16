# YAJL C Bindings for Ruby

This gem is a C binding to the excellent YAJL JSON parsing and generation library.

You can read more info at the project's website http://lloyd.github.com/yajl or check out its code at http://github.com/lloyd/yajl.

## Features

* JSON parsing and encoding directly to and from an IO stream (file, socket, etc) or String. Compressed stream parsing and encoding supported for Bzip2, Gzip and Deflate.
* Parse and encode *multiple* JSON objects to and from streams or strings continuously.
* JSON gem compatibility API - allows yajl-ruby to be used as a drop-in replacement for the JSON gem
* Basic HTTP client (only GET requests supported for now) which parses JSON directly off the response body *as it's being received*
* ~3.5x faster than JSON.generate
* ~1.9x faster than JSON.parse
* ~4.5x faster than YAML.load
* ~377.5x faster than YAML.dump
* ~1.5x faster than Marshal.load
* ~2x faster than Marshal.dump

## How to install

Go ahead and install it as usual:

```
gem install yajl-ruby
```

Or use your Gemfile:

``` ruby
gem 'yajl-ruby', require: 'yajl'
```

## Example of use

NOTE: I'm building up a collection of small examples in the examples (http://github.com/brianmario/yajl-ruby/tree/master/examples) folder.

First, you're probably gonna want to require it:

``` ruby
require 'yajl'
```

### Parsing

Then maybe parse some JSON from:

a File IO

``` ruby
json = File.new('test.json', 'r')
parser = Yajl::Parser.new
hash = parser.parse(json)
```

or maybe a StringIO

``` ruby
json = StringIO.new("...some JSON...")
parser = Yajl::Parser.new
hash = parser.parse(json)
```

or maybe STDIN

```
cat someJsonFile.json | ruby -ryajl -e "puts Yajl::Parser.parse(STDIN).inspect"
```

Or lets say you didn't have access to the IO object that contained JSON data, but instead
only had access to chunks of it at a time. No problem!

(Assume we're in an EventMachine::Connection instance)

``` ruby
def post_init
  @parser = Yajl::Parser.new(:symbolize_keys => true)
end

def object_parsed(obj)
  puts "Sometimes one pays most for the things one gets for nothing. - Albert Einstein"
  puts obj.inspect
end

def connection_completed
  # once a full JSON object has been parsed from the stream
  # object_parsed will be called, and passed the constructed object
  @parser.on_parse_complete = method(:object_parsed)
end

def receive_data(data)
  # continue passing chunks
  @parser << data
end
```

Or if you don't need to stream it, it'll just return the built object from the parse when it's done.
NOTE: if there are going to be multiple JSON strings in the input, you *must* specify a block or callback as this
is how yajl-ruby will hand you (the caller) each object as it's parsed off the input.

``` ruby
obj = Yajl::Parser.parse(str_or_io)
```

Or how about a JSON API HTTP request?
This actually makes a request using a raw TCPSocket, then parses the JSON body right off the socket. While it's being received over the wire!

``` ruby
require 'uri'
require 'yajl/http_stream'

url = URI.parse("http://search.twitter.com/search.json?q=engineyard")
results = Yajl::HttpStream.get(url)
```

Or do the same request, with Gzip and Deflate output compression support (also supports Bzip2, if loaded):
(this does the same raw socket Request, but transparently parses the compressed response body)

``` ruby
require 'uri'
require 'yajl/gzip'
require 'yajl/deflate'
require 'yajl/http_stream'

url = URI.parse("http://search.twitter.com/search.json?q=engineyard")
results = Yajl::HttpStream.get(url)
```

Since yajl-ruby parses JSON as a stream, supporting APIs like Twitter's Streaming API are a piece-of-cake.
You can simply supply a block to `Yajl::HttpStream.get`, which is used as the callback for when a JSON object has been
unserialized off the stream. For the case of this Twitter Streaming API call, the callback gets fired a few times a second (depending on your connection speed).
The code below is all that's needed to make the request and stream unserialized Ruby hashes off the response, continuously.
You'll note that I've enabled the :symbolize_keys parser option as well. Doing so is much more efficient for parsing JSON streams with
lots of repetitive keys - for things like result sets or multiple API requests - than the same parse with string keys.
This is because Ruby will reuse (and never GC) its symbol table. Be that as it may, if you want to parse JSON strings with random key names
it's much better to leave string keys enabled (the default), so they can get GC'd later.

``` ruby
require 'uri'
require 'yajl/http_stream'

uri = URI.parse("http://#{username}:#{password}@stream.twitter.com/spritzer.json")
Yajl::HttpStream.get(uri, :symbolize_keys => true) do |hash|
  puts hash.inspect
end
```

Or how about parsing directly from a compressed file?

``` ruby
require 'yajl/bzip2'

file = File.new('some.json.bz2', 'r')
result = Yajl::Bzip2::StreamReader.parse(file)
```

### Encoding

Since yajl-ruby does everything using streams, you simply need to pass the object to encode, and the IO to write the stream to (this happens in chunks).

This allows you to encode JSON as a stream, writing directly to a socket

``` ruby
socket = TCPSocket.new('192.168.1.101', 9000)
hash = {:foo => 12425125, :bar => "some string", ... }
Yajl::Encoder.encode(hash, socket)
```

Or what if you wanted to compress the stream over the wire?

``` ruby
require 'yajl/gzip'
socket = TCPSocket.new('192.168.1.101', 9000)
hash = {:foo => 12425125, :bar => "some string", ... }
Yajl::Gzip::StreamWriter.encode(hash, socket)
```

Or what about encoding multiple objects to JSON over the same stream?
This example will encode and send 50 JSON objects over the same stream, continuously.

``` ruby
socket = TCPSocket.new('192.168.1.101', 9000)
encoder = Yajl::Encoder.new
50.times do
  hash = {:current_time => Time.now.to_f, :foo => 12425125}
  encoder.encode(hash, socket)
end
```

Using `EventMachine` and you want to encode and send in chunks?
(Assume we're in an `EventMachine::Connection` instance)

``` ruby
def post_init
   # Passing a :terminator character will let us determine when the encoder
   # is done encoding the current object
   @encoder = Yajl::Encoder.new
   motd_contents = File.read("/path/to/motd.txt")
   status = File.read("/path/to/huge/status_file.txt")
   @motd = {:motd => motd_contents, :system_status => status}
end

def connection_completed
  # The encoder will do its best to hand you data in chunks that
  # are around 8kb (but you may see some that are larger)
  #
  # It should be noted that you could have also assigned the _on_progress_ callback
  # much like you can assign the _on_parse_complete_ callback with the parser class.
  # Passing a block (like below) essentially tells the encoder to use that block
  # as the callback normally assigned to _on_progress_.
  #
  # Send our MOTD and status
  @encoder.encode(@motd) do |chunk|
    if chunk.nil? # got our terminator, encoding is done
      close_connection_after_writing
    else
      send_data(chunk)
    end
  end
end
```

But to make things simple, you might just want to let yajl-ruby do all the hard work for you and just hand back
a string when it's finished. In that case, just don't provide and IO or block (or assign the on_progress callback).

``` ruby
str = Yajl::Encoder.encode(obj)
```

You can also use `Yajl::Bzip2::StreamWriter` and `Yajl::Deflate::StreamWriter`. So you can pick whichever fits your CPU/bandwidth sweet-spot.

### HTML Safety

If you plan on embedding the output from the encoder in the DOM, you'll want to make sure you use the html_safe option on the encoder. This will escape all '/' characters to ensure no closing tags can be injected, preventing XSS.

Meaning the following should be perfectly safe:

``` html
<script type="text/javascript">
  var escaped_str = <%= Yajl::Encoder.encode("</script><script>alert('hi!');</script>", :html_safe => true) %>;
</script>
```

## JSON gem Compatibility API

The JSON gem compatibility API isn't enabled by default. You have to explicitly require it like so:

``` ruby
require 'yajl/json_gem'
```

That's right, you can just replace `"require 'json'"` with the line above and you're done!

This will require yajl-ruby itself, as well as enable its JSON gem compatibility API.

This includes the following API:

JSON.parse, JSON.generate, JSON.pretty_generate, JSON.load, JSON.dump
and all of the #to_json instance method overrides for Ruby's primitive objects


Once the compatibility API is enabled, your existing or new project should work as if the JSON gem itself were being used. Only you'll be using Yajl ;)

There are a lot more possibilities that I'd love to see other gems/plugins for someday.

Some ideas:

* parsing logs in JSON format
* a Rails plugin - DONE! (http://github.com/technoweenie/yajl-rails)
* official support in Rails 3 - DONE (http://github.com/rails/rails/commit/a96bf4ab5e73fccdafb78b99e8a122cc2172b505)
 * and is the default (if installed) - http://github.com/rails/rails/commit/63bb955a99eb46e257655c93dd64e86ebbf05651
* Rack middleware (ideally the JSON body could be handed to the parser while it's still being received, this is apparently possible with Unicorn)
* JSON API clients (http://github.com/brianmario/freckle-api)

## Benchmarks

After I finished implementation - this library performs close to the same as the current JSON.parse (C gem) does on small/medium files.

But on larger files, and higher amounts of iteration, this library was around 2x faster than JSON.parse.

The main benefit of this library is in its memory usage.
Since it's able to parse the stream in chunks, its memory requirements are very, very low.

Here's what parsing a 2.43MB JSON file off the filesystem 20 times looks like:

### Memory Usage

#### Average

* Yajl::Parser#parse: 32MB
* JSON.parse: 54MB
* ActiveSupport::JSON.decode: 63MB

#### Peak

* Yajl::Parser#parse: 32MB
* JSON.parse: 57MB
* ActiveSupport::JSON.decode: 67MB

### Parse Time

* Yajl::Parser#parse: 4.54s
* JSON.parse: 5.47s
* ActiveSupport::JSON.decode: 64.42s

### Encode Time

* Yajl::Encoder#encode: 3.59s
* JSON#to_json: 6.2s
* ActiveSupport::JSON.encode: 45.58s

### Compared to YAML

NOTE: I converted the 2.4MB JSON file to YAML for this test.

#### Parse Time (from their respective formats)

* Yajl::Parser#parse: 4.33s
* JSON.parse: 5.37s
* YAML.load: 19.47s

#### Encode Time (to their respective formats)

* Yajl::Encoder#encode: 3.47s
* JSON#to_json: 6.6s
* YAML.dump(obj, io): 1309.93s

### Compared to Marshal.load/Marshal.dump

NOTE: I converted the 2.4MB JSON file to a Hash and a dump file from Marshal.dump for this test.

#### Parse Time (from their respective formats)

* Yajl::Parser#parse: 4.54s
* JSON.parse: 7.40s
* Marshal.load: 7s

#### Encode Time (to their respective formats)

* Yajl::Encoder#encode: 2.39s
* JSON#to_json: 8.37s
* Marshal.dump: 4.66s

## Third Party Sources Bundled

This project includes code from the BSD licensed yajl project, copyright 2007-2009 Lloyd Hilaiel

## Special Thanks & Contributors

For those of you using yajl-ruby out in the wild, please hit me up on Twitter (brianmario) or send me a message here on the Githubs describing the site and how you're using it. I'd love to get a list going!

I've had a lot of inspiration, and a lot of help. Thanks to everyone who's been a part of this and those to come!

* Lloyd Hilaiel - http://github.com/lloyd - for writing Yajl!!
* Josh Ferguson - http://github.com/besquared - for peer-pressuring me into getting back into C; it worked ;) Also tons of support over IM
* Jonathan Novak - http://github.com/cypriss - pointer-hacking help
* Tom Smith - http://github.com/rtomsmith - pointer-hacking help
* Rick Olson - http://github.com/technoweenie - for making an ActiveSupport patch with support for this library and teasing me that it might go into Rails 3. You sure lit a fire under my ass and I got a ton of work done because of it! :)
* The entire Github Crew - http://github.com/ - my inspiration, time spent writing this, finding Yajl, So many-MANY other things wouldn't have been possible without this awesome service. I owe you guys some whiskey at Kilowatt.
* Ben Burkert - http://github.com/benburkert
* Aman Gupta - http://github.com/tmm1 - tons of suggestions and inspiration for the most recent features, and hopefully more to come ;)
* Filipe Giusti
* Jonathan George
* Luke Redpath
* Neil Berkman
* Pavel Valodzka
* Rob Sharp
