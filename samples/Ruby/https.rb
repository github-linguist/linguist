require 'net/https'
require 'uri'
require 'pp'

uri = URI.parse('https://sourceforge.net')
http = Net::HTTP.new(uri.host,uri.port)
http.use_ssl = true
http.verify_mode = OpenSSL::SSL::VERIFY_NONE

http.start do
  content = http.get("/")
  p [content.code, content.message]
  pp content.to_hash
  puts content.body
end
