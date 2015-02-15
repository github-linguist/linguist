require 'benchmark'
Document = Struct.new(:id,:a,:b,:c)
documents_a = []
documents_h = {}
1.upto(10_000) do |n|
  d = Document.new(n)
  documents_a << d
  documents_h[d.id] = d
end
searchlist = Array.new(1000){ rand(10_000)+1 }

Benchmark.bm(10) do |x|
  x.report('array'){searchlist.each{|el| documents_a.any?{|d| d.id == el}} }
  x.report('hash'){searchlist.each{|el| documents_h.has_key?(el)} }
end
