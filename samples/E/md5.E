def makeMessageDigest := <import:java.security.makeMessageDigest>
def sprintf := <import:java.lang.makeString>.format

def digest := makeMessageDigest.getInstance("MD5") \
  .digest("The quick brown fox jumped over the lazy dog's back".getBytes("iso-8859-1"))

for b in digest {
  print(sprintf("%02x", [b]))
}
println()
