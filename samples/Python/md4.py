import hashlib
print hashlib.new("md4",raw_input().encode('utf-16le')).hexdigest().upper()
