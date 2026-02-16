#!/usr/bin/env python3

import hashlib

def generate_language_id(language):
    # Ruby: Digest::SHA256.hexdigest(language).to_i(16) % (2**30 - 1)
    hash_digest = hashlib.sha256(language.encode()).hexdigest()
    hash_int = int(hash_digest, 16)
    return hash_int % (2**30 - 1)

print(generate_language_id("COW"))
