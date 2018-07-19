USING: alien.c-types alien.data io layouts ;
"Word size: " write cell 8 * .
"Endianness: " write little-endian? "little" "big" ? print
