import std.stdio, std.algorithm, std.array, std.string, sha_256;

struct A25 {
    // Type for a 25 ubyte (not base58 encoded) bitcoin address.
    ubyte[25] enc;

    ubyte bitcoinVersion() const pure nothrow {
        return enc[0];
    }

    ubyte[4] embeddedChecksum() const pure nothrow {
        return enc[$ - 4 .. $];
    }

    /** Computes a double sha256 hash of the first 21 bytes of
    the address. Returns the full 32 ubyte sha256 hash. */
    ubyte[32] doubleSHA256() const pure nothrow {
        return SHA256.digest(SHA256.digest(enc[0 .. 21]));
    }

    /** Returns a four ubyte checksum computed from the first 21
    bytes of the address. */
    ubyte[4] computeChecksum() const pure nothrow {
        return doubleSHA256[0 .. 4];
    }

    /** Takes a base58 encoded address and decodes it into the
    receiver. Errors are returned if the argument is not valid base58
    or if the decoded value does not fit in the 25 ubyte address.
    The address is not otherwise checked for validity. */
    string set58(in ubyte[] s) pure nothrow {
        static immutable digits =
        "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
        .representation;
        static assert(digits.length == 58);

        foreach (immutable char s1; s) {
            immutable c = digits.countUntil(s1);
            if (c < 0)
                return "found a bad char in the Bitcoin address.";

            // Currently the D type system can't see c as nonegative.
            uint uc = (c < 0) ? 0 : c;

            foreach_reverse (ref aj; enc) {
                uc += digits.length * aj;
                aj = uc % 256;
                uc /= 256;
            }
            if (uc > 0)
                return "too long Bitcoin address.";
        }

        return null;
    }
}

/** Validates a base58 encoded bitcoin address.  An address is valid
if it can be decoded into a 25 ubyte address, the Version number is 0,
and the checksum validates.  Return value ok will be true for valid
addresses.  If ok is false, the address is invalid and the error value
may indicate why. */
string isValidA58(in ubyte[] a58) pure nothrow {
    A25 a;
    immutable err = a.set58(a58);
    if (!err.empty)
        return err;
    if (a.bitcoinVersion != 0)
        return "not Bitcoin version 0.";
    return (a.embeddedChecksum == a.computeChecksum) ? null :
           "checksums don't match.";
}

void main() {
    immutable tests = ["1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i",
                       "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j",
                       "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!",
                       "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz",
                       "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz"];

    foreach (immutable test; tests) {
        immutable err = test.representation.isValidA58;
        writefln(`"%s": %s`, test, err.empty ? "OK." : err);
    }
}
