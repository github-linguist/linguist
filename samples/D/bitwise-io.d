import std.stdio: File;
import core.stdc.stdio: FILE, fputc, fgetc;

/***********
Bitwise I/O, the file must be in binary mode, and its FILE*
must be kept open during the usage of BitwiseFile.
*/
struct BitwiseFile {
    FILE* fp;
    uint accu;
    int bits;

    this(File f)
    in {
        assert(f.isOpen);
        //assert(f.isBinary);
    } body {
        this.fp = f.getFP();
    }

    void write(const(ubyte)[] buf, size_t nBits, size_t shift)
    nothrow {
        auto accu = this.accu;
        auto bits = this.bits;
        auto bufPtr = buf.ptr;

        bufPtr += shift / 8;
        shift %= 8;

        while (nBits || bits >= 8) {
            while (bits >= 8) {
                bits -= 8;
                fputc(accu >> bits, this.fp);
                accu &= (1 << bits) - 1;
            }
            while (bits < 8 && nBits) {
                accu = (accu << 1) |
                       (((128 >> shift) & *bufPtr) >> (7 - shift));
                nBits--;
                bits++;
                shift++;
                if (shift == 8) {
                    shift = 0;
                    bufPtr++;
                }
            }
        }

        this.accu = accu;
        this.bits = bits;
    }

    size_t read(ubyte[] buf, size_t nBits, size_t shift) nothrow {
        auto accu = this.accu;
        auto bits = this.bits;
        auto bufPtr = buf.ptr;
        int i = 0;

        bufPtr += shift / 8;
        shift %= 8;

        while (nBits) {
            while (bits && nBits) {
                immutable mask = 128u >> shift;
                if (accu & (1 << (bits - 1)))
                    *bufPtr |= mask;
                else
                    *bufPtr &= ~mask;

                nBits--;
                bits--;
                shift++;

                if (shift >= 8) {
                    shift = 0;
                    bufPtr++;
                }
            }
            if (!nBits)
                break;
            accu = (accu << 8) | fgetc(this.fp);
            bits += 8;
        }

        this.accu = accu;
        this.bits = bits;
        return i;
    }

    void detach() nothrow {
        if (this.bits) {
            this.accu <<= 8 - this.bits;
            fputc(this.accu, this.fp);
        }
        this.fp = null;
        this.accu = 0;
        this.bits = 0;
    }

    nothrow ~this() {
        detach();
    }
}

void main() { // Demo code.
    import core.stdc.stdio: fopen, fclose;
    import std.stdio;

    immutable data = cast(immutable(ubyte)[])"abcdefghijk".dup;
    enum n = data.length;

    // For each ubyte in data, write 7 bits skipping 1.
    auto fout = File("bitwise_io_test.bin", "wb");
    auto bf1 = BitwiseFile(fout);
    foreach (immutable i; 0 .. n)
        bf1.write(data[i .. $], 7, 1);
    bf1.detach();
    fout.close();

    // Read 7 bits and expand to each ubyte of result skipping 1 bit.
    ubyte[n + 1] result = '\0';
    auto fin = File("bitwise_io_test.bin", "rb");
    auto bf2 = BitwiseFile(fin);
    foreach (immutable i; 0 .. n)
        bf2.read(result[i .. $], 7, 1);
    bf2.detach();
    fin.close();

    // Should be the same chars as 'data'.
    writeln(cast(string)result);
}
