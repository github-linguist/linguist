import test1;
import std.stdio;
import std.file;
class full2:base2 {
        this(byte[]manip,bool isroot=true) {super(manip,isroot);}
        this(){super();}
        void print() {
                foreach(item;rep) {
                        writefln(item.i32);
                }
        }
}

void main() {
        full2 base = new full2();
        base1 tmp = new base1;
        tmp.i32 = 34;
        base.add_rep(tmp);
        tmp = new base1;
        tmp.i32 = 32;
        base.add_rep(tmp);
        tmp = new base1;
        tmp.i32 = 33;
        base.add_rep(tmp);
        tmp = new base1;
        tmp.i32 = 36;
        base.add_rep(tmp);
        writefln("Input data:");
        base.print;
        write("objects.dat",base.Serialize());
        byte[]filedata = cast(byte[])read("objects.dat");
        base = new full2(filedata);
        writefln("Output data:");
        base.print;
}
