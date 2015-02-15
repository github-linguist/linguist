import std.stdio;
import std.file: getTimes, setTimes, SysTime;

void main() {
    auto fname = "unixdict.txt";
    SysTime fileAccessTime, fileModificationTime;
    getTimes(fname, fileAccessTime, fileModificationTime);
    writeln(fileAccessTime, "\n", fileModificationTime);
    setTimes(fname, fileAccessTime, fileModificationTime);
}
