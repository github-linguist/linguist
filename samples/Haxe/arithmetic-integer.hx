class BasicIntegerArithmetic {
    public static function main() {
        var args =Sys.args();
        if (args.length < 2) return;
        var a = Std.parseFloat(args[0]);
        var b = Std.parseFloat(args[1]);
        trace("a+b = " + (a+b));
        trace("a-b = " + (a-b));
        trace("a*b = " + (a*b));
        trace("a/b = " + (a/b));
        trace("a%b = " + (a%b));
    }
}
