module datetimedemo ;

import tango.time.Time ;
import tango.text.locale.Locale ;
import tango.time.chrono.Gregorian ;

import tango.io.Stdout ;

void main() {
    Gregorian g = new Gregorian ;
    Stdout.layout = new Locale; // enable Stdout to handle date/time format
    Time d = g.toTime(2007, 11, 10, 0, 0, 0, 0, g.AD_ERA) ;
    Stdout.format("{:yyy-MM-dd}", d).newline ;
    Stdout.format("{:dddd, MMMM d, yyy}", d).newline ;
    d = g.toTime(2008, 2, 1, 0, 0, 0, 0, g.AD_ERA) ;
    Stdout.format("{:dddd, MMMM d, yyy}", d).newline ;
}
