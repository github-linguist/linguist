@contributor{
  name = First Contributor
  email = first@example.org
}
@contributor{
  name = Second Contributor
  email = second@example.org
}
@documentation{
Data type and function definitions for basic types
}

declaration template pan/types;

include 'pan/legacy';

@documentation{
This type implements a date/time format consistent with
ASN.1 typically used by LDAP.  The actual specification is the
"GeneralizedTime" format as specified on page 38 of the X.208
ITU-T recommendation and references within.

Ex: 20040825120123Z
    20040825120123+0100
    20040825120123,5
    20040825120123.5
    20040825120123.5-0123
}
function is_asndate = {
    # Check cardinality and type of argument.
    if (ARGC != 1 || !is_string(ARGV[0]))
        error("usage: is_asndate(string)");

    # Match the datetime pattern, extracting interesting fields.
    result = matches(ARGV[0],
        '^(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})(?:[,\.](\d+))?([Zz]|(?:[-+]\d{2}\d{2}))?$');

    if (length(result) >= 7) {
        # Do further tests on various components of the date.
        # NOTE: the to_long(to_double(x)) construct below is to avoid having
        # the to_long function treat strings with leading zeros as octal
        # numbers.  E.g. to_long("09") will throw an exception because '9' is
        # not a valid octal digit.
        year = to_long(result[1]);
        month = to_long(to_double(result[2]));
        day = to_long(to_double(result[3]));
        hour = to_long(to_double(result[4]));
        minute = to_long(to_double(result[5]));
        second = to_long(to_double(result[6]));

        frac = 0;
        if (length(result) > 7) {
            frac = to_long(to_double(result[7]));
        };

        zone = '+0000';
        if (length(result) > 8) {
            zone = result[8];
        };

        # Check the range of months.
        if (month < 1 || month > 12) {
            error("is_asndate: invalid month");
            return(false);
        };

        # Check the range of days.
        if (day < 1 || day > 31) {
            error("is_asndate: invalid day");
            return(false);
        };

        # Be more specific on the days in each month.
        if (month == 4 || month == 6 || month == 9 || month == 11) {
            if (day > 30) {
                error("is_asndate: invalid day");
            };
        };

        # February is always a bother.  Too lazy to check that the leap
        # years have been specified correctly.
        if (month == 2 && day > 29) {
            error("is_asndate: invalid day");
        };

        # Check the time.
        if (hour > 23) {
            error("is_asndate: invalid hour");
            return(false);
        };
        if (minute > 59) {
            error("is_asndate: invalid minute");
            return(false);
        };

        # Allow for leap seconds here (since it is easy).
        if (second > 60) {
            error("is_asndate: invalid minute");
            return(false);
        };

        # Check the time zone format.
        if (zone != "Z" && zone != "z") {
            tz = matches(zone, '^[-+](\d{2})(\d{2})$');

            hoffset = to_long(to_double(tz[1]));
            moffset = to_long(to_double(tz[2]));

            if (hoffset >= 12) {
                error("is_asndate: invalid hour offset in time zone");
                return(false);
            };
            if (moffset > 59) {
                error("is_asndate: invalid minute offset in time zone");
                return(false);
            };
        };

    } else {
        error("is_asndate: invalid format for time");
        return(false);
    };

    # If it gets to this point, then the date must be OK.
    true;
};


type type_asndate = string with {
    is_asndate(SELF);
};

@documentation{
    desc = Type that enforces the existence of a named interface.
}
type valid_interface = string with {
    if (exists(format('/system/network/interfaces/%s', SELF))) {
        return(true);
    };
    foreach(ifc; attr; value('/system/network/interfaces')) {
        if (attr['device'] == SELF){
            return(true);
        };
    };
    false;
};

@documentation{
    desc = CPU architectures understood by Quattor
}
type cpu_architecture = string with match (SELF, '^(i386|ia64|x86_64|sparc|aarch64|ppc64(le)?)$');
