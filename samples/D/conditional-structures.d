void main() {
    enum int i = 5;

    // "static if" for various static checks:
    static if (i == 7) {
        // ...
    } else {
        //...
    }

    // is(T == U) checks if type T is U.
    static if (is(typeof(i) == int)) {
        // ...
    } else {
        // ...
    }

    // D switch is improved over C switch:
    switch (i) {
        case 0:
            break; // Silent fallthrough is forbidden.
        case 1:
            goto case; // Explicit fallthrough.
        case 2:
            // Empty cases don't require an explicit fallthrough.
        case 3:
            return;
        case 4, 5, 7: // Multiple cases.
            break;
        case 8: .. case 15: // Inclusive interval.
            goto case 3;
        default: // Default case is required.
            break;
    }

    enum Colors { yellow, blue, brown, green }
    immutable c = Colors.blue;

    // "final switch" is safer, for enums (and in future other values,
    // like Algebraic), because all cases must be present.
    // with() is handy to avoid repeating "Colors." for each case.
    final switch (c) with (Colors) {
        case yellow:        break;
        case blue:          break;
        case brown, green:  break;
        // case yellow: .. case brown: // Forbidden in final switches.
        // default: // Forbidden in final switches.
    }
}
