#define charMatch
{
    first = "qwertyuiop";

    // Determining if the first string starts with second string
    second = "qwerty";
    if (string_pos(second, first) > 0) {
        show_message("'" + first + "' starts with '" + second + "'");
    } else {
        show_message("'" + first + "' does not start with '" + second + "'");
    }

    second = "wert"
    // Determining if the first string contains the second string at any location
    // Print the location of the match for part 2
    if (string_pos(second, first) > 0) {
        show_message("'" + first + "' contains '" + second + "' at position " + string(x));
    } else {
        show_message("'" + first + "' does not contain '" + second + "'");
    }
    // Handle multiple occurrences of a string for part 2.
    x = string_count(second, first);
    show_message("'" + first + "' contains " + string(x) + " instances of '" + second + "'");

// Determining if the first string ends with the second string
    second = "random garbage"
    temp = string_copy(first,
                       (string_length(first) - string_length(second)) + 1,
                       string_length(second));
    if (temp == second) {
        show_message("'" + first + "' ends with '" + second + "'");
    } else {
        show_message("'" + first + "' does not end with '" + second + "'");
    }
}
