package std

import std::ascii

// TO BE DEPRECATED
import uint from std::integer

// =================================================================
// Print functions
// =================================================================

// Print an integer to stdout
public native method print(int value)

// Print an ASCII string to stdout
public native method print(ascii::string value)

// Print an integer to stdout (with newline terminator)
public native method println(int value)

// Print an ASCII string to stdout (with newline terminator)
public native method println(ascii::string value)

// =================================================================
// Stream Reader
// =================================================================

// A generic reader represents an input stream of items (e.g. bytes or
// characters), such as from a file, network socket, or a memory buffer.

public type Reader is {

    // Reads at most a given number of bytes from the stream.  This
    // operation may block if the number requested is greater than that
    // available.
    method read(uint) -> byte[],

    // Check whether the end-of-stream has been reached and, hence,
    // that there are no further bytes which can be read.
    method has_more() -> bool,

    // Closes this input stream thereby releasin any resources
    // associated with it.
    method close(),

    // Return the number of bytes which can be safely read without
    // blocking.
    method available() -> uint,

    // Space for additional operations defined by refinements of
    // Reader
    ...
}

// =================================================================
// Stream Writer
// =================================================================

// A generic writer represents an output stream of data items
// (e.g. bytes or characters), such as being written a file, socket or
// console.
public type Writer is {

    // Writes a given list of bytes to the output stream.
    method write(byte[]) -> uint,

    // Flush this output stream thereby forcing those items written
    // thus far to the output device.
    method flush(),

    // Closes this output stream thereby releasin any resources
    // associated with it.
    method close(),

    // Space for additional operations defined by refinements of
    // InputStream
    ...
}
