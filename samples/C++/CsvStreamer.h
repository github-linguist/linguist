#pragma once
#include <string>
#include <vector>
#include <fstream>
#include "util.h"

using namespace std;


#define DEFAULT_DELIMITER	','


class CsvStreamer
{
	private:
		ofstream file;				// File output stream
		vector<string> row_buffer;	// Buffer which stores a row's data before being flushed/written
		int fields;					// Number of fields (columns)
		long rows;					// Number of rows (records) including header row
		char delimiter;				// Delimiter character; comma by default
		string sanitize(string);	// Returns a string ready for output into the file
		
	public:
		CsvStreamer();				// Empty CSV streamer... be sure to open the file before writing!
		CsvStreamer(string, char);	// Same as open(string, char)...
		CsvStreamer(string);		// Opens an output CSV file given a file path/name
		~CsvStreamer();				// Ensures the output file is closed and saved
		void open(string);			// Opens an output CSV file given a file path/name (default delimiter)
		void open(string, char);	// Opens an output CSV file given a file path/name and a delimiting character (default comma)
		void add_field(string);		// If still on first line, adds a new field to the header row
		void save_fields();			// Call this to save the header row; all new writes should be through append()
		void append(string);		// Appends the current row with this data for the next field; quoted only if needed (leading/trailing spaces are trimmed)
		void append(string, bool);	// Like append(string) but can specify whether to trim spaces at either end of the data (false to keep spaces)
		void append(float);			// Appends the current row with this number
		void append(double);		// Appends the current row with this number
		void append(long);			// Appends the current row with this number
		void append(int);			// Appends the current row with this number
		void writeln();				// Flushes what was in the row buffer into the file (writes the row)
		void close();				// Saves and closes the file
		int field_count();			// Gets the number of fields (columns)
		long row_count();			// Gets the number of records (rows) -- NOT including the header row
};
