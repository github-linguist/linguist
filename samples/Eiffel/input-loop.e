note
	description : "{
There are several examples included, including input from a text file,
simple console input and input from standard input explicitly.
See notes in the code for details.

Examples were compile using Eiffel Studio 6.6 with only the default
class libraries.
}"

class APPLICATION

create
	make

feature

	make
		do
			-- These examples show non-console input (a plain text file)
			-- with end-of-input handling.
			read_lines_from_file
			read_words_from_file

			-- These examples use simplified input from 'io', that
			-- handles the details of whether it's stdin or not
			-- They terminate on a line (word) of "q"
			read_lines_from_console_with_termination
			read_words_from_console_with_termination

			-- The next examples show reading stdin explicitly
			-- as if it were a text file.  It expects and end of file
			-- termination and so will loop indefinitely unless reading
			-- from a pipe or your console can send an EOF.
			read_lines_from_stdin
			read_words_from_stdin

			-- These examples use simplified input from 'io', that
			-- handles the details of whether it's stdin or not,
			-- but have no explicit termination
			read_lines_from_console_forever
			read_words_from_console_forever
		end

	--|--------------------------------------------------------------

	read_lines_from_file
			-- Read input from a text file
			-- Echo each line of the file to standard output.
			--
			-- Some language examples omit file open/close operations
			-- but are included here for completeness.  Additional error
			-- checking would be appropriate in production code.
		local
			tf: PLAIN_TEXT_FILE
		do
			print ("Reading lines from a file%N")
			create tf.make ("myfile") -- Create a file object
			tf.open_read -- Open the file in read mode

			-- The actual input loop

			from
			until tf.end_of_file
			loop
				tf.read_line
				print (tf.last_string + "%N")
			end

			tf.close -- Close the file
		end

	--|--------------------------------------------------------------

	read_words_from_file
			-- Read input from a text file
			-- Echo each word of the file to standard output on a
			-- separate line.
			--
			-- Some language examples omit file open/close operations
			-- but are included here for completeness.  Additional error
			-- checking would be appropriate in production code.
		local
			tf: PLAIN_TEXT_FILE
		do
			print ("Reading words from a file%N")
			create tf.make ("myfile") -- Create a file object
			tf.open_read -- Open the file in read mode

			-- The actual input loop

			from
			until tf.end_of_file
			loop
				-- This instruction is the only difference between this
				-- example and the read_lines_from_file example
				tf.read_word
				print (tf.last_string + "%N")
			end

			tf.close -- Close the file
		end

	--|--------------------------------------------------------------

	read_lines_from_console_with_termination
			-- Read lines from console and echo them back to output
			-- until the line contains only the termination key 'q'
			--
			-- 'io' is acquired through inheritance from class ANY,
			-- the top of all inheritance hierarchies.
		local
			the_cows_come_home: BOOLEAN
		do
			print ("Reading lines from console%N")
			from
			until the_cows_come_home
			loop
				io.read_line
				if io.last_string ~ "q" then
					the_cows_come_home := True
					print ("Mooooo!%N")
				else
					print (io.last_string)
					io.new_line
				end
			end
		end

	--|--------------------------------------------------------------

	read_words_from_console_with_termination
			-- Read words from console and echo them back to output, one
			-- word per line, until the line contains only the
			-- termination key 'q'
			--
			-- 'io' is acquired through inheritance from class ANY,
			-- the top of all inheritance hierarchies.
		local
			the_cows_come_home: BOOLEAN
		do
			print ("Reading words from console%N")
			from
			until the_cows_come_home
			loop
				io.read_word
				if io.last_string ~ "q" then
					the_cows_come_home := True
					print ("Mooooo!%N")
				else
					print (io.last_string)
					io.new_line
				end
			end
		end

	--|--------------------------------------------------------------

	read_lines_from_console_forever
			-- Read lines from console and echo them back to output
			-- until the program is terminated externally
			--
			-- 'io' is acquired through inheritance from class ANY,
			-- the top of all inheritance hierarchies.
		do
			print ("Reading lines from console (no termination)%N")
			from
			until False
			loop
				io.read_line
				print (io.last_string + "%N")
			end
		end

	--|--------------------------------------------------------------

	read_words_from_console_forever
			-- Read words from console and echo them back to output, one
			-- word per line until the program is terminated externally
			--
			-- 'io' is acquired through inheritance from class ANY,
			-- the top of all inheritance hierarchies.
		do
			print ("Reading words from console (no termination)%N")
			from
			until False
			loop
				io.read_word
				print (io.last_string + "%N")
			end
		end

	--|--------------------------------------------------------------

	read_lines_from_stdin
			-- Read input from a stream on standard input
			-- Echo each line of the file to standard output.
			-- Note that we treat standard input as if it were a plain
			-- text file
		local
			tf: PLAIN_TEXT_FILE
		do
			print ("Reading lines from stdin (EOF termination)%N")
			tf := io.input

			from
			until tf.end_of_file
			loop
				tf.read_line
				print (tf.last_string + "%N")
			end
		end

	--|--------------------------------------------------------------

	read_words_from_stdin
			-- Read input from a stream on standard input
			-- Echo each word of the file to standard output on a new
			-- line
			-- Note that we treat standard input as if it were a plain
			-- text file
		local
			tf: PLAIN_TEXT_FILE
		do
			print ("Reading words from stdin (EOF termination)%N")
			tf := io.input

			from
			until tf.end_of_file
			loop
				tf.read_line
				print (tf.last_string + "%N")
			end
		end

end
