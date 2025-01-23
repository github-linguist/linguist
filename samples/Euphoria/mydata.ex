--****
-- === mydata.ex
-- This program uses the Euphoria Database System (EDS) to create and maintain a simple database.

constant FIELDS = {
	"Phone number",
	"Last Name",
	"First name",
	"Middle Initial"
	}

-- file to store the database in:
constant MYNAME   = "mydata.edb"

include std/eds.e  -- Euphoria Database System
include std/get.e
include std/sort.e
include std/text.e

constant KEYBOARD = 0,
	 SCREEN   = 1,
	 ERROR    = 2

constant TRUE = 1
constant WHITE_SPACE = " \t\n"
constant FORM_FEED = 12

type file_number(integer x)
-- document which vars are used as file numbers 
    return x >= 0
end type

procedure myfatal(sequence msg)
-- fatal error
    puts(ERROR, '\n' & "An unexpected error occurred: " & msg & '\n')
    ? 1/0 -- too see call stack
end procedure

function user_input()
-- get user input from keyboard
    object line

    while TRUE do
	line = gets(KEYBOARD)
	if sequence(line) then
	    -- delete any leading whitespace
	    while find(line[1], WHITE_SPACE) do
		line = line[2..length(line)]
		if length(line) = 0 then
		    exit
		end if
	    end while
	    if length(line) > 0 then
		exit
	    end if
	end if
	puts(SCREEN, "\n? ")
    end while
    -- delete trailing whitespace
    while find(line[length(line)], WHITE_SPACE) do
	line = line[1..length(line)-1] 
    end while
    return line
end function

procedure show(file_number f, object key, object data)
    puts(f, "\n" & key & '\n')
    for i = 2 to length(FIELDS) do
	puts(f, '\t' & data[i-1] & '\n')
    end for
end procedure

procedure add()
-- add a new record to the database
    sequence key, data
    integer f
    
    puts(SCREEN, "\n\t" & FIELDS[1] & ": ")
    key = user_input()
    f = db_find_key(key)
    if f >= 1 then
	show(SCREEN, db_record_key(f), db_record_data(f))
	puts(SCREEN, "Do you want to update this record? (y/n) ")
	if find('n', gets(0)) then
	    return
	end if
    end if
    data = {}
    for i = 2 to length(FIELDS) do
	puts(SCREEN, "\n\t" & FIELDS[i] & ": ")
	data = append(data, user_input())
    end for
    puts(SCREEN, '\n')
    if f >= 1 then
	-- update data part of record
	db_replace_data(f, data)
    else
	-- insert new record
	if db_insert(key, data) != DB_OK then
	    myfatal("insert failed!\n")
	end if
    end if
end procedure 

procedure delete()
-- delete a record, given first field 
    sequence name
    integer d

    puts(SCREEN, "\n\t" & FIELDS[1] & ": ")
    name = user_input()
    d = db_find_key(name)
    if d < 0 then
	puts(SCREEN, "\n\tnot found\n")
	return
    end if 
    show(SCREEN, db_record_key(d), db_record_data(d))
    puts(SCREEN, "Delete? (y/n) ")
    if find('n', gets(0)) then
	return
    end if
    db_delete_record(d)
end procedure

procedure find_name()
-- find the record that matches the surname
    sequence name
    integer f

    puts(SCREEN, "\n\t" & FIELDS[1] & ": ")
    name = user_input()
    f = db_find_key(name)
    if f < 0 then
	puts(SCREEN, "\n\tnot found\n")
	return
    end if 
    show(SCREEN, db_record_key(f), db_record_data(f))
end procedure

procedure list(file_number f)
-- list the entire database to a device
    puts(f, '\n')
    for rec = 1 to db_table_size() do
	show(f, db_record_key(rec), db_record_data(rec)) 
    end for
end procedure

procedure main()
    sequence command
    file_number printer
    
    -- open or create the database
    if db_open(MYNAME, DB_LOCK_NO) != DB_OK then
	if db_create(MYNAME, DB_LOCK_NO) != DB_OK then
	    myfatal("Couldn't create database")
	end if
	if db_create_table("phone numbers") != DB_OK then
	    myfatal("couldn't create table")
	end if
    end if

    -- select the (only) table in the database
    if db_select_table("phone numbers") != DB_OK then
	myfatal("couldn't select table\n")
    end if
    
    -- prompt the user for his command
    clear_screen()
    puts(SCREEN, "\t\tSimple Database\n")
    while TRUE do
	puts(SCREEN, 
	     "\n(a)dd, (d)elete, (f)ind, (l)ist, (p)rint, (q)uit: ")
	command = upper(user_input())
	if 'A' = command[1] then
	    add()

	elsif 'D' = command[1] then
	    delete()

	elsif 'F' = command[1] then
	    find_name()

	elsif 'Q' = command[1] then
	    exit

	elsif 'L' = command[1] then
	    list(SCREEN)

	elsif 'P' = command[1] then
	    printer = open("PRN", "w")
	    if printer = -1 then
		puts(SCREEN, "Can't open printer device\n")
	    else
		list(printer)
		puts(printer, FORM_FEED)
		close(printer)
	    end if
	else
	    puts(SCREEN, "\nsay what?\n")                   
	end if 
    end while
end procedure

main()

