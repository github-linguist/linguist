<?LassoScript

define_type: 'database',
	'knop_base',
	-namespace='knop_';
//	-prototype;
	
	local: 'version'='2010-11-23',
		'description'='Custom type to interact with databases. Supports both MySQL and FileMaker datasources. ';

/*

CHANGE NOTES
2012-06-10	SP	Fix for decimal precision bug in 8.6.0.1 in renderfooter.
2012-01-15	SP	Add support for inline host method.  Thanks to Ric Lewis.
2010-11-23	JS	->settable: removed reference for -table
2009-09-18	JS	Syntax adjustments for Lasso 9
2009-06-26	JS	->nextrecord: Added deprecation warning
2009-05-15	JS	->field: corrected the verification of the -index parameter
2009-01-09	JS	Added a check before calling resultset_count so it will not break in Lasso versions before 8.5
2009-01-09	JS	->_unknowntag: fixed incorrect debug_trace
2008-12-03	JS	->addrecord: improved how keyvalue is returned when adding records
2008-12-03	JS	->addrecord: inserting a generated keyvalue can now be suppressed by specifying -keyvalue=false
2008-12-03	JS	->saverecord and ->deleterecord will now use the current keyvalue (if any), so -keyvalue will not have to be specified in that case. 
2008-11-25	JS	->field and ->recorddata will no longer touch current_record if it was zero
2008-11-24	JS	->field: Added -index parameter to be able to access any occurrence of the same field name
2008-11-24	JS	Added -> records that returns a new data type knop_databaserows
2008-11-24	JS	->resultset_count: added support for -inlinename. 
2008-11-24	JS	Changed ->nextrecord to ->next. ->nextrecord remains supported for backwards compatibility.
2008-11-14	JS	->nextrecord resets the record pointer when reaching the last record
2008-11-13	JS	->recorddata now honors the current record pointer (as incremented by -nextrecord)
2008-11-13	JS	->recorddata: added -recordindex parameter so a specific record can be returned instead of the first found.
2008-10-30	JS	->getrecord now REALLY works with integer keyvalues (double oops) - I thought I fixed it 2008-05-28 but misplaced a paren...
2008-09-26	JS	Added -> resultset_count corresponding to the same Lasso tag, so [resultset]...[/resultset] can now be used through the use of inlinename.
2008-09-10	JS	-> getrecord, ->saverecord, ->deleterecord: Corrected handling of lock user to work better with knop_user
2008-07-09	JS	->saverecord: -keeplock now updates the lock timestamp
2008-05-28	JS	->getrecord now works with integer keyvalues (oops)
2008-05-27	JS	->get returns a new datatype knop_databaserow 
2008-05-27	JS	Added ->size and ->get so a database object can be iterated. When iterating each row is returned as an array of field values. 
2008-05-27	JS	Addedd ->nextrecord that increments the recordpointer each time it is called until the last record in the found set is reached. Returns true as long as there are more records. Useful in a while loop - see example below
2008-05-27	JS	Implemented record pointer 'current_record'. The record pointer is reset for each new query. 
2008-05-27	JS	->field: added -recordindex to get data from any record in the current found set
2008-05-27	JS	Added ->_unknowntag as shortcut to field
2008-05-26	JS	Removed onassign since it causes touble
2008-05-26	JS	Extended field_names to return the field names for any specified table, return field names also for db objects that have never been used for a database query and optionally return field types
2008-01-29	JS	->getrecord now supports -sql. Make sure that the SQL statement includes the relevant keyfield (and lockfield if locking is used). 
2008-01-10	JS	->capturesearchvars: error_code and error_msg was mysteriously not set after database operations that caused errors. 
2008-01-08	JS	->saverecord: added flag -keeplock to be able to save a locked record without releasing the lock
2007-12-15	JS	Adding support for knop_user in record locking is in progress. Done for ->oncreate and ->getrecord. 
2007-12-11	JS	Moved error_code and error_msg to knop_base
2007-12-11	JS	Added documentation as -description to most member tags, to be used by the new ->help tag
2007-12-11	JS	Moved ->help to knop_base
2007-12-10	JS	Added ->settable to be able to copy an existing database object and properly set a new table name for it. Faster than creating a new instance from scratch. 
2007-12-03	JS	Corrected shown_first once again, hoping it's right this time
2007-11-29	JS	Added support for field_names and corresponding member tag ->field_names
2007-11-05	JS	Added var name to trace output
2007-10-26	JS	->capturesearchvars: corrected shown_first when no records found
2007-10-26	JS	->oncreate: added default value "keyfield" if the -keyfield parameter is not specified
2007-09-06	JS	Corrected self -> 'tagtime' typo
2007-06-18	JS	Added tag timer to most member tags
2007-06-13	JS	added inheritance from knop_base
2007-06-11	JC	added handling of xhtml output
2007-05-30	JS	Changed recordid_value to keyfield_value and -recordid to -keyvalue
2007-05-28	JS	->oncreate: Added clearing of current error at beginning of tag
2007-04-19	JS	Corrected the handling of -maxrecords and -skiprecords for SQL selects that have LIMIT specified
2007-04-19	JS	Improved handling of foundrows so it finds any whitespace around SQL keywords, instead of just plain spaces
2007-04-18	JS	->select now populates recorddata with all the fields for the first found record. Previously it only populated recorddata when there was 1 found record. 
2007-04-12	JS	->oncreate: Added authentication inline around Database_TableNames../Database_TableNames
2007-04-10	JS	->oncreate: Improved validation of table name (table_realname can sometimes be null even for valid table names)
2007-04-03	JS	Changed namespace from mt_ to knop_
2007-02-02	JS	Improved reporting of Lasso error messaged in error_msg
2007-01-30	JS 	Added real error codes and additional error data for some errors (like record locked)
2007-01-30	JS	Changed -keyvalue parameters to copy value instead of pass as reference, to not cause problems when using keyvalue from the same db object as is being updated, for example $db->(saverecord: -keyvalue=$db->keyvalue)
2007-01-26	JS	Adjusted affectedrecord_keyvalue so it's only captured for -add and -update
2007-01-23	JS	Supports -uselimit (or querys that use LIMIT) and still gets proper searchresult vars (using a separate COUNT(*) query) - may not always get the right result for example for queries with GROUP BY
2007-01-23	JS	-keyfield can be specified for saverecord to override the default
2007-01-23	JS	Changed name of ->updaterecord to ->saverecord
2007-01-23	JS 	Fixed bug where keyfield was missing as returnfield when looking up locked record for deleterecord
2007-01-23	JS	Added ->field
2007-01-19	JS	Added maxrecords_value and skiprecords_value to searchresultvars
2007-01-18	JS	Added affectedrecord_keyvalue to make it possible to highlight affected record in record list (grid)


TODO:
Allow -keyfield to be specified for ->addrecord and ->deleterecord
Add some Active Record similar functionality for editing
Look at making it so -table can be set dynamically instead of fixed at oncreate, to eliminate the need for one db object for each table. This can cause problems with record locks and how they interact with knop_user 
datetime_create and datetime_mod, and also user_create and user_mod.
	Use default field names but allow to override at oncreate, and verify them at oncreate before trying to use them. 


*/

	// instance variables
	// these variables are set once
	local: 'database'=string,
		'table'=string,
		'table_realname'=string,	// the actual table name, to be used in SQL statements (in case the table name is aliased in Lasso)
		'username'=string,
		'password'=string,
		'db_connect'=array,
		'host'=array,				// add support for inline host method
		'datasource_name'=string,
		'isfilemaker'=false,
		'lock_expires'=1800,		// seconds before a record lock expires
		'lock_seed'=knop_seed,		// encryption seed for the record lock
		'error_lang'=(knop_lang: -default='en', -fallback),
		'user'=null,				// knop_user that will be used for record locking
		'databaserows_map'=map;		// map to hold databaserows for each inlinename

	// these variables are set for each query
	local: 'inlinename'=string,			// the inlinename that holds the result of the latest db operation
		'keyfield'=string,
		'keyvalue'=null,
		'affectedrecord_keyvalue'=null,	// keyvalue of last added or updated record (not reset by other db actions)
		'lockfield'=string,
		'lockvalue'=null,
		'lockvalue_encrypted'=null,
		'timestampfield'=string,		// for optimistic locking
		'timestampvalue'=string,
		'searchparams'=string,			// the resulting pair array used in the database action
		'querytime'=integer,			// query time in ms
		// 'tagtime'=integer,		moved to knop_base
		'recorddata'=map,				// for single record results, a map of all returned db fields
		'error_data'=map,				// additional data for certain errors
		'message'=string,				// user message for normal result
		'current_record'=integer,		// index of the current record to get field values from a specific record
		'field_names_map'=map,
		'resultset_count_map'=map;		// resultset_count stored for each inlinename 
	// these vars have directly corresponding Lasso tags so they can be set programatically
	local: 'searchresultvars'=(array: 'action_statement', 'found_count', 'shown_first', 
		'shown_last', 'shown_count', 'field_names', 'records_array', 'maxrecords_value', 'skiprecords_value');
	iterate: #searchresultvars, (local: 'resultvar');
		local(#resultvar = null);
	/iterate;

	local: 'errors_error_data'=(map: 7010, 7012, 7013, 7016, 7018, 7019); // these error codes can have more info in error_data map

	define_tag: 'oncreate',
		-required='database',
		-required='table',
		-optional='host',			// add support for inline host method
		-optional='username',
		-optional='password',
		-optional='keyfield',
		-optional='lockfield',
		-optional='user',
		-optional='validate';		// validate the database connection info (adds the overhead of making a test connection to the database)
		local: 'timer'=knop_timer; 
		
		// reset error
		error_code = 0;
		error_msg = error_noerror;
	
		// validate database and table names to make sure they exist in Lasso
		(self -> 'datasource_name') = Lasso_DatasourceModuleName: #database;
		fail_if: error_code != 0, error_code, error_msg;
				
		// store params as instance variables
		local_defined('database') ? (self -> 'database') = @#database; 
		local_defined('table') ? (self -> 'table') = @#table; 
		local_defined('host') ? (self -> 'host') = @#host;	// add support for inline host method
		local_defined('username') ? (self -> 'username') = @#username; 
		local_defined('password') ? (self -> 'password') = @#password; 
		local_defined('lockfield') ? (self -> 'lockfield') = @#lockfield; 
		local_defined('user') ? (self -> 'user') = @#user;
			// param has default value
		(self -> 'keyfield') = (local_defined('keyfield') 
								? @#keyfield // use parameter value
								| 'keyfield'); // use default value

		
		// build inline connection array
		local_defined('database') ? (self -> 'db_connect') -> insert('-database'  = @#database);
		local_defined('table') ? (self -> 'db_connect') -> insert('-table'  = @#table);
		local_defined('host') ? (self -> 'db_connect') -> insert('-host'  = @#host);	// add support for inline host method
		local_defined('username') ? (self -> 'db_connect') -> insert('-username'  = @#username);
		local_defined('password') ? (self -> 'db_connect') -> insert('-password'  = @#password);

		(self -> 'table_realname') = (table_realname: #database, #table);
		if:  (self -> 'table_realname') == null;
			// verify that the table exists even if table_realname is null
			inline: (self -> 'db_connect');
				Database_TableNames: #database;
					if: Database_TableNameItem == #table;
						(self -> 'table_realname') = #table;
						loop_abort;
					/if;
				/Database_TableNames;
			/inline;
		/if;
		fail_if: (self -> 'table_realname') == null, 7001, self -> error_msg(7001); // The specified table was not found
		
		if: (local_defined: 'validate');
			// validate db connection
			inline: (self -> 'db_connect');
				fail_if: error_code != 0, error_code, error_msg;
			/inline;
		/if;

		if: Lasso_DatasourceIsFilemaker: #database || Lasso_DatasourceIsFilemakerSA: #database;
			(self -> 'isfilemaker') = true;
		/if;
		(self -> 'debug_trace') -> (insert: tag_name + ': creating database object on ' + (self -> 'datasource_name') +', isfilemaker: ' + (self -> 'isfilemaker') + ' at ' + (date -> (format: '%Q %T')));

		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"

	/define_tag;
	
	/* 
	define_tag: 'onassign', -required='value', -description='Internal, needed to restore references when ctype is defined as prototype';
		// recreate references here
		(self -> 'user') = @(#value -> 'user');
	/define_tag;
	*/

	define_tag('_unknowntag', -description='Shortcut to field');
		if((self -> 'field_names_map') >> tag_name);
			return(self -> field(tag_name));
		else;
			//fail(-9948, self -> type + '->' + tag_name + ' not known.');
			(self -> 'debug_trace') -> insert(self -> type + '->' + tag_name + ' not known.');
		/if;
	/define_tag;

	define_tag: 'settable',	-description='Changes the current table for a database object. Useful to be able to create \
		database objects faster by copying an existing object and just change the table name. This is a little bit faster \
		than creating a new instance from scratch, but no table validation is performed. Only do this to add database \
		objects for tables within the same database as the original database object. ',
		-required='table', -type='string';
		local: 'timer'=knop_timer; 
	
		(self -> 'error_code')=0;
		(self -> 'error_msg')=string;
		(self -> 'table_realname') = #table;
		(self -> 'db_connect') -> removeall(#table);
		(self -> 'db_connect') -> (insert: '-table' = #table);
		(self -> 'table_realname') = (table_realname: self -> 'database', #table);
		if:  (self -> 'table_realname') == null;
			// verify that the table exists even if table_realname is null
			inline: (self -> 'db_connect');
				Database_TableNames: (self -> 'database');
					if: Database_TableNameItem == #table;
						(self -> 'table_realname') = #table;
						loop_abort;
					/if;
				/Database_TableNames;
			/inline;
		/if;
		fail_if: (self -> 'table_realname') == null, 7001, self -> error_msg(7001); // The specified table was not found
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'select', -description='perform database query, either Lasso-style pair array or SQL statement.\
			 ->recorddata returns a map with all the fields for the first found record. \
			 If multiple records are returned, the records can be accessed either through ->inlinename or ->records_array.\n\
			Parameters:\n\
			-search (optional array) Lasso-style search parameters in pair array\n\
			-sql (optional string) Raw sql query\n\
			-keyfield (optional) Overrides default keyfield, if any\n\
			-keyvalue (optional)\n\
			-inlinename (optional) Defaults to autocreated inlinename',
		-optional='search', -type='array',
		-optional='sql', -type='string',
		-optional='keyfield',
		-optional='keyvalue', -copy,
		-optional='inlinename', -copy;

		knop_debug(self->type + ' -> ' + tag_name, -open, -type=self->type);
		handle;
			//knop_debug(-close, -witherrors, -type=self->type);
			knop_debug('Done with ' + self->type + ' -> ' + tag_name, -close, -witherrors, -time);
		/handle;
		local: 'timer'=knop_timer; 

		// clear all search result vars
		self -> reset;
		
		local: '_search'=(local: 'search'),
			'_sql'=(local: 'sql');
		if: #_search -> type != 'array';
			#_search = array;
		/if;
		if: #_sql != '' && (self -> 'isfilemaker');
			#_sql='';
			fail: 7009, self -> error_msg(7009); // sql can not be used with filemaker
		/if;
		// inlinename defaults to a random string
		(self -> 'inlinename') = ((local: 'inlinename') != '' ? #inlinename | 'inline_' + knop_unique);
		#_search -> (removeall: -inlinename);
		#_search -> (insert: -inlinename=(self -> 'inlinename'));
		
		// remove all database actions from the search array
		#_search -> (removeall: -search) & (removeall: -add) & (removeall: -delete) & (removeall: -update) 
			& (removeall: -sql) & (removeall: -nothing) & (removeall: -show)
			// & (removeall: -table)  // table is ok to override
			& (removeall: -database);

		if: (local: 'sql') != '' && (string_findregexp: #sql, -find='\\bLIMIT\\b', -ignorecase) -> size;
			(self -> 'debug_trace') -> (insert: tag_name + ': grabbing -maxrecords and -skiprecords from search array');
			// store maxrecords and skiprecords for later use
			if: #_search >> '-maxrecords';
				(self -> 'maxrecords_value') = #_search -> (find: '-maxrecords') -> last -> value;
				(self -> 'debug_trace') -> (insert: tag_name + ': -maxrecords value found in search array ' + (self -> 'maxrecords_value'));
			/if;
			if: #_search >> '-skiprecords';
				(self -> 'skiprecords_value') = #_search -> (find: '-skiprecords') -> last -> value;
				(self -> 'debug_trace') -> (insert: tag_name + ': -skiprecords value found in search array ' + (self -> 'skiprecords_value'));
			/if;
			// remove skiprecords from the actual search parameters since it will conflict with LIMIT
			#_search -> (removeall: '-skiprecords');
		/if;

		if: !(local_defined: 'keyfield') && (self -> 'keyfield') != '';
			local: 'keyfield'=(self -> 'keyfield');
		/if;
		if: (local: 'keyfield') != '';
			#_search -> (removeall: '-keyfield');
			if: !(self -> 'isfilemaker');
				#_search -> (insert: '-keyfield'=#keyfield);
			/if;
			if: (local: 'keyvalue') != '';
				#_search -> (removeall: '-keyvalue');
				if: (self -> 'isfilemaker');
					#_search -> (insert: '-op'='eq');
					#_search -> (insert: #keyfield=#keyvalue);
				else;
					#_search -> (insert: '-keyvalue'=#keyvalue);
				/if;
			/if;
		/if;

		// add sql action or normal search action
		if: #_sql != '';
			#_search -> (insert: '-sql'=#_sql);
		else;
			#_search -> (insert: '-search');
		/if;
		// perform database query, put connection parameters last to override any provided by the search parameters
		//(self -> 'debug_trace') -> (insert: tag_name + ': search ' + #_search);
		local: 'querytimer'=knop_timer;
		inline: #_search,(self -> 'db_connect');
			(self -> 'querytime') = integer: #querytimer;
			(self -> 'searchparams') = #_search;
			(self -> 'debug_trace') -> (insert: tag_name ': action_statement ' + action_statement);
			knop_debug(action_statement, -sql);
			knop_debug(found_count ' found');
			self -> capturesearchvars;
		/inline;

		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		(self -> 'debug_trace') -> (insert: tag_name + ': found ' (self -> 'found_count') + ' records in ' + (self -> 'querytime') + ' ms, tag time ' + (self -> 'tagtime') + ' ms, ' + (self -> error_msg) + ' ' + (self -> error_code));
	/define_tag;


	define_tag: 'addrecord', -description='Add a new record to the database. A random string keyvalue will be generated unless a -keyvalue is specified. \n\
			Parameters:\n\
			-fields (required array) Lasso-style field values in pair array\n\
			-keyvalue (optional) If -keyvalue is specified, it must not already exist in the database. Specify -keyvalue=false to prevent generating a keyvalue. \n\
			-inlinename (optional) Defaults to autocreated inlinename',
		-required='fields', -type='array',
		-optional='keyvalue', -copy,
		-optional='inlinename';
		local: 'timer'=knop_timer; 

		// clear all search result vars
		self -> reset;
		local: '_fields'=#fields;
		
		// remove all database actions from the search array
		#_fields -> (removeall: '-search') & (removeall: '-add') & (removeall: '-delete') & (removeall: '-update') 
			& (removeall: '-sql') & (removeall: '-nothing') & (removeall: '-show')
			// & (removeall: '-table')  // table is ok to override
			& (removeall: '-database');

		inline: (self -> 'db_connect'); // connection wrapper
			if: (local: 'keyvalue') != '' && (local: 'keyvalue') !== false && (self -> 'keyfield')!='';
				// look for existing keyvalue
				inline: -op='eq', (self -> 'keyfield')=#keyvalue, 
					-maxrecords=1,
					-returnfield=(self -> 'keyfield'),
					-search;
					if: found_count > 0;
						(self -> 'error_code') = 7017; // duplicate keyvalue
					else;
						(self -> 'keyvalue') = #keyvalue;
					/if;
				/inline;
			/if;

			
			if: (self -> 'error_code') == 0;
				// proceed to add record

				if: (self -> 'keyfield') != '';
					if: (local: 'keyvalue') == '' && (local: 'keyvalue') !== false;
						(self -> 'debug_trace') -> (insert: tag_name + ': generating keyvalue');
						// create unique keyvalue
						(self -> 'keyvalue')=knop_unique;
					/if;
					#_fields -> (removeall: (self -> 'keyfield'));
					#_fields -> (removeall: '-keyfield') & (removeall: '-keyvalue');
					#_fields -> (insert: '-keyfield'=(self -> 'keyfield'));
					if: (local: 'keyvalue') !== false;
						#_fields -> (insert: (self -> 'keyfield')=(self -> 'keyvalue'));
					/if;
				/if;

				// inlinename defaults to a random string
				(self -> 'inlinename') = ((local: 'inlinename') != '' ? #inlinename | 'inline_' + knop_unique);
				#_fields -> (removeall: '-inlinename');
				#_fields -> (insert: '-inlinename'=(self -> 'inlinename'));
				
				local: 'querytimer'=knop_timer;
				inline: #_fields, -add;
					(self -> 'querytime') = integer: #querytimer;
					(self -> 'searchparams') = #_fields;
				
					self -> capturesearchvars;
					if: error_code != 0;
						(self -> 'keyvalue') = null;
					/if;
				/inline;
			/if;
		/inline;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		(self -> 'debug_trace') -> (insert: tag_name + ': ' + (self -> error_msg) + ' ' + (self -> error_code) 
			+ ' keyvalue ' + (self -> 'keyvalue') + ' ' + (self -> 'tagtime') + ' ms');
	/define_tag;


	define_tag: 'getrecord', -description='Returns a single specific record from the database, optionally locking the record. \
			If the keyvalue matches multiple records, an error is returned. \n\
			Parameters:\n\
			-keyvalue (optional) Uses a previously set keyvalue if not specified. If no keyvalue is available, an error is returned unless -sql is used. \n\
			-keyfield (optional) Temporarily override of keyfield specified at oncreate\n\
			-inlinename (optional) Defaults to autocreated inlinename\n\
			-lock (optional flag) If flag is specified, a record lock will be set\n\
			-user (optional) The user who is locking the record (required if using lock)\n\
			-sql (optional) SQL statement to use instead of keyvalue. Must include the keyfield (and lockfield of locking is used).',
		-optional='keyvalue', -copy,
		-optional='keyfield',
		-optional='inlinename', -copy,
		-optional='lock',
		-optional='user', -copy,
		-optional='sql', -type='string';
		local: 'timer'=knop_timer; 

		local: '_sql'=(local: 'sql');

		if: #_sql != '' && (self -> 'isfilemaker');
			#_sql='';
			fail: 7009, self -> error_msg(7009); // sql can not be used with filemaker
		/if;
		
		// get existing record pointer if any
		if: #_sql -> size == 0 && !(local_defined: 'keyvalue');
			local: 'keyvalue'=(self -> 'keyvalue');
		else: !(local_defined: 'keyvalue');
			local: 'keyvalue'=string;
		/if;

		// clear all search result vars
		self -> reset;
		
		fail_if: !(local_defined: 'keyfield') && (self -> 'keyfield') == '', 7002, self -> error_msg(7002); // Keyfield not specified
		if: (local_defined: 'lock') && #lock != false;
			fail_if: (self -> 'lockfield') == '', 7003, self -> error_msg(7003); // Lockfield must be specified to get record with lock
			if: !(local_defined: 'user') && ((self -> 'user') != '' || (self -> 'user') -> isa('user'));
				// use user from database object
				local('user' = (self -> 'user'));
			/if;
			fail_if: (local: 'user') == '' && !((local: 'user') -> isa('user')), 7004, self -> error_msg(7004); // User must be specified to get record with lock
			(self -> 'debug_trace') -> insert(tag_name ': user is type ' + (#user -> type) + ', isa(user) = ' + (#user -> isa('user')) );
			if: #user -> isa('user');
				#user= #user -> id_user;
				fail_if: #user == '', 7004, self -> error_msg(7004); // User must be logged in to get record with lock
			/if;
			(self -> 'debug_trace') -> insert(tag_name ': user id is ' + #user);
		/if;
		if: !(local_defined: 'keyfield') && (self -> 'keyfield') != '';
			local: 'keyfield'=(self -> 'keyfield');
		/if;
		if:  #_sql -> size == 0 && string(#keyvalue) -> size == 0;
			(self -> 'error_code') = 7007; // keyvalue missing
		/if;
		if: (self -> 'error_code') == 0;
			inline: (self -> 'db_connect'); // connection wrapper

				if: #_sql -> size;
					self -> (select: -sql=#_sql, -inlinename=(local: 'inlinename'));
					#keyvalue = (self -> 'keyvalue');
				else;
					self -> (select: -keyfield=#keyfield, -keyvalue=#keyvalue, -inlinename=(local: 'inlinename'));
				/if;
				if: (self -> field_names) !>> #keyfield;
					(self -> 'error_code') = 7020; // Keyfield not present in query
				/if;
				if: (self -> field_names) !>> (self -> 'lockfield') && (local_defined: 'lock') && #lock != false;
					(self -> 'error_code') = 7021; // Lockfield not present in query
				/if;
				
				if: (self -> 'found_count') == 0 && (self -> 'error_code') == 0;
					(self -> 'error_code') = -1728;
				else: (self -> 'found_count') > 1 && (self -> 'error_code') == 0;
					self -> reset;
					(self -> 'error_code') = 7008; // keyvalue not unique
				/if;
		
		
				// handle record locking
				if: (self -> 'error_code') == 0 && (local_defined: 'lock') && #lock != false;
					// check for current lock
					if: (self -> 'lockvalue') != '';
						// there is a lock already set, check if it has expired or if it is the same user
						local: 'lockvalue'=(self -> 'lockvalue') -> (split: '|');
						local: 'lock_timestamp'=date: (#lockvalue->size > 1 ? #lockvalue -> (get: 2) | null);
						local: 'lock_user'=#lockvalue -> first;
						if: (date - #lock_timestamp) -> seconds < (self -> 'lock_expires')
							&& #lock_user != #user;
							// the lock is still valid and it is locked by another user
							// this is not a real error, more a warning condition
							(self -> 'error_code') = 7010; 
							(self -> 'error_data') = (map: 'user' = #lock_user, 'timestamp' = #lock_timestamp);
							(self -> 'keyvalue') = null;
							(self -> 'debug_trace') -> (insert: tag_name ': record ' + #keyvalue + ' was already locked by ' + #lock_user + '.');
						/if;
					/if;
					if: (self -> 'error_code') == 0;
						// go ahead and lock record
						(self -> 'lockvalue') = #user + '|' + (date -> format: '%Q %T');
						(self -> 'lockvalue_encrypted') = (encrypt_blowfish: (self -> 'lockvalue'), -seed=(self -> 'lock_seed'));
						local: 'keyvalue_temp'=#keyvalue;
						if: (self -> 'isfilemaker');
							// find internal keyvalue
							inline: -op='eq', #keyfield=#keyvalue,
								-search;
								if: found_count == 1;
									#keyvalue_temp=keyfield_value;
									(self -> 'debug_trace') -> (insert: tag_name + ': will set record lock for FileMaker record id ' + keyfield_value + ' ' + error_msg + ' ' + error_code);
								else;
									(self -> 'debug_trace') -> (insert: tag_name + ': could not get record id for FileMaker record, ' found_count + ' found ' + + error_msg + ' ' + error_code);
								/if;
							/inline;
						/if;
						inline: -keyfield=#keyfield,
							-keyvalue=#keyvalue_temp, 
							(self -> 'lockfield')=(self -> 'lockvalue'),
							-update;
							if: error_code;
								(self -> 'error_code') = 7012; // could not set record lock
								(self -> 'error_data') = (map: 'error_code'=error_code, 'error_msg'=error_msg);
								(self -> 'lockvalue') = null;
								(self -> 'lockvalue_encrypted') = null;
								(self -> 'keyvalue') = null;
							else;
								// lock was set ok
								(self -> 'debug_trace') -> (insert: tag_name + ': set record lock ' + (self -> 'lockvalue') + ' ' + (self -> 'lockvalue_encrypted'));
								if: (self -> 'user') -> isa('user');
									// tell user it has locked a record in this db object
									(self -> 'user') -> addlock(-dbname=self -> varname);
								/if;
							/if;
						/inline;
					/if;
				/if;
				
			/inline;
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		(self -> 'debug_trace') -> (insert: tag_name + ': ' + (self -> error_msg) + ' ' + (self -> error_code) + ' ' + (self -> 'tagtime') + ' ms');
	/define_tag;


	define_tag: 'saverecord', -description='Updates a specific database record. \n\
			Parameters:\n\
			-fields (required array) Lasso-style field values in pair array\n\
			-keyfield (optional) Keyfield is ignored if lockvalue is specified\n\
			-keyvalue (optional) Keyvalue is ignored if lockvalue is specified\n\
			-lockvalue (optional) Either keyvalue or lockvalue must be specified\n\
			-keeplock (optional flag) Avoid clearing the record lock when saving. Updates the lock timestamp.\n\'
			-user (optional) If lockvalue is specified, user must be specified as well\n\
			-inlinename (optional) Defaults to autocreated inlinename',
		-required='fields', -type='array',
		-optional='keyfield',
		-optional='keyvalue', -copy,
		-optional='lockvalue', -copy,
		-optional='keeplock',
		-optional='user', -copy,
		-optional='inlinename', -copy;
		
		local: 'timer'=knop_timer; 

		if(!local_defined('keyvalue') && string(self -> 'keyvalue') -> size);
			// use current record's keyvalue if any
			local('keyvalue'=(self -> 'keyvalue'));
		/if;

		// clear all search result vars
		self -> reset;

		fail_if: !(local_defined: 'keyvalue') && !(local_defined: 'lockvalue'), 7005, self -> error_msg(7005); // Either keyvalue or lockvalue must be specified for update or delete
		fail_if: (local_defined: 'keyvalue') && (self -> 'keyfield') == '' && (local: 'keyfield') == '', 7002, self -> error_msg(7002); // Keyfield not specified
		if: (local_defined: 'lockvalue');
			fail_if: (self -> 'lockfield') == '', 7003, self -> error_msg(7003); // Lockfield not specified
			if: !(local_defined: 'user') && ((self -> 'user') != '' || (self -> 'user') -> isa('user'));
				// use user from database object
				local('user' = (self -> 'user'));
			/if;
			fail_if: (local: 'user') == '' && !((local: 'user') -> isa('user')), 7004, self -> error_msg(7004); 
			(self -> 'debug_trace') -> insert(tag_name ': user is type ' + (#user -> type) + ', isa(user) = ' + (#user -> isa('user')) );
			if: #user -> isa('user');
				#user= #user -> id_user;
				fail_if: #user == '', 7004, self -> error_msg(7004); // User must be logged in to get record with lock
			/if;
			(self -> 'debug_trace') -> insert(tag_name ': user id is ' + #user);
		/if;
		
		!(local_defined: 'keyfield') ? local: 'keyfield'=self -> 'keyfield';
		
		local: '_fields'=#fields;
		
		// remove all database actions from the search array
		#_fields -> (removeall: '-search') & (removeall: '-add') & (removeall: '-delete') & (removeall: '-update') 
			& (removeall: '-sql') & (removeall: '-nothing') & (removeall: '-show')
			// & (removeall: '-table') // table is ok to override
			& (removeall: '-database');
		#_fields -> (removeall: '-keyfield') & (removeall: '-keyvalue');

		inline: (self -> 'db_connect'); // connection wrapper

			// handle record locking
			if: (self -> 'error_code') == 0 && (local: 'lockvalue') != '';

				// first check if record was locked by someone else, and that lock is still valid
				local: 'lock'=(decrypt_blowfish: #lockvalue, -seed=(self -> 'lock_seed')) -> (split: '|');
				local: 'lock_timestamp'=date: (#lock->size > 1 ? (#lock -> (get: 2)) | null);
				local: 'lock_user'=#lock -> first;
				if: (date - #lock_timestamp) -> seconds < (self -> 'lock_expires')
					&& #lock_user != #user;
					// the lock is still valid and it is locked by another user
					(self -> 'error_code') = 7010; 
					(self -> 'error_data') = (map: 'user' = #lock_user, 'timestamp' = #lock_timestamp);
				/if;

				// check that the current lock is still valid
				if: (self -> 'error_code') == 0;
					inline: -op='eq', (self -> 'lockfield')=#lock -> (join: '|'), 
						-maxrecords=1,
						-returnfield=(self -> 'lockfield'),
						-returnfield=(self -> 'keyfield'),
						-search;
						if: error_code == 0 && found_count != 1;
							// lock is not valid any more
							(self -> 'error_code') = 7011; // Update failed, record lock not valid any more
						else: error_code != 0;
							(self -> 'error_code') = 7018; // Update error
							(self -> 'error_data') = (map: 'error_code'=error_code, 'error_msg'=error_msg);
						else;
							// lock OK, grab keyvalue for update
							local: 'keyvalue'=(field: (self -> 'keyfield'));
						/if;
					/inline;
				/if;
				
				if: (self -> 'error_code') == 0;
					// go ahead and release record lock by clearing the field value in the update fields array
					#_fields -> (removeall: (self -> 'lockfield'));
					if: ((local_defined: 'keeplock') && #keeplock != false);
						// update the lock timestamp
						(self -> 'lockvalue') = #user + '|' + (date -> format: '%Q %T');
						(self -> 'lockvalue_encrypted') = (encrypt_blowfish: (self -> 'lockvalue'), -seed=(self -> 'lock_seed'));
						#_fields -> (insert: (self -> 'lockfield')=(self -> 'lockvalue'));
					else;
						#_fields -> (insert: (self -> 'lockfield') = '');
					/if;
				/if;

			/if;

			if: (self -> 'error_code') == 0 && (local: 'keyvalue') != '';
				if: (self -> 'isfilemaker');
					inline: -op='eq', #keyfield=#keyvalue, -search;
						if: found_count == 1;
							#_fields -> (insert: '-keyvalue'=keyfield_value);
							(self -> 'debug_trace') -> (insert: tag_name + ': FileMaker record id ' + keyfield_value);
						/if;
					/inline;
				else;
					#_fields -> (insert: '-keyfield'=#keyfield);
					#_fields -> (insert: '-keyvalue'=#keyvalue);
				/if;
			/if;
			
			
			
			if: (#_fields >> '-keyfield' && #_fields -> (find: '-keyfield') -> first -> value != '' || (self -> 'isfilemaker'))
				&& #_fields >> '-keyvalue' && #_fields -> (find: '-keyvalue') -> first -> value != '';
				// ok to update
			else: (self -> 'error_code') == 0;
				(self -> 'error_code') = 7006; // Update failed, keyfield or keyvalue missing';
			/if;

			// update record
			if: (self -> 'error_code') == 0;

				// inlinename defaults to a random string
				(self -> 'inlinename') = ((local: 'inlinename') != '' ? #inlinename | 'inline_' + knop_unique);
				#_fields -> (removeall: '-inlinename');
				#_fields -> (insert: '-inlinename'=(self -> 'inlinename'));
	
				local: 'querytimer'=knop_timer;
				inline: #_fields, -update;
					(self -> 'querytime') = integer: #querytimer;
					(self -> 'searchparams') = #_fields;
					self -> capturesearchvars;
				/inline;
			/if;
		/inline;
	
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		(self -> 'debug_trace') -> (insert: tag_name + ': ' + (self -> 'keyvalue') + ' '+ (self -> error_msg) + ' ' + (self -> error_code) + ' ' + (self -> 'tagtime') + ' ms');
	/define_tag;


	define_tag: 'deleterecord', -description='Deletes a specific database record. \n\
			Parameters:\n\
			-keyvalue (optional) Keyvalue is ignored if lockvalue is specified\n\
			-lockvalue (optional) Either keyvalue or lockvalue must be specified\n\
			-user (optional) If lockvalue is specified, user must be specified as well',
		-optional='keyvalue', -copy,
		-optional='lockvalue', -copy,
		-optional='user';
		local: 'timer'=knop_timer; 

		if(!local_defined('keyvalue') && string(self -> 'keyvalue') -> size);
			// use current record's keyvalue if any
			local('keyvalue'=(self -> 'keyvalue'));
		/if;

		// clear all search result vars
		self -> reset;

		fail_if: !(local_defined: 'keyvalue') && !(local_defined: 'lockvalue'), 7005,  self -> error_msg(7005); // Either keyvalue or lockvalue must be specified for update or delete
		fail_if: (local_defined: 'keyvalue') && (self -> 'keyfield') == '', 7002,  self -> error_msg(7002); // Keyfield not specified
		if: (local_defined: 'lockvalue');
			fail_if: (self -> 'lockfield') == '', 7003,  self -> error_msg(7003); //  Lockfield not specified
			if: !(local_defined: 'user') && ((self -> 'user') != '' || (self -> 'user') -> isa('user'));
				// use user from database object
				local('user' = (self -> 'user'));
			/if;
			fail_if: (local: 'user') == '' && !((local: 'user') -> isa('user')), 7004, self -> error_msg(7004); 
			(self -> 'debug_trace') -> insert(tag_name ': user is type ' + (#user -> type) + ', isa(user) = ' + (#user -> isa('user')) );
			if: #user -> isa('user');
				#user= #user -> id_user;
				fail_if: #user == '', 7004, self -> error_msg(7004); // User must be logged in to get record with lock
			/if;
			(self -> 'debug_trace') -> insert(tag_name ': user id is ' + #user);
		/if;
		
		local: '_fields'=array;
		
		inline: (self -> 'db_connect'); // connection wrapper

			// handle record locking
			if: (self -> 'error_code') == 0 && (local: 'lockvalue') != '';

				// first check if record was locked by someone else, and that lock is still valid
				local: 'lockvalue'=(decrypt_blowfish: #lockvalue, -seed=(self -> 'lock_seed')) -> (split: '|');
				local: 'lock_timestamp'=date: (#lockvalue->size > 1 ? #lockvalue -> (get: 2) | null);
				local: 'lock_user'=(#lockvalue -> first);
				if: (date - #lock_timestamp) -> seconds < (self -> 'lock_expires')
					&& #lock_user != #user;
					// the lock is still valid and it is locked by another user
					(self -> 'error_code') = 7010; // Delete failed, record locked 
					(self -> 'error_data') = (map: 'user' = #lock_user, 'timestamp' = #lock_timestamp);
				/if;

				// check that the current lock is still valid
				if: (self -> 'error_code') == 0;
					inline: -op='eq', (self -> 'lockfield')=#lockvalue -> (join: '|'), 
						-maxrecords=1,
						-returnfield=(self -> 'lockfield'),
						-returnfield=(self -> 'keyfield'),
						-search;
						if: error_code == 0 && found_count != 1;
							// lock is not valid any more
							(self -> 'error_code') = 7011; // Delete failed, record lock not valid any more';
						else: error_code != 0;
							(self -> 'error_code') = 7019; // delete error
							(self -> 'error_data') = (map: 'error_code'=error_code, 'error_msg'=error_msg);
						else;
							// lock OK, grab keyvalue for update
							local: 'keyvalue'=(field: (self -> 'keyfield'));
							(self -> 'debug_trace') -> (insert: tag_name + ': got keyvalue ' + #keyvalue + ' for keyfield ' + (self -> 'keyfield'));
						/if;
					/inline;
				/if;
				
			/if;

			if: (self -> 'error_code') == 0 && (local: 'keyvalue') != '';
				if: (self -> 'isfilemaker');
					inline: -op='eq', (self -> 'keyfield')=#keyvalue, -search;
						if: found_count == 1;
							#_fields -> (insert: '-keyvalue'=keyfield_value);
							(self -> 'debug_trace') -> (insert: tag_name + ': FileMaker record id ' + keyfield_value);
						/if;
					/inline;
				else;
					#_fields -> (insert: '-keyfield'=(self -> 'keyfield'));
					#_fields -> (insert: '-keyvalue'=#keyvalue);
				/if;
			/if;
			
			(self -> 'debug_trace') -> (insert: tag_name + ': will delete record with params ' + #_fields);
			
			if: (#_fields >> '-keyfield' && #_fields -> (find: '-keyfield') -> first -> value != '' || (self -> 'isfilemaker'))
				&& #_fields >> '-keyvalue' && #_fields -> (find: '-keyvalue') -> first -> value != '';
				// ok to delete
			else;
				(self -> 'error_code') = 7006; // Delete failed, keyfield or keyvalue missing
			/if;
			
			// delete record
			if: (self -> 'error_code') == 0;
	
				local: 'querytimer'=knop_timer;
				inline: #_fields, -delete;
					(self -> 'querytime') = integer: #querytimer;
					(self -> 'searchparams') = #_fields;
				
					self -> capturesearchvars;
				
				/inline;
			/if;
		/inline;
	
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		(self -> 'debug_trace') -> (insert: tag_name + ': ' + (self -> error_msg) + ' ' + (self -> error_code) + ' ' + (self -> 'tagtime') + ' ms');
	/define_tag;


	define_tag: 'clearlocks', -description='Release all record locks for the specified user, suitable to use when showing record list. \n\
			Parameters:\n\
			-user (required) The user to unlock records for',
		-required='user';
		// release all record locks for the specified user, suitable to use when showing record list
		local: 'timer'=knop_timer; 

		fail_if: (self -> 'lockfield') == '', 7003,  self -> error_msg(7003); //  Lockfield not specified
		fail_if: #user == '', 7004, self -> error_msg(7004); // User not specified
	
		if: (self -> 'isfilemaker');
			inline: (self -> 'db_connect'),
				-maxrecords=all,
				(self -> 'lockfield')='"' + #user + '|"',
				-search;
				if: found_count > 0;
					(self -> 'debug_trace') -> (insert: tag_name + ': clearing locks for ' + #user + ' in ' + found_count + ' FileMaker records ' + error_msg + ' ' + error_code);
					records;
						inline: -keyvalue=keyfield_value,
							(self -> 'lockfield')='',
							-update;
							if: error_code;
								(self -> 'error_code') = 7013; // Clearlocks failed
								(self -> 'error_data') = (map: 'error_code'=error_code, 'error_msg'=error_msg);
								(self -> 'debug_trace') -> (insert: tag_name + ': error when clearing lock on FileMaker record ' + keyfield_value + ' ' + error_msg + ' ' + error_code);
								return;
							/if;
						/inline;
					/records;
				else: error_code;
					(self -> 'error_code') = 7013; // Clearlocks failed
					(self -> 'error_data') = (map: 'error_code'=error_code, 'error_msg'=error_msg);
				/if;
			/inline;
		else;
			inline: (self -> 'db_connect'),
				-sql='UPDATE `' + (self -> 'table_realname') + '` SET `' + (self -> 'lockfield') + '`=""  WHERE `' + (self -> 'lockfield') 
					+ '` LIKE "' + (encode_sql: #user) + '|%"';
				if: error_code != 0;
					(self -> 'error_code') = 7013; // Clearlocks failed
					(self -> 'error_data') = (map: 'error_code'=error_code, 'error_msg'=error_msg);
				/if;
			/inline;
			(self -> 'debug_trace') -> (insert: tag_name + ': clearing all locks for ' + #user + ' ' + (self -> error_msg) + ' ' + (self -> error_code));
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	define_tag: 'action_statement';		return: (self -> 'action_statement');	/define_tag;
	define_tag: 'found_count';			return: (self -> 'found_count');		/define_tag;
	define_tag: 'shown_count';			return: (self -> 'shown_count');		/define_tag;
	define_tag: 'shown_first';			return: (self -> 'shown_first');		/define_tag;
	define_tag: 'shown_last';			return: (self -> 'shown_last');			/define_tag;
	define_tag: 'maxrecords_value';		return: (self -> 'maxrecords_value');	/define_tag;
	define_tag: 'skiprecords_value';	return: (self -> 'skiprecords_value');	/define_tag;
	define_tag: 'keyfield';				return: (self -> 'keyfield');			/define_tag;
	define_tag: 'keyvalue';				return: (self -> 'keyvalue');			/define_tag;
	define_tag: 'lockfield';			return: (self -> 'lockfield');			/define_tag;
	define_tag: 'lockvalue';			return: (self -> 'lockvalue');			/define_tag;
	define_tag: 'lockvalue_encrypted';	return: (self -> 'lockvalue_encrypted'); /define_tag;
	define_tag: 'querytime';			return: (self -> 'querytime');			/define_tag;
	define_tag: 'inlinename';			return: (self -> 'inlinename');			/define_tag;
	define_tag: 'searchparams';			return: (self -> 'searchparams');		/define_tag;
	define_tag: 'resultset_count',
		-optional='inlinename';
		!local_defined('inlinename') ? local('inlinename'=(self -> 'inlinename'));
		return((self -> 'resultset_count_map') -> find(#inlinename));
	/define_tag;

	define_tag('recorddata', -description='A map containing all fields, only available for single record results',
		-optional='recordindex', -copy);
		!local_defined('recordindex') ? local('recordindex'=(self -> 'current_record'));
		#recordindex < 1 ? #recordindex = 1;
		if(#recordindex == 1);
			// return default (i.e. first) record
			return(self -> 'recorddata');
		else;
			local('recorddata'=map);
			iterate(self -> field_names, local('field_name'));
				#recorddata -> insert(#field_name  =  (self -> 'records_array' -> get(#recordindex) 
					-> get(self -> 'field_names_map' -> find(#field_name))));
			/iterate;
			return(#recorddata);
		/if;
	/define_tag;

	define_tag: 'records_array';		return: (self -> 'records_array');		/define_tag;
	
	define_tag('field_names', -description='Returns an array of the field names from the last database query. If no database query has been performed, a "-show" request is performed. \n\
			Parameters: \n\
			-table (optional) Return the field names for the specified table\n\
			-types (optional flag) If specified, returns a pair array with fieldname and corresponding Lasso data type',
		-optional='table',
		-optional='types');
		!local_defined('table') ? local('table'=(self -> 'table'));
		local('field_names'=(self -> 'field_names'));
		if(#field_names -> size == 0 || (local_defined('types') && #types != false));
			#field_names=array;
			if(local_defined('types') && #types != false);
				local('types_mapping'=map('text'='string', 'number'='decimal', 'date/time'='date'));
			/if;
			inline(self->'db_connect', -table=#table, -show);
				if(local_defined('types') && #types != false);
					loop(field_name(-count));
						#field_names -> insert(field_name(loop_count) = #types_mapping->find(field_name(loop_count, -type)));
					/loop;
				else;
					#field_names=field_names;
				/if;
			/inline;
		/if;
		return(@#field_names);
	/define_tag;
	
	define_tag('table_names', -description='Returns an array with all table names for the database');
		local('table_names'=array);
		inline(self -> 'db_connect');
			Database_TableNames(self -> 'database');
				#table_names -> insert(Database_TableNameItem);
			/Database_TableNames;
		/inline;
		return(@#table_names);
	/define_tag;
	
	define_tag: 'error_data', -description='Returns more info for those errors that provide such';
		if: (self -> 'errors_error_data') >> (self -> error_code);
			return: (self -> 'error_data');
		else;
			return: map;
		/if;
	/define_tag;

	define_tag('size');
		return(self -> 'shown_count');
	/define_tag;

	define_tag('get', -required='index');
		return(knop_databaserow(
			-record_array=(self -> 'records_array' -> get(#index)), 
			-field_names=(self -> 'field_names')));
	/define_tag;

	define_tag('records', -description='Returns all found records as a knop_databaserows object',
		-optional='inlinename');
		!local_defined('inlinename') ? local('inlinename'=(self -> 'inlinename'));
		if((self -> 'databaserows_map') !>> #inlinename);
			// create knop_databaserows on demand
			(self -> 'databaserows_map') -> insert(#inlinename = knop_databaserows(
					-records_array=(self -> 'records_array'), 
					-field_names=(self -> 'field_names'))
				);
		/if;
		return(@((self -> 'databaserows_map') -> find(#inlinename)));
	/define_tag;

	define_tag('field', -description='A shortcut to return a specific field from a single record result',
		-required='fieldname',
		-optional='recordindex',
		-optional='index');
		!local_defined('recordindex') ? local('recordindex'=(self -> 'current_record'));
		#recordindex < 1 ? #recordindex = 1;
		!local_defined('index') ? local('index'=1);
		if(#recordindex == 1 && #index == 1);
			// return first field occurrence from the default (i.e. first) record
			return((self -> 'recorddata') -> find(#fieldname));
		else(self -> 'field_names_map' >> #fieldname 
			&& #recordindex >= 1 
			&& #recordindex <= (self -> 'records_array') -> size);
			// return specific record
			if(#index==1);
				// return first ocurrence of field name through the index map - this is faster
				return(self -> 'records_array' -> get(#recordindex) -> get(self -> 'field_names_map' -> find(#fieldname)));
			else;
				// return another occurrence of the field - this is slightly slower
				local('indexmatches'=(self -> 'field_names') -> findposition(#fieldname));
				if(#index >= 1 && #index <= #indexmatches -> size);
					return(self -> 'records_array' -> get(#recordindex) -> get(#indexmatches -> get(#index)));
				/if;
			/if;
		/if;
	/define_tag;
	
	define_tag('next', -description='Increments the record pointer, returns true if there are more records to show, false otherwise.\n\
			Useful as an alternative to a regular records loop:\n\
			\t$database -> select;\n\
			\twhile: $database -> next;\n\
			\t\t$database -> field(\'name\');\'<br>\';\n\
			\t/while;');
		if((self -> 'current_record') < (self -> 'shown_count'));
			(self -> 'current_record') += 1;
			return(true);
		else;
			// reset record pointer
			(self -> 'current_record') = 0;
			return(false);
		/if;
	/define_tag;

	define_tag('nextrecord', -description='Deprecated synonym for ->next');
		(self -> 'debug_trace') -> insert('*** DEPRECATION WARNING *** ' + tag_name + ' is deprecated, use ->next instead ');
		return(self -> next);
	/define_tag;

	define_tag: 'trace', 
		-optional='html',
		-optional='xhtml';

		local: 'endslash' = ((self -> (xhtml: params)) ? ' /' | '');

		local: 'eol'=(local_defined: 'html') || #endslash -> size ? '<br' + #endslash + '>\n' | '\n';

		return: #eol + 'Debug trace for database $' + (self -> varname) + ' (' (self -> 'database') '.' (self -> 'table') + ')' +  #eol 
			+ (self -> 'debug_trace') -> (join: #eol) + #eol;

	/define_tag;


	// =========== Internal member tags ===============
	
	define_tag: 'reset', -description='Internal, reset all search result vars';
		// reset all search result vars
		// searchresultvars
		(self -> 'action_statement') = null;
		(self -> 'found_count') = null;
		(self -> 'shown_first') = null;
		(self -> 'shown_last') = null;
		(self -> 'shown_count') = null;
		(self -> 'field_names') = null;
		(self -> 'records_array') = null;
		(self -> 'maxrecords_value') = null;
		(self -> 'skiprecords_value') = null;
		
		(self -> 'inlinename')=string;
		(self -> 'keyvalue')=null;
		(self -> 'lockvalue')=null;
		(self -> 'lockvalue_encrypted')=null;
		(self -> 'timestampfield')=string;
		(self -> 'timestampvalue')=string;
		(self -> 'searchparams')=string;
		(self -> 'querytime')=integer;
		(self -> 'recorddata')=map;
		(self -> 'message')=string;
		(self -> 'current_record')=0;
		(self -> 'field_names_map')=map;

		(self -> 'error_code')=0;
		(self -> 'error_msg')=string;
	/define_tag;

	define_tag: 'capturesearchvars', -description='Internal';
		// internal member tag

		// capture various result variables like found_count, shown_first, shown_last, shown_count
		// searchresultvars
		(self -> 'action_statement') = action_statement;
		(self -> 'found_count') = found_count;
		(self -> 'shown_first') = shown_first;
		(self -> 'shown_last') = shown_last;
		(self -> 'shown_count') = shown_count;
		(self -> 'field_names') = field_names;
		(self -> 'records_array') = records_array;
	
		!((self -> 'maxrecords_value') > 0) ? (self -> 'maxrecords_value') = maxrecords_value;
		!((self -> 'skiprecords_value') > 0) ? (self -> 'skiprecords_value') = skiprecords_value;

		lasso_tagexists('resultset_count') ? (self -> 'resultset_count_map') -> insert((self -> 'inlinename')=resultset_count);
		iterate(field_names, local('field_name'));
			(self -> 'field_names_map') !>> #field_name 
				? (self -> 'field_names_map') -> insert(#field_name=loop_count);
		/iterate;
		
		(self -> 'error_code') = error_code;
		error_code && error_msg -> size ? (self -> 'error_msg') = error_msg;
		

		// handle queries that use LIMIT
		if: !(self -> 'isfilemaker') && (string_findregexp: action_statement, -find= '\\sLIMIT\\s', -ignorecase) -> size;
			(self -> 'debug_trace') -> (insert: tag_name + ': old found_count, shown_first and shown_last ' + (self -> 'found_count') + ' '+ (self -> 'shown_first') + ' '+ (self -> 'shown_last'));
			(self -> 'found_count') = knop_foundrows;
			// adjust shown_first and shown_last
			(self -> 'shown_first') = ((self -> 'found_count') ? (self -> 'skiprecords_value') + 1 | 0);
			(self -> 'shown_last') = integer(math_min(((self -> 'skiprecords_value') + (self -> 'maxrecords_value')), (self -> 'found_count')));
			(self -> 'debug_trace') -> (insert: tag_name + ': new found_count, shown_first and shown_last ' + (self -> 'found_count') + ' '+ (self -> 'shown_first') + ' '+ (self -> 'shown_last'));
		/if;

		// capture some variables for single record results
		if: found_count <= 1  // -update gives found_count 0 but still has one record result
			&& error_code == 0;
			if((self -> 'keyfield') != '' && string(field(self -> 'keyfield')) -> size);
				(self -> 'keyvalue')=field(self -> 'keyfield');
			else: (self -> 'keyfield') != '' && (self -> 'keyvalue') == '' && !(self -> 'isfilemaker');
				(self -> 'keyvalue')=keyfield_value;
			/if;
			if: lasso_currentaction == 'add' || lasso_currentaction == 'update';
				(self -> 'affectedrecord_keyvalue') = (self -> 'keyvalue');
			/if;
			if: (self -> 'lockfield') != ''; 
				(self -> 'lockvalue')=(field: (self -> 'lockfield'));
				(self -> 'lockvalue_encrypted')=(encrypt_blowfish: (field: (self -> 'lockfield')), -seed=(self -> 'lock_seed'));
			/if;
		/if;
		if: error_code == 0;
			// populate recorddata with field values from the first found record
			iterate: field_names, local: 'field_name';
				(self -> 'recorddata') !>> #field_name 
					? (self -> 'recorddata') -> (insert: #field_name  =  (field: #field_name) );
			/iterate;
		else;
			(self -> 'debug_trace') -> (insert: tag_name + ': ' + error_msg);
		/if;
		(self -> 'debug_trace') -> (insert: tag_name + ': found_count ' + (self -> 'found_count') + ' ' + (self -> 'keyfield') + ' '+ (field: (self -> 'keyfield')) + ' keyfield_value ' + keyfield_value + ' keyvalue ' + (self -> 'keyvalue') + ' fieldcount ' + (field_name: -count));

	/define_tag;

/define_type;


define_type('databaserows',
	-namespace='knop_');
	local('version'='2009-01-08',
		'description'='Custom type to return all record rows from knop_database. Used as output for knop_database->records. ');
/*

CHANGE NOTES
2009-01-08	JS	->_unknowntag: Added -index parameter
2008-11-24	JS	Created the type


*/

	local('records_array'=array,
		'field_names'=array,
		'field_names_map'=map,
		'current_record'=integer);
		
	define_tag('oncreate', -description='Create a record rows object. \n\
			Parameters:\n\
			-records_array (array) Array of arrays with field values for all fields for each record of all found records
			-field_names (array) Array with all the field names',
		-required='records_array',
		-required='field_names');
		self -> 'records_array'=#records_array;
		self -> 'field_names'=#field_names;
		// store indexes to first occurrence of each field name for faster access
		iterate(#field_names, local('field_name'));
			(self -> 'field_names_map') !>> #field_name 
				? (self -> 'field_names_map') -> insert(#field_name=loop_count);
		/iterate;
	/define_tag;
	
	define_tag('_unknowntag', -description='Shortcut to field',
		-optional='index');
		!local_defined('index') ? local('index'=1);
		if(self -> 'field_names' >> tag_name);
			return(self -> field(tag_name(-index=#index)));
		else;
			//fail: -9948, self -> type + '->' + tag_name + ' not known.';
		/if;
	/define_tag;

	define_tag('onconvert', -description='Output the current record as a plain array of field values');
		!local_defined('recordindex') ? local('recordindex'=(self -> 'current_record'));
		#recordindex < 1 ? #recordindex = 1;
		if(#recordindex >= 1 
			&& #recordindex <= (self -> 'records_array' -> size));
			return(self -> 'records_array' -> get(#recordindex));
		/if;
	/define_tag;

	define_tag('size');
		return(self -> 'records_array' -> size);
	/define_tag;

	define_tag('get', -required='index');
		return(knop_databaserow(-record_array=(self -> 'records_array' -> get(#index)), -field_names=(self -> 'field_names')));
	/define_tag;

	define_tag('field', -description='Return an individual field value',
		-required='fieldname',
		-optional='recordindex',
		-optional='index');
		!local_defined('recordindex') ? local('recordindex'=(self -> 'current_record'));
		#recordindex < 1 ? #recordindex = 1;
		!local_defined('index') ? local('index'=1);
		if(self -> 'field_names_map' >> #fieldname 
			&& #recordindex >= 1 
			&& #recordindex <= (self -> 'records_array') -> size);
			// return specific record
			if(#index==1);
				// return first ocurrence of field name through the index map - this is faster
				return(self -> 'records_array' -> get(#recordindex) -> get(self -> 'field_names_map' -> find(#fieldname)));
			else;
				// return another occurrence of the field - this is slightly slower
				local('indexmatches'=(self -> 'field_names') -> findposition(#fieldname));
				if(#index >= 1 && #index <= #indexmatches -> size);
					return(self -> 'records_array' -> get(#recordindex) -> get(#indexmatches -> get(#index)));
				/if;
			/if;
		/if;
	/define_tag;

	define_tag('summary_header', -description='Returns true if the specified field name has changed since the previous record, or if we are at the first record',
		-required='fieldname');
		local('recordindex'=(self -> 'current_record'));
		#recordindex < 1 ? #recordindex = 1;
		if(#recordindex == 1 // first record
			|| self -> field(#fieldname) != self -> field(#fieldname, -recordindex=(#recordindex - 1)) ); // different than previous record (look behind)
			return(true);
		else;
			return(false);
		/if;
	/define_tag;

	define_tag('summary_footer', -description='Returns true if the specified field name will change in the following record, or if we are at the last record',
		-required='fieldname');
		local('recordindex'=(self -> 'current_record'));
		#recordindex < 1 ? #recordindex = 1;
		if(#recordindex == (self -> 'records_array') -> size // last record
			|| self -> field(#fieldname) != self -> field(#fieldname, -recordindex=(#recordindex + 1)) ); // different than next record (look ahead)
			return(true);
		else;
			return(false);
		/if;
	/define_tag;


	define_tag('next', -description='Increments the record pointer, returns true if there are more records to show, false otherwise.');
		if((self -> 'current_record') < (self -> 'records_array') -> size);
			(self -> 'current_record') += 1;
			return(true);
		else;
			// reset record pointer
			(self -> 'current_record') = 0;
			return(false);
		/if;
	/define_tag;
/define_type;



define_type('databaserow',
	-namespace='knop_',
	//-prototype, // prototype prevents the namespace from unloading without restart
	);
	local: 'version'='2009-01-08',
		'description'='Custom type to return individual record rows from knop_database. Used as output for knop_database->get. ';
/*

CHANGE NOTES
2009-01-08	JS	->_unknowntag: Added -index parameter
2008-11-24	JS	->field: Added -index parameter to be able to access any occurrence of the same field name
2008-05-29	JS	Removed -prototype since it prevents unloading the namespace. It is recommended to turn it on for best performance
2008-05-27	JS	Created the type


*/
	local('record_array'=array,
		'field_names'=array);
		
	define_tag('oncreate', -description='Create a record row object. \n\
			Parameters:\n\
			-record_array (array) Array with field values for all fields for the record
			-field_names (array) Array with all the field names, should be same size as -record_array',
		-required='record_array',
		-required='field_names');
		self -> 'record_array'=#record_array;
		self -> 'field_names'=#field_names;
	/define_tag;
	
	define_tag('_unknowntag', -description='Shortcut to field',
		-optional='index');
		!local_defined('index') ? local('index'=1);
		if(self -> 'field_names' >> tag_name);
			return(self -> field(tag_name, -index=#index));
		else;
			//fail: -9948, self -> type + '->' + tag_name + ' not known.';
		/if;
	/define_tag;

	define_tag('onconvert', -description='Output the record as a plain array of field values');
		return(self -> 'record_array');
	/define_tag;


	define_tag('field', -description='Return an individual field value',
		-required='fieldname',
		-optional='index');
		!local_defined('index') ? local('index'=1);
		if(self -> 'field_names' >> #fieldname);
			// return any occurrence of the field
			local('indexmatches'=(self -> 'field_names') -> findposition(#fieldname));
			if(#index >= 1 && #index <= #indexmatches -> size);
				return((self -> 'record_array') -> get(#indexmatches -> get(#index)));
			/if;
		/if;
	/define_tag;


/define_type;
?>
