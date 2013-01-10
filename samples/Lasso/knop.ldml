[/* 

	On-Demand library for namespace knop
	Namespace file built date 2012-06-10 02:05:30 by http://knop8/buildnamespace.lasso
	Montania System AB

*/]

[
//------------------------------------------------------------------
//    Begin knop custom tags in util.inc
//------------------------------------------------------------------

]<?LassoScript

/*

2009-09-04	JS	Added content_body tag for compatibility with pre 8.5
2009-05-07	JS	Added timestamp to knop_cachestore and -maxage parameter to knop_cachefetch
2009-05-07	JS	Corrected construction of cache_name internally in the knop_cache tags so it will work correctly at the site root
2008-11-04	JS	Added dummy knop_debug ctype to be able to work transparently with or without L-Debug
2008-09-24	JS	Added knop_stripbackticks
2008-01-28	JS	Cache tags: -name is now used also when using session storage
2007-12-12	JS	Added knop_cachedelete
2007-12-11	JS	Created knop_cachestore and knop_cachefetch
2007-12-03	JS	Corrected knop_foundrows condition for returning normal found_count

*/

// For compatibility with pre Lasso 8.5 
if(!lasso_tagexists('content_body'));
	define_tag('content_body', -namespace='_global_');
		return(@$__html_reply__);
	/define_tag;
/if;

if(!lasso_tagexists('knop_debug'));
	define_type('debug',
		-namespace='knop_');
		define_tag('_unknowntag');/define_tag;
		define_tag('onconvert');/define_tag;
	/define_type;
/if;

define_tag('stripbackticks', -description='Remove backticks (`) from a string to make it safe for MySQL object names',
	-namespace='knop_',
	-priority='replace',
	-required='input');
	local('output'=string(#input));
	#output = #output -> split('`') -> first;
	return(@#output);
/define_tag;

define_tag: 'unique', -description='Returns a very unique but still rather short random string',
	-namespace='knop_',
	-priority='replace';

	// Johan Sölve 2006-09-20 
	
	local: 'output'=string,
		'seed'=integer,
		'charlist'='abcdefghijklmnopqrstuvwxyz0123456789';
	local: 'base'=(#charlist -> size);
	// start with the current date and time in a mixed up format as seed
	#seed = integer: (date -> (format: '%S%y%m%d%H%M'));
	// convert this integer to a string using base conversion
	while: #seed>0;
		#output = #charlist -> (get: (#seed % #base)+1) + #output;
		#seed = #seed / #base;
	/while;
	// start over with a new chunk as seed
	#seed = string: 1000+(date->millisecond);
	#seed = #seed + string: (math_random: -lower=1000, -upper=9999);
	#seed = integer: #seed;
	// convert this integer to a string using base conversion
	while: #seed>0;
		#output = #charlist -> (get: (#seed % #base)+1) + #output;
		#seed = #seed / #base;
	/while;
	return: #output;
/define_tag;


define_tag: 'seed', 
	-namespace='knop_',
	-priority='replace';
	
	local: 'seed'= (string: $__lassoservice_ip__) + response_localpath;
	#seed -> removetrailing(response_filepath);
	return:	 #seed;
/define_tag;

define_tag: 'foundrows', // http://tagswap.net/found_rows
	-namespace='knop_',
	-priority='replace';
	local: 'sql'= action_statement;
	if: (string_findregexp: #sql, -find= '\\sLIMIT\\s', -ignorecase) -> size == 0;
		// || found_count < maxrecords_value; (this condition is inaccurate)
		// found_count must be accurate
		return: found_count;
	/if;
	if: (string_findregexp: #sql, -find= '\\s(GROUP\\s+BY|HAVING)\\s', -ignorecase) -> size == 0;
		// Default method, usually the fastest. Can not be used with GROUP BY for example. 
		// First normalize whitespace around FROM in the expression
		#sql = (string_replaceregexp: #sql, -find= '\\sFROM\\s', -replace=' FROM ', -ignorecase, -ReplaceOnlyOne);
		#sql = 'SELECT COUNT(*) AS found_rows ' + #sql -> (substring: (#sql -> (find: ' FROM ')) + 1) ;
		#sql = (string_replaceregexp: #sql, -find='\\sLIMIT\\s+[0-9,]+', -replace='');
		if: (string_findregexp: #sql, -find= '\\sORDER\\s+BY\\s', -ignorecase) -> size;
			// remove ORDER BY statement since it causes problems with field aliases
			// first normalize the expression so we can find it with simple string expression later
			#sql = (string_replaceregexp: #sql, -find= '\\sORDER\\s+BY\\s', -replace=' ORDER BY ', -ignorecase);
			#sql = #sql -> (substring: 1, (#sql -> (find: ' ORDER BY ')) -1);
		/if;
	else; // query contains GROUP BY so use SQL_CALC_FOUND_ROWS which can be much slower, see http://bugs.mysql.com/bug.php?id=18454
		#sql -> (removeleading: 'SELECT');
		#sql = 'SELECT SQL_CALC_FOUND_ROWS ' + #sql + ';SELECT FOUND_ROWS() AS found_rows';
		#sql = (string_replaceregexp: #sql, -find='\\sLIMIT\\s+[0-9,]+', -replace=' LIMIT 1', -ignorecase);
	/if;
	inline: -sql=#sql;
		if: (field: 'found_rows') > 0;
			return: integer: (field: 'found_rows'); // exit here normally
		/if;
	/inline;
	// fallback
	return: found_count;
/define_tag;	

define_tag:'IDcrypt', -description='Encrypts or Decrypts integer values',
	-namespace='knop_',
	-required='value',
	-optional='seed',
	-priority='replace';
/*

[IDcrypt]
Encrypts or Decrypts integer values

Author: Pier Kuipers
Last Modified: Jan. 29, 2007
License: Public Domain

Description:
This tag was written to deal with "scraping" attacks where bots keep 
requesting the same page with incremental id parameters, corresponding to 
mysql id columns. Rather than introducing a new column with a unique id, this 
tag will "intelligently" blowfish encrypt or decrypt existing id values.


Sample Usage:
[local('myID' = (action_param('id')))]
[IDcrypt(#myID)]

[IDcrypt('35446')] -> j4b50f315238d68df

[IDcrypt('j4b50f315238d68df')] -> 35446



Downloaded from tagSwap.net on Feb. 07, 2007.
Latest version available from <http://tagSwap.net/IDcrypt>.

*/
// if id values need to be retrieved from bookmarked urls, the tag's built-in seed value must be used,
// or the seed value used must be guaranteed to be the same as when the value was encrypted!		

	local('cryptvalue' = string);
	!local_defined('seed') ? local('seed' = knop_seed);
	Local('RandChars' = 'AaBbCcDdEeFfGgHhiJjKkLmNnoPpQqRrSsTtUuVvWwXxYyZz');
	Local('anyChar' = (#RandChars -> (Get:(Math_Random: -Min=1, -Max=(#RandChars->Size)))));
// taken from Bil Corry's [lp_string_getNumeric]
	local('numericValue' = (string_findregexp((string: #value), -find='\\d')->(join:'')));
	
	if(
		(#numericValue == (integer(#value))) 
		&& 
		(((string(#value))->length) == ((string(#numericValue)) -> length))
	);
// alpha character is inserted at beginning of encrypted string in case value needs to be
// cast to a javascript variable, which cannot start with a number		
		#cryptvalue = (#anyChar + (Encrypt_Blowfish(#value, -seed=#seed)));
	else(
		((((string(#value))->length) - 1) % 2 == 0)
		&&
		(((string(#value))->length) > 16)
	);
		#cryptvalue = (decrypt_blowfish((String_Remove: #value, -StartPosition=1, -EndPosition=1),-Seed=#seed));
	else;
		#cryptvalue = 0;
	/if;
	
	if(String_IsAlphaNumeric(#cryptvalue));
		return(#cryptvalue);
	else;
// successfully decrypted values resulting in lots of strange characters are probably
// the result of someone guessing a value		
		return(0);
	/if;

/define_tag;



define_type: 'timer', -description='Utility type to provide a simple timer',
	-namespace='knop_';
	/*
	
	CHANGE NOTES
	2007-06-17	JS	Created the type
	
	*/

	local: 't'=integer;
	define_tag: 'oncreate';
		(self -> 't') = _date_msec;
	/define_tag;
	define_tag: 'onconvert';
		return: _date_msec - (self -> 't');
	/define_tag;
	
/define_type;

define_tag: 'cachestore', -description='Stores all instances of page variables of the specified type in a cache object. Caches are stored \
		in a global variable named by host name and document root to isolate the storage of different hosts. \n\
		Parameters:\n\
		-type (required string) Page variables of the specified type will be stored in cache. Data types can be specified with or without namespace.\n\
		-expires (optional integer) The number of seconds that the cached data should be valid. Defaults to 600 (10 minutes)\n\
		-session (optional string) The name of an existing session to use for cache storage instead of the global storage\n\
		-name (optional string) Extra name parameter to be able to isolate the cache storage from other sites on the same virtual hosts, or caches for different uses. ',
	-namespace='knop_',
	-required='type', -type='string',
	-optional='expires', -type='integer', // seconds
	-optional='session', -type='string',
	-optional='name', -type='string';

	local: 'data'=map;
	!(local_defined: 'expires') ? local: 'expires'=600; // default seconds
	// store all page vars of the specified type
	iterate: vars -> keys, local: 'item';
		if: (var: #item) -> isa(#type);
			#data -> insert(#item = (var: #item));
		/if;
	/iterate;
	if: (local_defined: 'session');
		//fail_if: (session_id: -name=#session) -> size == 0, -1, 'Cachestore with -session requires that the specified session is started';
		local: 'cache_name' = '_knop_cache_' + (local: 'name');
		session_addvar: -name=#session, #cache_name;
		!((var: #cache_name) -> isa('map')) ? var: #cache_name = map;
		(var: #cache_name) -> insert(#type = (map: 
			'content'=#data, 
			'timestamp'=date, 
			'expires'=(date + (duration: -second=#expires))));
	else;
		local: 'cache_name'='knop_' + (local: 'name') + '_' + server_name + response_localpath;
		#cache_name -> removetrailing(response_filepath);
		// initiate thread RW lock
		!(global: 'rwlock_' + #cache_name) -> isa('rwlock') ? global: 'rwlock_' + #cache_name=Thread_RWLock;
		// create a reference to the lock
		local: 'lock'=@(global: 'rwlock_' + #cache_name);
		// lock for writing
		#lock -> writelock;
		// check and initiate the cache storage
		!((global: #cache_name) -> isa('map')) ? global: #cache_name = map;
		(global: #cache_name) -> insert(#type = (map: 
			'content'=#data, 
			'timestamp'=date, 
			'expires'=(date + (duration: -second=#expires))));
		// unlock
		#lock -> writeunlock;
	/if;
/define_tag;

define_tag: 'cachefetch', -description='Recreates page variables from previously cached instances of the specified type, returns true if successful or false if there was no valid \
		existing cache for the specified type. Caches are stored in a global variable named by host name and document root to isolate the storage of different hosts. \n\
		Parameters:\n\
		-type (required string) Page variables of the specified type will be stored in cache. \n\
		-session (optional string) The name of an existing session to use for cache storage instead of the global storage\n\
		-name (optional string) Extra name parameter to be able to isolate the cache storage from other sites on the same virtual hosts. \n\
		-maxage (optional date) Cache data older than the date/time specified in -maxage will not be used.',
	-namespace='knop_',
	-required='type', -type='string',
	-optional='session', -type='string',
	-optional='name', -type='string',
	-optional='maxage', -type='date';
	
	
	local: 'data'=null;
	if: (local_defined: 'session');
		//fail_if: (session_id: -name=#session) -> size == 0, -1, 'Cachefetch with -session requires that the specified session is started';
		local: 'cache_name' = '_knop_cache_' + (local: 'name');
		if: (var: #cache_name) -> isa('map') 
			&& (var: #cache_name) >> #type 
			&& (var: #cache_name) -> find(#type) -> find('expires') > date;
			if(local_defined('maxage') 
				&& var(#cache_name) -> find(#type) -> find('timestamp') < #maxage);
				// cached data too old
			else;
				#data = (var: #cache_name) -> find(#type) -> find('content');
			/if;
		/if;
	else;
		local: 'cache_name'='knop_' + (local: 'name') + '_' + server_name + response_localpath;
		#cache_name -> removetrailing(response_filepath);
		// initiate thread RW lock
		!(global: 'rwlock_' + #cache_name) -> isa('rwlock') ? global: 'rwlock_' + #cache_name=Thread_RWLock;
		// create a reference to the lock
		local: 'lock'=@(global: 'rwlock_' + #cache_name);
		// lock for reading
		#lock -> readlock;
		if: (global: #cache_name) -> isa('map') 
			&& (global: #cache_name) >> #type 
			&& (global: #cache_name) -> find(#type) -> find('expires') > date;
			if(local_defined('maxage') 
				&& global(#cache_name) -> find(#type) -> find('timestamp') < #maxage);
				// cached data too old
			else;
				#data = (global: #cache_name) -> find(#type) -> find('content');
			/if;
		/if;
		// unlock
		#lock -> readunlock;
	/if;
	if: #data -> isa('map');
		iterate: #data, local: 'item';
			var: (#item -> name) = #item -> value;
		/iterate;
		return: true;
	else;
		return: false;
	/if;
/define_tag;


define_tag: 'cachedelete', -description='Deletes the cache for the specified name (and optionally name). \n\
		Parameters:\n\
		-type (required string) Page variables of the specified type will be stored in cache. \n\
		-session (optional string) The name of an existing session to use for cache storage instead of the global storage\n\
		-name (optional string) Extra name parameter to be able to isolate the cache storage from other sites on the same virtual hosts. ',
	-namespace='knop_',
	-required='type', -type='string',
	-optional='session', -type='string',
	-optional='name', -type='string'; // ignored for session
	if: (local_defined: 'session');
		//fail_if: (session_id: -name=#session) -> size == 0, -1, 'Cachestore with -session requires that the specified session is started';
		local: 'cache_name' = '_knop_cache_' + (local: 'name');
		session_addvar: -name=#session, #cache_name;
		!((var: #cache_name) -> isa('map')) ? var: #cache_name = map;
		(var: #cache_name) -> remove(#type);
	else;
		local: 'cache_name'='knop_' + (local: 'name') + '_' + server_name + response_localpath;
		#cache_name -> removetrailing(response_filepath);
		// initiate thread RW lock
		!(global: 'rwlock_' + #cache_name) -> isa('rwlock') ? global: 'rwlock_' + #cache_name=Thread_RWLock;
		// create a reference to the lock
		local: 'lock'=@(global: 'rwlock_' + #cache_name);
		// lock for writing
		#lock -> writelock;
		// check and initiate the cache storage
		!((global: #cache_name) -> isa('map')) ? global: #cache_name = map;
		(global: #cache_name) -> remove(#type);
		// unlock
		#lock -> writeunlock;
	/if;

/define_tag;


?>
[
//------------------------------------------------------------------
//    End knop custom tags in util.inc
//------------------------------------------------------------------

//##################################################################

][
//------------------------------------------------------------------
//    Begin knop_base
//------------------------------------------------------------------

]<?LassoScript

define_type('knoptype',
	-namespace='knop_');
	local('description'='All Knop custom types should have this type as parent type. This is to be able to identify all registered knop types. ');
	local('isknoptype'=true);
/define_type;

define_type: 'base',
	'knop_knoptype',
	-namespace='knop_';
//	-prototype;

	local: 'version'='2009-09-14',
		'description'='Base data type for Knop framework. Contains common member tags. Used as boilerplate when creating the other types. \
						All member tags and instance variables in this type are available in the other knop types as well. ';
/*


CHANGE NOTES
2009-09-14	JS	Syntax adjustments for Lasso 9
2009-09-04	JS	Changed $__html_reply__ to content_body
2009-04-07	JS	->error_msg: custom error numbers can now be added, even if the language already exists.
2008-01-10	JS	->error_msg: improved reporting of custom error messages such as from bad database queries
2007-12-13	JS	Added -> error_lang to provide a reference to the knop_lang object for error messages, to be able to add localized error messages to any Knop type (except knop_lang and knop_base)
2007-12-12	JS	Added -html and -xhtml to ->help to get a nicely formatted output. 
2007-12-11	JS	Centralized ->error_code and ->error_msg to knop_base. Moved all error codes to error_msg
2007-12-06	JS	Changed ->help to improve the self-documentation. It will now always return an up to date list of member tags and parameter. 
2007-11-05	JS	Added var name to trace output
2007-06-17	JS	Added ->tagtime (was in nav earlier)
2007-06-13	JS	Added -> varname to be able to retreive the name of the page variable that a type instance is stored in.
2007-06-13	JS	Added -> xhtml to automatically sense if an xhtml doctype exists in the current page buffer. The result is cached in a page variable for performance. 
				This is for internal use for member tags that output html. 
2007-06-13	JS	Introduced page variable $_knop_data for general page level storage and caching, common between different knop objects. 
2007-06-13	JS	Created the data type

TODO: ->help: add output option to format for Google Code Wiki
->xhtml is not working properly when site is run by atbegin handler and explicitly writing to content_body 


*/

	local: 'debug_trace'=array,
		'_debug_trace'=array,
		'instance_unique'=null,
		'instance_varname'=null,
		'tagtime'=integer,				// time for entire tag in ms
		'tagtime_tagname'=string,
		'error_code'=0,
		'error_msg'=string,
		'error_lang'=null,	// must be defined as knop_lang in each type instead, to avoid recursion
		;

	define_tag: 'ondeserialize', -description='Recreates transient variables after coming back from a session';
		self -> properties -> first -> insert('_debug_trace'=array);
	/define_tag;

	define_tag: 'help', -description='Auto generates an overview of all member tags of a type, with all parameters specified for each member tag.', 
		-optional='html',
		-optional='xhtml';
		local: 'endslash' = ((self -> (xhtml: params)) ? ' /' | '');
		local: 'eol'=(local_defined: 'html') || #endslash -> size ? '<br' + #endslash + '>\n' | '\n';

		local: 'output'=string,
			'tags'=array,
			'description'=string,
			'parameters'=string;
		#output += (self -> type) + ' - version ' + (self -> 'version') + '\n' ;
		#output += (self -> 'description') + '\n\n';
		iterate: (self -> properties -> second) , local: 't';
			#tags -> (insert: #t);
		/iterate;
		if: (self -> parent -> type != 'null'); // this doesn't work
			iterate: (self -> parent -> properties -> second) , local: 't';
				#tags -> (insert: #t);
			/iterate;
		/if;
		#tags -> sort;
		iterate: #tags , local: 't';
			#parameters = string;
			#output += '-> ' + (#t -> name);
			#description=(#t -> value -> description);
			iterate: (#t -> value -> paraminfo) , local: 'p';
				if: #description !>> '-' + (#p -> paramname);
					#parameters += '-' + (#p -> paramname) + ' (' (#p -> isrequired ? 'required' | 'optional') 
						+ (#p -> paramtype != 'null' && #p -> paramtype -> size ? ' ' + (#p -> paramtype))  + ')\n';
				/if;
			/iterate;
			#output += (#description -> size || #parameters -> size ? '\n' + #description);
			#output += (#description >> 'Parameters:' ?  '\n');
			#output += (#description !>> 'Parameters:' && #parameters -> size ? '\nParameters:\n');
			#output += (#parameters -> size ? #parameters);
			#output -> removetrailing('\n');
			#output += '\n\n';
		/iterate;
		if: ((local_defined: 'html') && #html != false) || ((local_defined: 'xhtml') && #xhtml != false);
			#output = encode_html: #output;
			// normalize line breaks and convert to <br>
			#output -> (replace: '\r\n', '\n') & (replace: '\r', '\n') & (replace: '\n', #eol + '\n');
		/if;
		return: #output;
	/define_tag;


	define_tag: 'xhtml', -description='Internal. Finds out if xhtml output should be used. Looks at doctype unless -xhtml is specified \
			in the params array. The result is cached in a page variable. \n\
			Looking at doctype doesn\'t work when using atbegin driven solutions since content_body isn\'t filled with the page buffer until the page has already been processed.  ',
		-optional='params';
		if: (local_defined: 'params') && #params >> '-xhtml';
			local: 'xhtmlparam'=#params -> (find: '-xhtml') -> first;
			if: #xhtmlparam -> type == 'pair'; // -xhtml=true / -xhtml=false
				return: boolean: (#xhtmlparam -> value);
			else; // plain -xhtml
				return: true;
			/if;
		/if;
		if: (var: '_knop_data') -> type != 'map';
			$_knop_data = map;
		/if;
		if: $_knop_data !>> 'doctype_xhtml';
			local: 'doctype' = content_body -> (substring: 1, (content_body -> (find: '>')));
			$_knop_data -> (insert: 'doctype_xhtml' = (#doctype >> '<!DOCTYPE' && #doctype >> 'xhtml'));
		/if;
		return: $_knop_data -> (find: 'doctype_xhtml');
	/define_tag;


	define_tag: 'error_lang', -description='Returns a reference to the language object used for error codes, to be able to add localized error messages to any Knop type (except knop_lang and knop_base)';
		return: @(self -> 'error_lang');
	/define_tag;

	define_tag: 'error_code', -description='Either proprietary error code or standard Lasso error code';
				return: integer: (self -> 'error_code');
	/define_tag;

	define_tag: 'error_msg',
		-optional='error_code', -type='integer', -copy;
		!(local_defined: 'error_code') ? local: 'error_code'=(self -> error_code);
		local: 'error_lang_custom'=(self -> 'error_lang');
		local: 'error_lang'=(knop_lang: -default='en', -fallback);

		local: 'errorcodes'=(map:
			0 = 'No error',
			-1728 = 'No records found', // standard Lasso error code
			
			// database errors 7000
			7001 ='The specified table was not found',
			7002 = 'Keyfield not specified',
			7003 = 'Lockfield not specified',
			7004 = 'User not specified for record lock',
			7005 = 'Either keyvalue or lockvalue must be specified for update or delete',
			7006 = 'Keyfield or keyvalue missing',
			7007 = 'Keyvalue missing',
			7008 = 'Keyvalue not unique',
			7009 = '-sql can not be used with FileMaker',
			7010 = 'Record locked by another user', // see error_data
			7011 = 'Record lock not valid any more',
			7012 = 'Could not set record lock', // see error_data
			7013 = 'Failed to clear record locks', // see error_data
			7016 = 'Add error', // see error_data
			7017 = 'Add failed, duplicate key value',
			7018 = 'Update error', // see error_data
			7019 = 'Delete error', // see error_data
			7020 = 'Keyfield not present in query',
			7021 = 'Lockfield not present in query',
			
			// form errors 7100
			7101 ='Form validation failed',
			7102 = 'Unsupported field type',
			7103 = 'Form->process requires that a database object is defined for the form',
			7104 = 'Copyfield must copy to a different field name',

			// grid errors 7200 
			
			// lang errors 7300
			
			// nav errors 7400
			
			// user errors 7500
			7501 = 'Authentication failed',
			7502 = 'Username or password missing',
			7503 = 'Client fingerprint has changed'
			
			);
		#error_lang -> (addlanguage: -language='en', -strings=@#errorcodes);
		// add any custom error strings
		iterate(#error_lang_custom -> 'strings', local('custom_language'));
			if(#error_lang -> 'strings' !>> #custom_language -> name);
				// add entire language at once
				#error_lang -> addlanguage(-language=#custom_language -> name, -strings=#custom_language -> value);
			else;
				// add one string at a time
				iterate(#custom_language -> value, local('custom_string'));
					#error_lang -> insert(-language=#custom_language -> name, 
						-key=#custom_string -> name, 
						-value=#custom_string -> value);
				/iterate;
			/if;
		/iterate;
		
		if: #errorcodes >> #error_code;
			// return error message defined by this tag
			if: #error_lang -> keys >> #error_code;
				return: #error_lang -> (getstring: #error_code);
			else;
				return: #errorcodes -> (find: #error_code);
			/if;
		else;
			if: (self -> 'error_msg') != '';
				// return literal error message
				return: (self -> 'error_msg');
			else;
				// test for error known by lasso
				error_code = #error_code;
				// return Lasso error message
				return: error_msg;
			/if;
		/if;
	/define_tag;

	define_tag: 'varname', -description='Returns the name of the variable that this type instance is stored in.';
		local: 'timer'=knop_timer;
		if: self -> 'instance_unique' == null;
			self -> 'instance_unique' = knop_unique;
		/if;
		if: self -> 'instance_varname' == null;
			// look for the var name and store it in instance variable
			iterate: (vars -> keys), (local: 'varname');
				if: (var: #varname) -> type == self -> type 
					&& ((var: #varname) -> 'instance_unique') == (self -> 'instance_unique');
					(self -> 'instance_varname')=#varname;
					loop_abort;
				/if;
			/iterate;
		/if;

		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer;
		return: self -> 'instance_varname';
	/define_tag;

	define_tag: 'trace', -description='Returns the debug trace for a type instance',
		-optional='html',
		-optional='xhtml';

		local: 'endslash' = ((self -> (xhtml: params)) ? ' /' | '');
		local: 'eol'=(local_defined: 'html') || #endslash -> size ? '<br' + #endslash + '>\n' | '\n';
		local: 'trace'=(self -> 'debug_trace');
		(self -> '_debug_trace') -> isa('array') ? #trace -> merge(self -> '_debug_trace');
		return: #eol + 'Debug trace for ' + (self -> type ) + ' $' + (self -> varname) + #eol 
			+ #trace -> (join: #eol) + #eol;

	/define_tag;


	define_tag: 'tagtime', -description='Returns the time it took to execute the last executed member tag for a type instance.',
		-optional='html',
		-optional='xhtml';
		/* Standard timer code
		At beginning of tag code:
		local: 'timer'=knop_timer; 
		
		Before the end of tag code (before return):
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		
		*/
		local: 'endslash' = ((self -> (xhtml: params)) ? ' /' | '');

		((local_defined: 'html') || (local_defined: 'xhtml')) ? return: (self -> type) + '->' + (self -> 'tagtime_tagname') + ': ' + (self -> 'tagtime') + ' ms<br' + #endslash + '>';
		return: (self -> 'tagtime');
	/define_tag;

/define_type;



?>
[
//------------------------------------------------------------------
//    End knop_base
//------------------------------------------------------------------

//##################################################################

][
//------------------------------------------------------------------
//    Begin knop_database
//------------------------------------------------------------------

]<?LassoScript

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
[
//------------------------------------------------------------------
//    End knop_database
//------------------------------------------------------------------

//##################################################################

][
//------------------------------------------------------------------
//    Begin knop_form
//------------------------------------------------------------------

]<?LassoScript

define_type: 'form', 
	'knop_base',
	-namespace='knop_';
//	-prototype; 

	local: 'version'='2011-02-28',
		'description'='Custom type to handle forms.';

/*

CHANGE NOTES
2011-02-28	JS	->addfield: Added -template to specify field specific template 
2010-11-22	SP	->init: Correction of -lockvalue handling after L9 syntax adjustment
2010-07-18	SP	Added support for series for -options
2010-06-10	JS	->renderform: avoid adding -upload parameters to post forms since it conflicts with file uploads (found by Steve Piercy)
2010-04-21	JS	->renderhtml:  removed encode_html for label
2010-03-06	SP	Changed default behavior of ->updatefields with -sql to add backticks between the table and column names.  Now JOINs may be used.
2010-03-06	SP	Added ->updatefields with -removedotbackticks for backward compatibility for fields that contain periods.  If you use periods in a fieldname then you cannot use a JOIN in Knop.
2009-11-11	JS	Added class and id to optiongroup div that surrounds for checkbox and radio
2009-11-11	JS	Corrected id for checkbox and radio option labels
2009-10-02	JS	Added id for labels, auto generated from the field's id with _label appended
2009-09-16	JS	Syntax adjustments for Lasso 9
2009-09-04	JS	Changed $__html_reply__ to content_body
2009-09-04	JS	->renderhtml: corrected typ for autoparams
2009-07-23	JS	->renderform: removed encode_html that somehow has reappeared for label. 
2009-07-10	SP	added -maxlength option for text fields
2009-06-26	JS	->oncreate: added deprecation warning for -action
2009-06-22	JS	->addfield: corrected -options check to look for set instead of series (besides array)
2009-04-16	JS	->loadfileds can now load field values from -params also inside an inline
2009-03-20	JS	Added  <![CDATA[ ... ]]>  around injected scripts for better xhtml compliance
2009-01-08	JS	->getvalue and _unknowntag: added -index parameter to be able to get value for a specific field instance when there are multiple fields with the same name 
2009-01-08	JS	->loadfields: implemented support for multiple fields with the same name when loading field values from form submission where the number of same name fields matches
2009-01-07	JS	->setvalue: added -index parameter to be able to set value for a specific field instance when there are multiple fields with the same name 
2008-12-08	JS	->renderform: Removed the onclick handlers for checkbox and radio since Safari now supports clicking the label text as click for the checkbox/radio control. 
2008-12-05	JS	->renderform: the fieldset and legend field types will now use id and class on the fieldset tag if specified
2008-12-03	JS	->renderform: fields of type fieldset now uses value as legend (just as field type legend already did) instead of always using an empty legend
2008-09-24	JS	->updatefields: Added protection against backtick sql injection in MySQL object names
2008-09-17	JS	->renderform and ->renderhtml: -from and -to allows negative numbers to count from end of form instead
2008-09-13	JS	Added ->getlabel to return the display name for a field. 
2008-09-13	JS	->addfield and ->validate: Implemented -validate to specify a compound expression to validate the field input. 
2008-09-13	JS	->addfield and ->loadfields: Implemented -filter to specify a compound expression to filter the field input. 
2008-09-11	JS	->updatefields: fixed exclusion of special field types html, legend and fieldset. 
2008-09-11	JS	->renderform: Fixed missing value for password fields
2008-07-02	JS	->renderform: Cleaned up the automatic adding of javascript code so it's not added if not needed. Also moved all scripts to the end of the page. More work with with the javascripts is needed.
2008-06-03	JS	->renderform: corrected missing closing </fieldset>
2008-05-15	JS	->renderform and ->renderhtml: adjusted the behavior for nested fieldsets
2008-05-13	JS	Implemented -legend for ->renderhtml, to make it consistent with the new legend field type
2008-05-13	JS	Implemented special field types html, fieldset and legend. Use -value to display data for these fields. A legend field also creates a fieldset (closes any previously open fieldsets). Use fieldset with -value=false to close a fieldset without opening a new one. 
2008-05-06	JS	Added unknowntag as shortcut to getvalue
2008-01-30	JS	Removed duplicate endscript entries for if(dirty) {makedirty()};
2007-12-13	JS	Corrected ->addfield: -dbfield so empty dbfields are properly ignored by ->updatefields. 
2007-12-11	JS	Moved error_msg to knop_base (special version of error_code stays here) 
2007-12-11	JS	Added documentation as -description to most member tags, to be used by the new ->help tag
2007-12-11	JS	Moved ->help to knop_base
2007-11-13	JS	Added -buttontemplate to be able to specify separate template for buttons, defaults to no <br>, but if template has been specified that will be used instead (for backwards compatibility)
2007-11-12	JS	->process delete now works also when not using record locking (not specifying -user)
2007-11-01	JS	->renderform: added support for -hint for textarea fields.
2007-09-27	JS	->renderhtml: multiple values (array) for radio, checkbox and select are now rendered properly with either "," or <br> depending on the presence of -linebreak, and with the display text instead of the actual option value
2007-09-27	JS	->renderform: improved handling of multiple values for checkbox, radio and select
2007-09-21	JS	->addfield: flag parameters now accept false as value
2007-09-06	JS	->oncreate: changed name of -action to -formaction to make it more clear what it is. -action is still supported but deprecated.
2007-09-06	JS	->renderform: Corrected the exception for -session... (duh)
2007-08-08	JS	->renderform: Added exception for -session
2007-06-18	JS	Added tag timer to most member tags
2007-06-13	JS	added inheritance from knop_base
2007-06-12	JC	bugfixed -xhtml form rendering when called by quicksearch
2007-06-11	JC	added handling of xhtml output
2007-04-19	JS	->loadfields: fixed -params that was broken when adding -database
2007-04-19	JS	->renderform: removed invalid wrap="soft" from textarea
2007-04-12	JS	->process: made -user optional (only needed when using record locking)
2007-04-12	JS ->loadfields can now take a -database parameter, either as a flag (no value) where the database object connected to the form will be used, or by specifying a database object as value. 
2007-04-03	JS	Changed namespace from mt_ to knop_
2007-03-01	JS	->renderform fixed unsavedwarning on page load by moving checkdirty() to afterscript
2007-03-01	JS	->formmode and ->init changed so it preserves the right mode after a failed add
2007-02-27	JS	->renderform: added <div class="inputgroup"> around checkboxes and radios for css formating
2007-02-26	JS	->oncreate: added -actionpath to specify the framework action path for the form instead of manually adding the -action hidden field
2007-02-24	JS	Corrected entersubmitblock behavior by adding onfocus handler on form and starting with submitBlock=false
2007-02-23	JS	Removed encode_html from form field labels
2007-02-22	JS	->setformat: Added -legend
2007-02-07	JS	Added ->copyfield to copy a form field to a new name, with the same properties. 
2007-02-07	JS	->errors now returns empty array if validate has not been called, instead of performing validation
2007-02-05	JS	->getbutton can now look for also button names that are not one of the built-in ones (for example button_apply)
2007-02-05	JS The -keyvalue parameter can be given another name by specifying -keyparamname in oncreate
2007-02-02	JS	Added ->lockvalue_decrypted
2007-02-02	JS	->addfield: -value is now stored as reference
2007-02-02	JS	error_code now returns an error for when the form contains validation errors
2007-02-02	JS	Improved reporting of Lasso error messaged in error_msg
2007-02-02	JS 	Added real error codes
2007-01-31	JS	->rederform action_params now also exclude "-" params that appear in the form action
2007-01-29	JS	->renderform: The first field with input error will get focus when loading page
2007-01-29	JS	Added -focus to ->addfield to give default field focus when loading page with form
2007-01-29	JS	Added -disabled to ->addfield, and handling of it in ->renderform
2007-01-29	JS	Added -noautoparams to ->oncreate to disable the automatic passing of action_params that begin with "-"
2007-01-29	JS	->renderform now renders label also for submit, reset to format properly with css
2007-01-26	JS	Added support for Safari specific <input type="search">
2007-01-26	JS	->renderform action_params that begin with "-" now exclude params that exist in the form. Minor corrections to the behavior. 
2007-01-25	JS	Added -nowarning to ->oncreate to disable unsaved warnings for the entire form
2007-01-25	JS	Added -required to ->oncreate (and a few more from ->setformat)
2007-01-23	JS	Autogenerates id for the form itself
2007-01-23	JS	Added ->getbutton to return the button that was clicked when submitting a form (cancel, add, save, delete)
2007-01-23	JS	Added auto conversion of options left hand pair member to string, to make comparsions work reliably. Integer zeros don't compare nicely to strings. 
2007-01-23	JS	Added support for submit-on-enter prevention: specify -entersubmitblock at oncreate
2007-01-19	JS	Addes renderform: -legend to be able to group form fields at render time
2007-01-19	JS	added support for -optgroup in -options for select. Also works for radio and checkbox. Specify empty -optgroup to close optgroup in select without starting a new, or to add extra linebreak between checkboxes/radio buttons. 
2007-01-19	JS	added -template for oncreate
2007-01-19	JS	added optional fieldset and legend to form, legend can be specified as -legend at oncreate. if -legend is specified, the form will be wrapped in a fieldset. 
2007-01-19	JS	method now defaults to post
2007-01-19	JS	Corrected line separator for FileMaker checkboxes and added the same handling also for radio
2007-01-18	JS	renderform: any action_params that begin with "-" (except -keyvalue and -lockvalue) are added as form parameters
2007-01-18	JS	renderform: checkboxes and multiselects now show checked and selected properly when loading values from database
2007-01-18	JS	updatefields: added support for multiple values for one fieldname, like checkboxes (multiple fields in the update pair array, -sql generates comma separated values)
2007-01-17	JS	reset button now makes form undirty
2007-01-17	JS	addfield: -confirmmessage can now be specified for any submit or reset button
2007-01-17	JS	added addfield: -nowarning to avoid unsaved warning when the field is changed
2007-01-17	JS	changed default class name for unsaved marker from dirty to unsaved
2007-01-17	JS	changed name of -dirtymarker and -dirtymarkerclass to unsavedmarker and -unsavedmarkerclass for userfriendlyness
2007-01-17	JS	added setformat: -unsavedwarning to dynamically set the javascript form dirty warning message
2007-01-17	JS	renderform: -field changed to renderform: -name for consistency
2007-01-16	JS	renderform: -field with wrong field name does not output anything, instead of the entire form
2007-01-16	JS	fixed onbeforeunload in javascript form dirty handler

TODO:
->addfield: Add -format to manipulate the field value before it is displayed by ->renderform and ->renderhtml, much like -filter but only for display and without affecting input. 
->addfield: Add -fieldgroup to be able to group related fields together, useful for ->updatefields to return just fields that belong to a specific db table, or ->renderform as another way to render a form selectively
->renderform needs a better way to display errors inline together with the fields
Make _unknowntag also work as shortcut to setvalue if a value is specified
Add a new special field type to the form object, let's say "data". That field type will not interact with forms and will never be touched by loadfields, but it will populate ->updatefields.
Add -> searchfields, which will return a fulltext enabled pair array better suited for searchs than ->updatefields is. -fulltext needs to be specified per field. 
Review and clean up the javascripts inserted automatically by knop_form - partially done
Option to let textarea grow automatically depending on the amount of text in it.  
Use http://bassistance.de/jquery-plugins/jquery-plugin-validation/ instead of client side validation
Possibly add support for the same validation expressions as the jquery validation plugin uses, so server side a nd client side validation can be specified at once. 
Add -path as parameter for oncreate so the form action can be set with less confusion...  In that case -formaction will be a physical url, while -path would be a framework path. 
Fix actionpath reference so it updates properly when altering the value (not possible?)
Should loadfields load "-" params?
Unsavedwarning made optional, does not seem to work properly now?
More flexible error hightlighting
Move templates to a member tag to be make it easier to subclass (Douglas Burchard)
Add "button". <button></button>. Subtypes are submit, reset and button. How to specify the subtype? (Douglas Burchard)
Change ->addfield to ->insert and make ->addfield deprecated
There is no src for input type image!
Add ->size and ->get so the form object can be iterated
Add -skipemtpy to to ->renderhtml
Option for -> renderhtml to output without html encoding
->renderhtml should never html encode fields of type html

*/
	

	// instance variables
	local: 'fields'=array,
		'template'=string,			// html template used to render the html form fields
		'buttontemplate'=string,	// html template used to render the html buttons (submit, reset, image)
		'class'=string,				// default class for all form fields, can be overridden field by field
		'errorclass'=string,		// class used to highlight field labels when validation fails
		'formaction'=null,
		'method'='post',
		'fieldset'=false,			// html form fieldset
		'legend'=null,				// html form legend
		'name'=null,
		'id'=null,
		'raw'=null,
		'enctype'=null,				// is automatically set to multipart/formdata if the form contains a file input
		'actionpath'=null,
		'noautoparams'=false,		// if true then no parameters that begin with - will be automatically added to the form
		'fieldsource'=null,			// the source of the latest -> loadfields, can be database, form or params
		'required'=string,			// marker used to show fields that are required (html or plain string)
		'entersubmitblock'=false,	// if true, a javascript will prevent form submit without clicking on submit button (like pressing enter key)
		'unsavedmarker'=null,
		'unsavedmarkerclass'=null,
		'unsavedwarning'=string,	// must be specified, or else there is no unsaved warning for the form
		'database'=null,
		'keyparamname'=string,		// param name to use instead of the default -keyvalue
		'formmode'=null,			// whether the form is for editing an existing record or a blank for for adding a new record (edit/add)
									// only valid if a database object is specified
		'formbutton'=null,			// the button that was clicked when submitting a form (cancel, add, save, delete)
		'db_keyvalue'=null,
		'db_lockvalue'=null,
		
		'render_fieldset_open'=false,	// used when rendering to keep track of if a fieldset from fieldset or legend field types is open so it can be closed properly
		'render_fieldset2_open'=false,	// used when rendering to keep track of if a fieldset from renderform or renderhtml legend is open so it can be closed properly
		'noscript'=false,				// when set to true, no scripts will be injected by renderform
		'error_lang'=(knop_lang: -default='en', -fallback);
	
	local: 'errors'=null;


	// config vars
	local: 'validfieldtypes' = (map: 'text', 'password', 'checkbox', 'radio', 'textarea', 'select', 'file', 'search',
				'submit', 'reset', 'image', 'hidden', 
				'fieldset', 'legend', 'html'), // special types
		'exceptionfieldtypes' = (map: 'file', 'submit', 'reset', 'image', 'addbutton', 'savebutton', 'deletebutton', 'cancelbutton',
				'fieldset', 'legend', 'html'); // special types
	local: 'validfieldtypes_array'=array;
	iterate: #validfieldtypes, (local: 'temp');
		#validfieldtypes_array -> (insert: #temp -> name);
	/iterate;
	local: 'exceptionfieldtypes_array'=array;
	iterate: #exceptionfieldtypes, (local: 'temp');
		#exceptionfieldtypes_array -> (insert: #temp -> name);
	/iterate;
	
	// page var to keep track of the number of forms that have been rendered on a page
	if: !(var_defined: 'knop_form_renderform_counter');
		var: 'knop_form_renderform_counter'=0;
	/if;
	

	define_tag: 'oncreate', -description='Parameters:\n\
			-formaction (optional) The action atribute in the form html tag\n\
			-action (optional) Deprecated synonym to -formaction\n\
			-method (optional) Defaults to post\n\
			-name (optional)\n\
			-id (optional)\n\
			-raw (optional) Anything in this parameter will be put in the opening form tag\n\
			-actionpath (optional) Knop action path\n\
			-fieldset (optional)\n\
			-legend (optional string) legend for the entire form - if specified, a fieldset will also be wrapped around the form\n\
			-entersubmitblock (optional)\n\
			-noautoparams (optional)\n\
			-template (optional string) html template, defaults to #label# #field##required#<br>\n\
			-buttontemplate (optional string) html template for buttons, defaults to #field# but uses -template if specified\n\
			-required (optional string) character(s) to display for required fields (used for #required#), defaults to *\n\
			-class (optional string) css class name that will be used for the form element, default none\n\
			-errorclass (optional string) css class name that will be used for the label to highlight input errors, if not defined style="color: red" will be used\n\
			-unsavedmarker (optional string) id for html element that should be used to indicate when the form becomes dirty. \n\
			-unsavedmarkerclass (optional string) class name to use for the html element. Defaults to "unsaved". \n\
			-unsavedwarning (optional string)\n\
			-keyparamname (optional)\n\
			-noscript (optional flag) if specified, don\'t inject any javascript in the form. This will disable all client side functionality such as hints, focus and unsaved warnings. \n\
			-database (optional database) Optional database object that the form object will interact with',
		// parameters for form html tag attributes
		-optional='formaction',
		-optional='action',
		-optional='method',
		-optional='name',
		-optional='id',
		-optional='raw',

		// knop parameters
		-optional='actionpath',
		-optional='fieldset',
		-optional='legend', 
		-optional='entersubmitblock',
		-optional='noautoparams',
		-optional='template', -type='string',
		-optional='buttontemplate', -type='string',
		-optional='required', -type='string',
		-optional='class', -type='string',
		-optional='errorclass', -type='string',
		-optional='unsavedmarker', -type='string',
		-optional='unsavedmarkerclass', -type='string',
		-optional='unsavedwarning', -type='string',
		-optional='keyparamname',
		-optional='noscript',
		-optional='database', -type='database';
		local: 'timer'=knop_timer; 


		local_defined('method') ? (self -> 'method') = #method;
		local_defined('name') ? (self -> 'name') = #name;
		local_defined('id') ? (self -> 'id') = #id;
		local_defined('raw') ? (self -> 'raw') = #raw;
		local_defined('legend') ? (self -> 'legend') = #legend;
		local_defined('template') ? (self -> 'template') = #template;
		local_defined('buttontemplate') ? (self -> 'buttontemplate') = #buttontemplate;
		local_defined('required') ? (self -> 'required') = #required;
		local_defined('class') ? (self -> 'class') = #class;
		local_defined('errorclass') ? (self -> 'errorclass') = #errorclass;
		local_defined('unsavedmarker') ? (self -> 'unsavedmarker') = #unsavedmarker;
		local_defined('unsavedmarkerclass') ? (self -> 'unsavedmarkerclass') = #unsavedmarkerclass;
		local_defined('unsavedwarning') ? (self -> 'unsavedwarning') = #unsavedwarning;
		local_defined('keyparamname') ? (self -> 'keyparamname') = #keyparamname;

		// the following params are stored as reference, so the values of the params can be altered after adding a field simply by changing the referenced variable. 
		local_defined('formaction') ? (self -> 'formaction') = @#formaction;
		local_defined('actionpath') ? (self -> 'actionpath') = @#actionpath;
		local_defined('database') ? (self -> 'database') = @#database;

		if: !(local_defined: 'formaction') && (local_defined: 'action');
			// keep support for old -action insead of -formaction
			(self -> 'debug_trace') -> insert('*** DEPRECATION WARNING *** ' + tag_name + ' -action parameter is deprecated, use -formaction instead ');
			(self -> 'formaction') = @#action;
		/if;
		
		(self -> 'noscript') = (local_defined('noscript') && #noscript != false);

		// default value
		!(local_defined: 'required') ? (self -> 'required' = '*');
		!(local_defined: 'keyparamname') ? (self -> 'keyparamname' = '-keyvalue');

		(self -> 'fieldset') = ((local_defined: 'fieldset') && #fieldset != false) || (self -> 'legend') != '';
		(self -> 'entersubmitblock') = (local_defined: 'entersubmitblock');
		(self -> 'noautoparams') = (local_defined: 'noautoparams');


		if: (self -> 'unsavedmarker') != '' && (self -> 'unsavedmarkerclass') == '';
			// set default unsavedmarkerclass
			(self -> 'unsavedmarkerclass')='unsaved';
		/if;

		if: (self -> 'unsavedwarning') == '';
			// set default dirtywarning message
			//(self -> 'unsavedwarning')='Det finns ändringar som inte har sparats - vill du fortsätta utan att spara?';
		/if;
		
		// escape quotes for javascript
		(self -> 'unsavedwarning') -> (replace: '\'', '\\\'');
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	/*
	define_tag: 'onassign', -description='Internal, needed to restore references when ctype is defined as prototype',
		-required='value'; 
		// recreate references here

		iterate: (array: 
			'formaction',
			'actionpath',
			'database'), (local: 'param');
			(self -> #param) = @(#value -> #param);
		/iterate;

	/define_tag;
	*/
	
	define_tag: 'onconvert', -description='Outputs the form data in very basic form, just to see what it contains',
		-optional='xhtml';
		local: 'timer'=knop_timer; 

		local: 'endslash' = ((self -> (xhtml: params)) ? ' /' | '');

		local: 'output'=string;
		iterate: (self -> 'fields'), (local: 'fieldpair');
			#output += #fieldpair -> name + ' = ' + #fieldpair -> value + '    <br' + #endslash + '>\n';
		/iterate;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: #output;
	/define_tag;
	
	define_tag: '_unknowntag', -description='Shortcut to getvalue',
		-optional='index', -type='integer', -copy;
		!local_defined('index') ? local('index') = 1;
		if: (self -> 'fields') >> tag_name; // should be (self -> keys) but this is faster
			return: (self -> (getvalue: tag_name, -index=#index));
		else;
			//fail: -9948, self -> type + '->' + tag_name + ' not known.';
			(self -> '_debug_trace') -> insert(self -> type + '->' + tag_name + ' not known.');
		/if;
	/define_tag;
	
	define_tag: 'addfield', -description='Inserts a form element in the form. \n\
			Parameters:\n\
			-type (required) Supported types are listed in form -> \'validfieldtypes_array\'. Also custom field types addbuton, savebutton or deletebutton are supported (translated to submit buttons with predefined names). \
			For the field types html, fieldset and legend use -value to specify the data to display for these fields. A legend field automatically creates a fieldset (closes any previously open fieldsets). Use fieldset with -value=false to close a fieldset without opening a new one. \n\
			-name (optional) Required for all input types except addbuton, savebutton, deletebutton, fieldset, legend and html\n\
			-id (optional) id for the html object, will be autogenerated if not specified\n\
			-dbfield (optional) Corresponding database field name (name is used if dbfield is not specified), or null/emtpy string if ignore this field for database\n\
			-value (optional) Initial value for the field\n\
			-hint (optional) Optional gray hint text to show in empty text field\n\
            -options (optional) For select, checkbox and radio, must be array, set or series. For select, the array can contain -optgroup=label to create an optiongroup. \n\ 
			-multiple (optional flag) Used for select\n\
			-linebreak (optional flag) Put linebreaks between checkbox and radio values\n\
			-default (optional) Default text to display in a popup menu, will be selected (with empty value) if no current value is set. Is followed by an empty option. \n\
			-label (optional) Text label for the field\n\
			-size (optional) Used for text and select\n\
			-maxlength (optional) Used for text\n\
			-rows (optional) Used for textarea\n\
			-cols (optional) Used for textarea\n\
			-focus (optional flag) The first text field with this parameter specified will get focus when page loads\n\
			-class (optional)\n\
			-disabled (optional flag) The form field will be rendered as disabled\n\
			-raw (optional) Raw attributes that will be put in the html tag\n\
			-confirmmessage (optional) Message to show in submit/reset confirm dialog (delete button always shows confirm dialog)\n\
			-required (optional flag) If specified then the field must not be empty (very basic validation)\n\
			-validate (optional) Compound expression to validate the field input. The input can be accessed as params inside the expression which should either return true for valid input or false for invalid, or return 0 for valid input or a non-zero error code or error message string for invalid input. \n\
			-filter (optional) Compound expression to filter the input before it is loaded into the form by ->loadfields. The field value can be accessed as params inside the expression which should return the filtered field value. -filter is applied before validation. \n\
			-nowarning (optional flag) If specified then changing the field will not trigger an unsaved warning\n\
			-after (optional) Numeric index or name of field to insert after\n\
			-template (optional) Format string that will override global template or buttontemplate',
		-required='type',
		-optional='name',
		-optional='id',	
		-optional='dbfield',
		-optional='value',
		-optional='hint',
		-optional='options',
		-optional='multiple',
		-optional='linebreak',
		-optional='default',
		-optional='label',
		-optional='size',
		-optional='maxlength',
		-optional='rows',
		-optional='cols',
		-optional='focus',
		-optional='class',
		-optional='disabled',
		-optional='raw',
		-optional='confirmmessage',
		-optional='required',
		-optional='validate', -type='tag',
		-optional='filter', -type='tag',
		-optional='nowarning',
		-optional='after',
		-optional='template';
		// TODO: add optiontemplate to be able to format individual options
		local: 'timer'=knop_timer; 
		
		local: '_type'=(local: 'type'), '_name'=(local: 'name'), 'originaltype'=(local: 'type');
		if: (map: 'addbutton', 'savebutton', 'deletebutton', 'cancelbutton') >> #_type;
			#originaltype = #_type;
			#_name = 'button_' + #_type;
			#_name -> (removetrailing: 'button');
			#_type = 'submit';
		else: #_type == 'reset' && (local: 'name') == '';
			#_name = 'button_' + #_type;
		else: (map: 'legend', 'fieldset', 'html') >> #_type && (local: 'name') == '';
			#_name = #_type;
		else;
			fail_if: (local: 'name') == '', -9956, 'form->addfield missing required parameter -name';
		/if;
		
		
		fail_if: !((self -> 'validfieldtypes') >> #_type), 7102, self -> error_msg(7202);
		fail_if: (map: 'select', 'radio', 'checkbox') >> #_type 
			&& (local: 'options') -> type != 'array' 
			&& (local: 'options') -> type != 'set' 
			&& (local: 'options') -> type != 'series', 
			-9956, 'Field type ' #_type ' requires -options array, set or series'; 
		local: 'index'= (self -> 'fields') -> size + 1;
		(local_defined: 'after') 	? (#after -> type == 'string' && (self -> 'fields') >> #after 
										? #index = (integer: ((self -> 'fields') -> (findindex: #after) -> first)) + 1
										| #after -> type == 'integer' ? #index= #after + 1);
		if: #_type == 'file';
			(self -> 'enctype') ='multipart/form-data';
			(self -> 'method') = 'post';
		/if;
		local: 'field'=(map: 
			'required'=(local_defined: 'required') 		&& #required != false,
			'multiple'=(local_defined: 'multiple') 		&& #multiple != false,
			'linebreak'=(local_defined: 'linebreak') 	&& #linebreak != false,
			'focus'=(local_defined: 'focus') 			&& #focus != false,
			'nowarning'=(local_defined: 'nowarning') 	&& #nowarning != false,
			'disabled'=(local_defined: 'disabled') 		&& #disabled != false
			);
		if: (self -> 'exceptionfieldtypes') >> #_type;
			// || (map: 'legend', 'fieldset', 'html') >> #_type;
			// never make certain field types required
			#field -> insert('required'=false);
		/if;

		#field -> (insert: 'type'=#_type);
		#field -> (insert: 'name'=#_name);

		local_defined('id') ? #field -> insert('id' = #id);
		local_defined('hint') ? #field -> insert('hint' = #hint);
		local_defined('default') ? #field -> insert('default' = #default); 
		local_defined('label') ? #field -> insert('label' = #label); 
		local_defined('size') ? #field -> insert('size' = #size); 
		local_defined('maxlength') ? #field -> insert('maxlength' = #maxlength); 
		local_defined('rows') ? #field -> insert('rows' = #rows); 
		local_defined('cols') ? #field -> insert('cols' = #cols); 
		local_defined('class') ? #field -> insert('class' = #class); 
		local_defined('raw') ? #field -> insert('raw' = #raw); 
		local_defined('confirmmessage') ? #field -> insert('confirmmessage' = #confirmmessage);
		local_defined('originaltype') ? #field -> insert('originaltype' = #originaltype);
		(local_defined: 'template') ? #field -> (insert: 'template'=#template);

		#field -> (insert: 'dbfield'=( (local_defined: 'dbfield') ? #dbfield | #_name ) );
		(local_defined: 'value') ? #field -> (insert: 'defaultvalue'=#value);
		
		// the following params are stored as reference, so the values of the params can be altered after adding a field simply by changing the referenced variable. 
		local_defined('options') ? #field -> insert('options' = @#options);
		local_defined('value') ? #field -> insert('value' = @#value);
		local_defined('validate') ? #field -> insert('validate' = @#validate);
		local_defined('filter') ? #field -> insert('filter' = @#filter);
		
		(self -> 'fields') -> (insert: #_name = @#field, #index);

		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'copyfield', -description='Copies a form field to a new name.',
		-required='name',
		-required='newname';
		local: 'timer'=knop_timer; 
		fail_if: #name == #newname, 7104, self -> error_msg(7104);
		if: (self -> 'fields') >> #name;
			local: 'copyfield'=(self -> 'fields') -> (find: #name) -> first -> value;
			#copyfield -> (insert: 'name' = #newname);
			(self -> 'fields') -> (insert: #newname = #copyfield);
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;


	define_tag: 'init', -description='Initiates form to grab keyvalue and set formmode if we have a database connected to the form. \
			Does nothing if no database is specified. ',
		-optional='get',
		-optional='post',
		-optional='keyvalue';
		local: 'timer'=knop_timer; 
		// Initiates form to grab keyvalue and set formmode if we have a database connected to the form. 
		// TODO: should we run init if form is not valid? Now we have a condition in lib before running init. 
		// TODO: how can we get the right formmode when showing an add form again after failed validation? Now we have an extra condition in lib for this
		
		if: (self -> 'database') -> type == 'database';
			(self -> 'db_keyvalue') = null;
			(self -> 'db_lockvalue') = null;
			local: '_params'=array,
				'source'='form',
				'field'=map;
			#_params = array;
			if: (local_defined: 'post');
				#_params -> (merge: client_postparams);
			/if;
			if: (local_defined: 'get');
				#_params -> (merge: client_getparams);
			/if;
			if: !(local_defined: 'post') && !(local_defined: 'get');
				#_params -> (merge: client_postparams);
				#_params -> (merge: client_getparams);
			/if;
			(self -> 'debug_trace') -> (insert: 'Init ');
		
			if: #_params >> '-lockvalue';
				if: #_params -> type == 'map';
					(self -> 'db_lockvalue')=((#_params -> (find: '-lockvalue' ) ) != '' 
						? (#_params -> (find: '-lockvalue' ) ) | null);
				else;
					(self -> 'db_lockvalue')=((#_params -> (find: '-lockvalue' ) -> first -> value) != '' 
						? (#_params -> (find: '-lockvalue' ) -> first -> value) | null);
				/if;
				(self -> 'debug_trace') -> (insert: tag_name + ': grabbing lockvalue from form ' + (self -> 'db_lockvalue'));
			else: (local_defined: 'keyvalue');
				(self -> 'db_keyvalue') = #keyvalue;
				(self -> 'debug_trace') -> (insert: tag_name + ': grabbing keyvalue from parameter ' + (self -> 'db_keyvalue'));
			else: #_params >> (self -> 'keyparamname');
				if: #_params -> type == 'map';
					(self -> 'db_keyvalue')=((#_params -> (find: (self -> 'keyparamname') ) ) != '' 
						? (#_params -> (find: (self -> 'keyparamname') ) ) | null);
				else;
					(self -> 'db_keyvalue')=((#_params -> (find: (self -> 'keyparamname') ) -> first -> value) != '' 
						? (#_params -> (find: (self -> 'keyparamname') ) -> first -> value) | null);
				/if;
				(self -> 'debug_trace') -> (insert: tag_name + ': grabbing keyvalue from form ' + (self -> 'db_keyvalue'));
			/if;
			if: (self -> 'db_lockvalue') == '' && (self -> 'db_keyvalue') == '';
				// we have no keyvalue or lockvalue - this must be an add operation
				(self -> 'formmode') = 'add';
				// create a keyvalue for the record to add
				(self -> 'db_keyvalue') = knop_unique;
				(self -> 'debug_trace') -> (insert: tag_name + ': generating keyvalue ' + (self -> 'db_keyvalue'));
			else: (self -> getbutton) == 'add';
				(self -> 'formmode') = 'add';
			else: (self -> formmode)=='';
				(self -> 'formmode') = 'edit';
			/if;
			(self -> 'debug_trace') -> (insert: tag_name + ': formmode ' + (self -> formmode));
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;


	define_tag: 'loadfields', -description='Overwrites all field values with values from either database, action_params or explicit -params. \
				Auto-detects based on current lasso_currentaction.\n\
			Parameters:\n\
				-params (optional) Array or map to take field values from instead of database or submit (using dbnames)\n\
				-get (optional flag) Only getparams will be used\n\
				-post (optional flag) Only postparams will be used\n\
				-inlinename (optional) The first record in the result from the specified inline will be used as field values\n\ 
				-database (optional) If a database object is specified, the first record from the latest search result of the database object will be used. \
					If -database is specified as flag (no value) and the form object has a database object attached to it, that database object will be used.',
		-optional='params',
		-optional='post',
		-optional='get',
		-optional='inlinename',
		-optional='database';
		local: 'timer'=knop_timer; 
		local: '_params'=array,
			'source'='form',
			'field'=map;
		(self -> 'fieldsource') = null;
		if: (local_defined: 'params');
			(self -> 'fieldsource') = 'params';
			local: 'source'='params';
			#_params = #params;
		else: (local_defined: 'database') && !(local_defined: 'inlinename');
			if: #database -> type == 'database';
				local: 'inlinename'=#database -> inlinename;
			else: self -> 'database' -> type == 'database';
				local: 'inlinename'=self -> 'database' -> inlinename;
			/if;
		/if;
			
		if: (local_defined: 'inlinename');
			(self -> 'fieldsource') = 'database';
			local: 'source'='params';
			#_params=map;
			records: -inlinename=#inlinename;
				loop: (field_name: -count);
					#_params -> (insert: (field_name: loop_count)  =  (field: (field_name: loop_count)) );
				/loop;
				loop_abort;
			/records;
		else: (self -> 'fieldsource') == null && lasso_currentaction != 'nothing';
			(self -> 'fieldsource') = 'database';
			local: 'source'='database';
		else: (self -> 'fieldsource') == null;
			(self -> 'fieldsource') = 'form';
			#_params = array;
			if: (local_defined: 'post');
				#_params -> (merge: client_postparams);
			/if;
			if: (local_defined: 'get');
				#_params -> (merge: client_getparams);
			/if;
			if: !(local_defined: 'post') && !(local_defined: 'get');
				#_params -> (merge: client_postparams);
				#_params -> (merge: client_getparams);
			/if;
		/if;
		(self -> 'debug_trace') -> (insert: tag_name + ': loading field values from ' + (self -> 'fieldsource'));
		local('fieldnames_done'=map, 'fields_samename'=array, 'params_fieldname'=array);
		iterate: (self -> 'fields'), (local: 'fieldpair');
			//#field = @(#fieldpair -> value);
			if: (self -> 'exceptionfieldtypes') !>> #fieldpair -> value -> (find: 'type') // do not load data for excluded form fields (maybe it should do that in some cases???)
				// && (map: 'legend', 'fieldset', 'html') !>> #fieldpair -> value -> (find: 'type')
				&& !(#fieldpair -> name -> (beginswith: '-')); // exclude field names that begin with "-"
				if(#fieldnames_done !>> #fieldpair -> name); // check if we are already done with this field name (for multiple fields with the same name)
					// find all fields with the same name
					#fields_samename = @((self -> 'fields') -> find(#fieldpair -> name));
					#params_fieldname = @(#_params -> find(#fieldpair -> name));
					if: #source == 'database' && found_count > 0;
						// load field values from database
						if: (#fieldpair -> value -> find: 'dbfield') != '';
							// first remove value to break reference
							(#fieldpair -> value) -> (remove: 'value');
							(#fieldpair -> value) -> (insert: 'value'=(field: (#fieldpair -> value -> find: 'dbfield')) );
						/if;
					else: #source == 'params';
						// load field values from explicit -params using dbfield names
						if: #_params >> (#fieldpair -> value -> find: 'dbfield') && (#fieldpair -> value -> find: 'dbfield') != '';
							// first remove value to break reference
							(#fieldpair -> value) -> (remove: 'value');
							if(#_params -> isa('map'));
								(#fieldpair -> value) -> (insert: 'value'=(#_params -> (find: (#fieldpair -> value -> find: 'dbfield') ) ) );
							/*else: #_params -> (find: (#fieldpair -> value -> find: 'dbfield') ) -> size > 1;
								// multiple field values
								local: 'valuearray'=array;
								iterate: #_params -> (find:  (#fieldpair -> value -> find: 'dbfield')), (local: 'parampair');
									#parampair -> value != '' ? #valuearray -> (insert: #parampair -> value);
								/iterate;
								(#fieldpair -> value) -> (insert: 'value'=#valuearray);*/
							else(#_params -> isa('array'));
								(#fieldpair -> value) -> (insert: 'value'=(#_params -> (find: (#fieldpair -> value -> find: 'dbfield')) -> first -> value) );
							/if;
						/if;
					else: #source == 'form';
						// load field values from form submission
						iterate(#fields_samename, local('fieldpair_samename'));
							// first remove value to break reference
							(#fieldpair_samename -> value) -> (remove: 'value');
							if(#params_fieldname -> size == #fields_samename -> size);
								// the number of submitted fields match the number of fields in the form
								(#fieldpair_samename -> value) -> (insert: 'value'=(#params_fieldname -> get(loop_count) -> value) );
							else;
								if: #params_fieldname -> size > 1;
									// multiple field values
									local: 'valuearray'=array;
									iterate: #_params -> (find:  (#fieldpair -> name)), (local: 'parampair');
										#parampair -> value != '' ? #valuearray -> (insert: #parampair -> value);
									/iterate;
									(#fieldpair_samename -> value) -> (insert: 'value'=#valuearray);
								else: #_params >> (#fieldpair -> name);
									(#fieldpair_samename -> value) -> (insert: 'value'=(#_params -> (find: #fieldpair_samename -> name) -> first -> value) );
								else;
									(#fieldpair_samename -> value) -> (insert: 'value'='');
								/if;
							/if;
						/iterate;
						#fieldnames_done -> insert(#fieldpair -> name);
					/if;
				/if;
				// apply filtering of field value (do this for all instances of the same field name, so outside of the #fieldnames_done check
				if(#fieldpair -> value -> find('filter') -> isa('tag'));
					(#fieldpair -> value) -> insert('value'= (#fieldpair -> value -> find('filter')) -> run(-params=(#fieldpair -> value -> find('value'))));
				/if;
			/if;
		/iterate;
		
		// capture keyvalue or lockvalue if we have a database object connected to the form
		if: (self -> 'database') -> type == 'database';
			//(self -> 'db_keyvalue') = null;
			//(self -> 'db_lockvalue') = null;
			if: (self -> 'fieldsource') == 'database';
				if: (self -> 'database') -> lockfield != '' && (self -> 'database') -> lockvalue != '';
					(self -> 'db_lockvalue') = (self -> 'database') -> lockvalue_encrypted;
					(self -> 'debug_trace') -> (insert: tag_name + ': grabbing lockvalue from database ' + (self -> 'db_lockvalue'));
				else: (self -> 'database') -> keyfield != '' && (self -> 'database') -> keyvalue != '';
					(self -> 'db_keyvalue') = (self -> 'database') -> keyvalue;
					(self -> 'debug_trace') -> (insert: tag_name + ': grabbing keyvalue from database ' + (self -> 'db_keyvalue'));
				/if;
			else;
				if: #_params >> '-lockvalue';
					if: #_params -> type == 'map';
						(self -> 'db_lockvalue')=((#_params -> (find: '-lockvalue' ) ) != '' 
							? (#_params -> (find: '-lockvalue' ) ) | null);
					else;
						(self -> 'db_lockvalue')=((#_params -> (find: '-lockvalue' ) -> first -> value) != '' 
							? (#_params -> (find: '-lockvalue' ) -> first -> value) | null);
					/if;
					(self -> 'debug_trace') -> (insert: tag_name + ': grabbing lockvalue from form ' + (self -> 'db_lockvalue'));
				else: #_params >> (self -> 'keyparamname');
					if: #_params -> type == 'map';
						(self -> 'db_keyvalue')=((#_params -> (find: (self -> 'keyparamname') ) ) != '' 
							? (#_params -> (find: (self -> 'keyparamname') ) ) | null);
					else;
						(self -> 'db_keyvalue')=((#_params -> (find: (self -> 'keyparamname') ) -> first -> value) != '' 
							? (#_params -> (find: (self -> 'keyparamname') ) -> first -> value) | null);
					/if;
					(self -> 'debug_trace') -> (insert: tag_name + ': grabbing keyvalue from form ' + (self -> 'db_keyvalue'));
				/if;
			/if;
			if: (self -> 'db_lockvalue') == '' && (self -> 'db_keyvalue') == '';
				// we have no keyvalue or lockvalue - this must be an add operation
				(self -> 'formmode') = 'add';
				// create a keyvalue for the record to add
				(self -> 'db_keyvalue') = knop_unique;
				(self -> 'debug_trace') -> (insert: tag_name + ': generating keyvalue ' + (self -> 'db_keyvalue'));
			else: (self -> formmode) == '';
				(self -> 'formmode') = 'edit';
			/if;
			(self -> 'debug_trace') -> (insert: tag_name + ': formmode ' + (self -> formmode));
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'clearfields', -description='Emtpies all form field values';
		local: 'timer'=knop_timer; 
		iterate: (self -> 'fields'), (local: 'fieldpair');
			if: (self -> 'exceptionfieldtypes') !>> #fieldpair -> value -> (find: 'type');
				// && (map: 'legend', 'fieldset', 'html') !>> #fieldpair -> value -> (find: 'type');
				// first remove value to break reference
				(#fieldpair -> value) -> (remove: 'value');
				(#fieldpair -> value) -> (insert: 'value'='');
			/if;
		/iterate;	
		if: (self -> 'database') -> type == 'database';
			(self -> 'db_keyvalue') = null;
			(self -> 'db_lockvalue') = null;
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'resetfields', -description='Resets all form field values to their initial values';
		local: 'timer'=knop_timer; 
		iterate: (self -> 'fields'), (local: 'fieldpair');
			if: (self -> 'exceptionfieldtypes') !>> #fieldpair -> value -> (find: 'type');
				//&& (map: 'legend', 'fieldset', 'html') !>> #fieldpair -> value -> (find: 'type');
				// first remove value to break reference
				(#fieldpair -> value) -> (remove: 'value');
				(#fieldpair -> value) -> (insert: 'value'=#fieldpair -> value -> (find: 'defaultvalue'));
			/if;
		/iterate;	
		if: (self -> 'database') -> type == 'database';
			(self -> 'db_keyvalue') = null;
			(self -> 'db_lockvalue') = null;
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'validate', -description='Performs validation and fills a transient array with field names that have input errors. \
				 form -> loadfields must be called first.';
		local: 'timer'=knop_timer; 
		
		// Performs validation and fills a transient array with field names that have input errors.
		// Must call -> loadfields first
		if: (self -> 'errors') == null;
			// initiate the errors array so we know validate has been performed
			(self -> 'errors') = array;
			iterate: (self -> 'fields'), (local: 'fieldpair');
				if: !( (self -> 'exceptionfieldtypes') >> #fieldpair -> value -> (find: 'type') );
					if: (#fieldpair -> value -> (find: 'required') ) 
						&& (#fieldpair -> value -> (find: 'value') ) == '';
						(self -> 'errors') -> (insert: (#fieldpair -> value -> (find: 'name') ));
					/if;
					if(#fieldpair -> value -> find('validate') -> isa('tag'));
						// perform validation expression on the field value
						local('result'=(#fieldpair -> value -> find('validate')) -> run(-params=#fieldpair -> value -> find('value')));
						if(#result === true || #result === 0);
							// validation was ok
						else(#result != 0 || #result -> size);
							// validation result was an error code or message
							(self -> 'errors') -> insert(#fieldpair -> value -> find('name') = #result);
						else;
							(self -> 'errors') -> insert(#fieldpair -> value -> find('name'));
						/if;
					/if;
				/if;
			/iterate;
		/if;
		(self -> 'debug_trace') -> (insert: tag_name + ': form is valid ' + ((self -> 'errors') -> size == 0));
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'isvalid', -description='Returns the result of form -> validate (true/false) without performing the validation again (unless it hasn\'t been performed already)';
		local: 'timer'=knop_timer; 
		// Returns the result of -> validate (true/false) without performing the validation again (unless it is needed)
		(self -> 'errors') == null ? self -> validate;
		(self -> 'debug_trace') -> (insert: tag_name + ': form is valid ' + ((self -> 'errors') -> size == 0));
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: (self -> 'errors') -> size == 0;
	/define_tag;


	define_tag: 'adderror', -description='adds the name for a field that has validation error, used for custom field validation. \
				calls form -> validate first if needed',
		-required='fieldname';
		local: 'timer'=knop_timer; 
		// adds a field that has error
		// calls ->validate first if needed, to make sure self -> 'errors' is an array
		(self -> 'errors') == null ? self -> validate;
		(self -> 'errors') -> (insert: #fieldname);
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;


	define_tag: 'errors', -description='Returns an array with fields that have input errors, or empty array if no errors or form has not been validated';
		// returns an array with fields that have input errors, or emtpy array if no errors or form has not been validated
		if: (self -> 'errors') == null;
			return: array;
		else;
			return: (self -> 'errors');
		/if;
	/define_tag;



	define_tag: 'updatefields', -description='Returns a pair array with fieldname=value, or optionally SQL string to be used in an update inline.\
		form -> loadfields must be called first.\n\
		Parameters:\n\
		-sql (optional)\n\
		-removedotbackticks (optional flag) Use with -sql for backward compatibility for fields that contain periods.  If you use periods in a fieldname then you cannot use a JOIN in Knop.',
		-optional='sql',
		-optional='removedotbackticks';
		local: 'timer'=knop_timer; 
		// Returns a pair array with fieldname=value, or optionally SQL string to be used in an update inline. Optionally use -removedotbackticks with -sql for backward compatibility with fields that contain periods.
		// Must call ->loadfields first.
		local: 'output'=array,
			'_sql'=(local_defined: 'sql'),
			'_removedotbackticks'=(local_defined: 'removedotbackticks'),
			'fieldvalue'=null, 'onevalue'=null;
		iterate: (self -> 'fields'), (local: 'fieldpair');
			if: !( (self -> 'exceptionfieldtypes') >> #fieldpair -> value -> (find: 'type') )
				&& !(#fieldpair -> value -> (find: 'name') -> (beginswith: '-'))
				&& (#fieldpair -> value -> (find: 'dbfield')) != '';
				// don't use submit etc and exclude fields whose name begins with -
				#fieldvalue = (#fieldpair -> value -> (find: 'value') );
				if: #fieldvalue -> type != 'array';
					// to support multiple values for one fieldname, like checkboxes
					#fieldvalue = array: #fieldvalue;
				/if;
				if: #_sql;
					if(#_removedotbackticks);
						#output -> (insert:  '`' + (encode_sql(knop_stripbackticks(#fieldpair -> value -> find('dbfield'))) ) + '`' 
							+ '="' + (encode_sql: (#fieldvalue -> (join: ',')) ) + '"');
					else;
						#output -> (insert:  '`' + (encode_sql(string_replace(knop_stripbackticks(#fieldpair -> value -> find('dbfield')), -find='.', -replace='`.`')) ) + '`' 
						+ '="' + (encode_sql: (#fieldvalue -> (join: ',')) ) + '"');
					/if;
				else;
					iterate: #fieldvalue, #onevalue;
						#output -> (insert:  (#fieldpair -> value -> (find: 'dbfield') ) 
							= #onevalue );
					/iterate;
				/if;
			/if;
		/iterate;
		if: #_sql;

			#output = '(' + #output -> (join: ',') + ')';

		/if;

		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @#output;
		
	/define_tag;
	
	
	define_tag: 'getbutton', -description='Returns what button was clicked on the form on the previous page. Assumes that submit buttons are named button_add etc. \
				Returns add, update, delete, cancel or any custom submit button name that begins with button_.';
		local: 'timer'=knop_timer; 
		if: (self -> 'formbutton') != '';
			// we have already found out once what button was clicked
			(self -> 'debug_trace') -> (insert: tag_name + ': cached ' + (self -> 'formbutton'));
			self -> 'tagtime_tagname'=tag_name;
			self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
			return: (self -> 'formbutton');
		/if;
		local: 'clientparams'=client_getparams;
		#clientparams -> (merge: client_postparams);
		// look for submit buttons, the least destructive first
		iterate: (array: 'cancel', 'save', 'add', 'delete'), (local: 'buttonname');
			if: #clientparams >> 'button_' + #buttonname 
				|| #clientparams >> 'button_' + #buttonname + '.x'
				|| #clientparams >> 'button_' + #buttonname + '.y';
				(self -> 'debug_trace') -> (insert: tag_name + ': built-in button name ' + #buttonname);
				(self -> 'formbutton') = #buttonname;
				self -> 'tagtime_tagname'=tag_name;
				self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
				return: #buttonname;
			/if;
		/iterate;
		// no button found yet - look for custom button names
		iterate: #clientparams, #buttonname;
			#buttonname -> type == 'pair' ? #buttonname =  #buttonname -> name;
			if: #buttonname -> (beginswith: 'button_');
				#buttonname -> (removeleading: 'button_') & (removetrailing: '.x') & (removetrailing: '.y');
				(self -> 'debug_trace') -> (insert: tag_name + ': custom button name ' + #buttonname);
				(self -> 'formbutton') = #buttonname;
				self -> 'tagtime_tagname'=tag_name;
				self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
				return: #buttonname;
			/if;
		/iterate;
		(self -> 'debug_trace') -> (insert: tag_name + ': No button found');
	/define_tag;
	
	define_tag: 'process', -description='Automatically handles a form submission and handles add, update, or delete. \
				Requires that a database object is specified for the form',
		-optional='user',
		-optional='lock',
		-optional='keyvalue';
		local: 'timer'=knop_timer; 
		fail_if: (self -> 'database') -> type != 'database', 7103, self -> error_msg(7103);

		(self -> 'error_code') = 0;
		(self -> 'error_msg') = string;
		
		if: self -> getbutton == 'cancel';
			// do nothing at all
			(self -> 'debug_trace') -> (insert: tag_name + ': cancelling ');
			
		else: self -> getbutton == 'save';
			self -> loadfields;
			if: self -> isvalid;
				if: (local_defined: 'user') && (self -> lockvalue) != '';
					(self -> database) -> (saverecord: (self -> updatefields), -lockvalue=(self -> lockvalue), -keyvalue=(self -> keyvalue), -user=#user);
				else;
					(self -> database) -> (saverecord: (self -> updatefields), -keyvalue=(self -> keyvalue));
				/if;
				if: self -> database -> error_code != 0;
					(self -> 'error_code') = self -> database -> error_code;
					(self -> 'error_msg') = 'Process: update record error ' + (self -> database -> error_msg);
				/if;
				(self -> 'debug_trace') -> (insert: tag_name + ': updating record ' + (self -> database -> error_msg) + ' ' + (self -> database -> error_code));
			else;
				(self -> 'error_code') = 7101; // Process: update record did not pass form validation
				(self -> 'debug_trace') -> (insert: tag_name + ': update record did not pass form validation');
			/if;
		
		else: self -> getbutton == 'add';
			self -> loadfields;
			if: self -> isvalid;
				(self -> database) -> (addrecord: (self -> updatefields), -keyvalue=(self -> keyvalue));
				if: self -> database -> error_code != 0;
					(self -> 'error_code') = self -> database -> error_code;
					(self -> 'error_msg') = 'Process: add record error ' + (self -> database -> error_msg);
				/if;
				(self -> 'debug_trace') -> (insert: tag_name + ': adding record ' + (self -> database -> error_msg) + ' ' + (self -> database -> error_code));
			else;
				(self -> 'error_code') = 7101; // Process: add record did not pass form validation
				(self -> 'debug_trace') -> (insert: tag_name + ': add record did not pass form validation');
				(self -> 'debug_trace') -> (insert: tag_name + ': reverting form mode to add');
			/if;

		else: self -> getbutton == 'delete';
			self -> loadfields;
			(self -> 'debug_trace') -> (insert: tag_name + ': will delete record with keyvalue ' + (self -> keyvalue) + ' lockvalue ' + (self -> lockvalue));
			if: (local_defined: 'user') && (self -> lockvalue) != '';
				(self -> database) -> (deleterecord: -lockvalue=(self -> lockvalue), -keyvalue=(self -> keyvalue), -user=#user);
			else;
				(self -> database) -> (deleterecord: -keyvalue=(self -> keyvalue));
			/if;
			if: self -> database -> error_code == 0;
				self -> resetfields;
			else;
				(self -> 'error_code') = self -> database -> error_code;
				(self -> 'error_msg') = 'Process: delete record error ' + (self -> database -> error_msg);
			/if;
			(self -> 'debug_trace') -> (insert: tag_name + ': deleting record ' + (self -> database -> error_msg) + ' ' + (self -> database -> error_code));
		else: false;
			// do not go here, database record should be loaded with a separate call
			if: (local_defined: 'lock');
				self -> database ->(getrecord: (local: 'keyvalue'), -lock, -user=#user);
				(self -> 'debug_trace') -> (insert: tag_name + ': loading record using lock' + (self -> database -> error_msg) + ' ' + (self -> database -> error_code));
			else;
				self -> database ->(getrecord: (local: 'keyvalue'), -user=#user);
				(self -> 'debug_trace') -> (insert: tag_name + ': loading record' + (self -> database -> error_msg) + ' ' + (self -> database -> error_code));
			/if;
			self -> (loadfields: -inlinename=(self -> database -> inlinename));
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'setformat', -description='Defines a html template for the form. \n\
			Parameters:\n\
			-template (optional string) html template, defaults to #label# #field##required#<br>\n\
			-buttontemplate (optional string) html template for buttons, defaults to #field#\n\
			-required (optional string) character(s) to display for required fields (used for #required#), defaults to *\n\
			-legend (optional string) legend for the entire form - if specified, a fieldset will also be wrapped around the form\n\
			-class (optional string) css class name that will be used for the form element, default none\n\
			-errorclass (optional string) css class name that will be used for the label to highlight input errors, if not defined style="color: red" will be used\n\
			-unsavedmarker (optional string) \n\
			-unsavedmarkerclass (optional string) \n\
			-unsavedwarning (optional string)',
		-optional='template', -type='string',
		-optional='buttontemplate', -type='string',
		-optional='required', -type='string',
		-optional='legend', -type='string',
		-optional='class', -type='string',
		-optional='errorclass', -type='string',
		-optional='unsavedmarker', -type='string',
		-optional='unsavedmarkerclass', -type='string',
		-optional='unsavedwarning', -type='string';
		local: 'timer'=knop_timer; 

		local_defined('template') ? (self -> 'template') = #template;
		local_defined('buttontemplate') ? (self -> 'buttontemplate') = #buttontemplate;
		local_defined('required') ? (self -> 'required') = #required;
		local_defined('legend') ? (self -> 'legend') = #legend;
		local_defined('class') ? (self -> 'class') = #class;
		local_defined('errorclass') ? (self -> 'errorclass') = #errorclass;
		local_defined('unsavedmarker') ? (self -> 'unsavedmarker') = #unsavedmarker;
		local_defined('unsavedmarkerclass') ? (self -> 'unsavedmarkerclass') = #unsavedmarkerclass;
		local_defined('unsavedwarning') ? (self -> 'unsavedwarning') = #unsavedwarning;

		if: local_defined: 'unsavedwarning';
			// escape quotes for javascript
			(self -> 'unsavedwarning') -> (replace: '\'', '\\\'');
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;


	define_tag: 'renderform', -description='Outputs HTML for the form fields, a specific field, a range of fields or all fields of a specific type. \
			Also inserts all needed javascripts into the page. \
			Use form -> setformat first to specify the html format, otherwise default format #label# #field##required#<br> is used. \n\
			Parameters:\n\
			-name (optional) Render only the specified field\n\
			-from (optional) Render form fields from the specified number index or field name. Negative number count from the last field.\n\
			-to (optional) Render form fields to the specified number index or field name. Negative number count from the last field.\n\
			-type (optional) Only render fields of this or these types (string or array)\n\
			-excludetype (optional) Render fields except of this or these types (string or array)\n\
			-legend (optional) Groups the rendered fields in a fieldset and outputs a legend for the fieldset\n\
			-start (optional) Only render the starting <form> tag\n\
			-end (optional) Only render the ending </form> tag\n\
			-xhtml (optional flag) XHTML valid output',
		-optional='name', -copy, 	// field name
		-optional='from', -copy, 	// number index or field name
		-optional='to', -copy, 		// number index or field name
		-optional='type', -copy,	// only output fields of this or these types (string or array)
		-optional='excludetype', -copy,	// output fields except of this or these types (string or array)
		-optional='legend',			// groups the rendered fields in a fieldset and outputs a legend for the fieldset
		-optional='start',			// only output the starting <form> tag
		-optional='end',			// only output the ending </form> tag
		-optional='xhtml';			// boolean, if set to true adjust output for XHTML
		local: 'timer'=knop_timer; 
		handle;knop_debug('Done with ' + self->type + ' -> ' + tag_name, -time, -type=self->type);/handle;
		
		// Outputs HTML for the form fields
		
		/*
			TODO: 
			Handling of multiple fields with the same name
		*/
		local: 'output'=string, 
			'onefield'=map, 
			'renderfield'=string, 
			'renderfield_base'=string, 
			'renderrow'=string,
			'formid'=null,
			'usehint'=array,
			'nowarning'=false,
			'fieldtype',
			'fieldvalue'=string,
			'fieldvalue_array'=array,
			'options'=array,
			'focusfield';

		local: 'clientparams'=client_getparams;
		#clientparams -> (merge: client_postparams);
		#clientparams -> (removeall: (self -> 'keyparamname'));
		#clientparams -> (removeall: '-lockvalue');
		#clientparams -> (removeall: '-action');
		#clientparams -> (removeall: '-xhtml');

		// local var that adjust tag endings if rendered for XHTML
		local: 'endslash' = ((self -> (xhtml: params)) ? ' /' | '');

		// page var to keep track of the number of forms that has been rendered on a page
		if: !(var_defined: 'knop_form_renderform_counter');
			var: 'knop_form_renderform_counter'=0;
		/if;
		
		$knop_form_renderform_counter += 1;

		if: (self -> 'id') != '';
			#formid = (self -> 'id');
		else: (self -> 'name') != '';
			#formid = (self -> 'name');
		else;
			#formid = 'form' + $knop_form_renderform_counter;
		/if;
		
		
		local: 'renderformStartTag'=false, 'renderformEndTag'=false;
		// remove params that should not stop formstarttag and formendtag from rendering
		params -> type == 'array' ? params -> (removeall: '-legend') & (removeall: '-xhtml');
		if: (self -> 'formaction') != null 
			&& (params -> size == 0 || (local_defined: 'start') );
			#renderformStartTag=true;
		/if;
		if: (self -> 'formaction') != null 
			&& (params -> size == 0 || (local_defined: 'end') );
			#renderformEndTag=true;
		/if;
		if: #renderformStartTag;
			// render opening form tag

			#output +='<form';
			(self -> 'debug_trace') -> insert(tag_name + ': formaction = ' + (self -> 'formaction'));
			if: (self -> 'formaction') != null;
				#output += ' action="' + (self -> 'formaction');
				if: (self -> 'method') == 'post' && !(self -> 'noautoparams');
					local: 'actionparams'=array;
					iterate: #clientparams, (local: 'clientparam');
						if: #clientparam -> type == 'pair';
							if: #clientparam -> name -> (beginswith: '-') 
							    && !(#clientparam -> name -> (beginswith: '-upload.'))
							    && !(#clientparam -> name -> (beginswith: '-upload2.'))
								&& #clientparam -> name != '-session'
								&& (self -> 'fields') !>> #clientparam -> name
								// check if param name appears in form action
								// turn param into [p][a][r][a][m] to avoid problems with most reserved regex characters like "."
								&& (string_findregexp: (self -> 'formaction'), -find='[?;&]['
									+ ((#clientparam -> name) -> (split: '') -> (join: '][')) 
									+ ']([&=]|$)', -ignorecase) -> size == 0;
								#actionparams -> (insert: (#clientparam -> name) + '=' + (encode_url: (#clientparam -> value)) );
							/if;
						else: #clientparam -> type == 'string' 
							&& #clientparam -> (beginswith: '-') 
							&& (self -> 'fields') !>> #clientparam
							// check if param appears in form action
							// turn param into [p][a][r][a][m] to avoid problems with most reserved regex characters like "."
							&& (string_findregexp: (self -> 'formaction'), -find='[?;&]['
								+ (#clientparam -> (split: '') -> (join: '][')) 
								+ ']([&=]|$)', -ignorecase) -> size == 0;
							#actionparams -> (insert: #clientparam );
						/if;
					/iterate;
					if: #actionparams -> size;
						#output += ((self -> 'formaction') >> '?' ? '&amp;' | '?' ) + #actionparams -> (join: '&amp;');
					/if;
				/if;
				#output += '"';
			/if;
			(self -> 'method') != null		? #output += ' method="' + (self -> 'method') + '"';
			(self -> 'name') != null		? #output += ' name="' + (self -> 'name') + '"';
			#output += ' id="' + #formid + '"';
			(self -> 'class') != null		? #output += ' class="' + (self -> 'class') + '"';
			(self -> 'enctype') != ''		? #output += ' enctype="' + (self -> 'enctype') + '"';
			(self -> 'raw') != null			? #output += ' ' + (self -> 'raw');
			!(self -> 'noscript') ? #output += ' onsubmit="return validateform(this)"';
			(self -> 'entersubmitblock') && !(self -> 'noscript')	? #output += ' onkeydown="return submitOk(event);" onfocus="submitBlock=true; return true;" onblur="submitBlock=false; return true;"';
			#output += '>\n';


			if: (self -> 'actionpath') != '' 
				&& !(self -> 'noautoparams') 
				&& (self -> 'fields') !>> '-action';
				// auto-add -action unless there is already an -action field in the form
				#output += '<input type="hidden" name="-action" value="' + (encode_html: (self -> 'actionpath')) + '"' + #endslash + '>\n';
			/if;
			if: (self -> 'fieldset');
				#output += '<fieldset>\n';
				#output +='<legend>' + (self -> 'legend') + '</legend>\n';
			/if;
			if: (self -> 'method') == 'get' && !(self -> 'noautoparams');
				iterate: #clientparams, (local: 'clientparam');
					if: #clientparam -> type == 'pair';
						if: #clientparam -> name -> (beginswith: '-')
							&& #clientparam -> name != '-session'
							&& (self -> 'fields') !>> #clientparam -> name
							// check if param name appears in form action
							// turn param into [p][a][r][a][m] to avoid problems with most reserved regex characters like .
							&& (string_findregexp: (self -> 'formaction'), -find='[?;&]['
								+ ((#clientparam -> name) -> (split: '') -> (join: '][')) 
								+ ']([&=]|$)', -ignorecase) -> size == 0;
							#output += '<input type="hidden" name="' + (#clientparam -> name) + '" value="' + (encode_html: (#clientparam -> value)) + '"' + #endslash + '>\n';
						/if;
					else: #clientparam -> type == 'string' 
						&& #clientparam -> (beginswith: '-')
						&& (self -> 'fields') !>> #clientparam
						// check if param appears in form action
						// turn param into [p][a][r][a][m] to avoid problems with most reserved regex characters like .
						&& (string_findregexp: (self -> 'formaction'), -find='[?;&]['
							+ (#clientparam -> (split: '') -> (join: ']['))
							+ ']([&=]|$)', -ignorecase) -> size == 0;
						#output += '<input type="hidden" name="' + #clientparam + '"' + #endslash + '>\n';
					/if;
				/iterate;
			/if;
			if: (self -> 'database') -> type == 'database';
				if: (self -> 'database') -> lockfield != '' && (self -> 'db_lockvalue') != '';
					#output += '<input type="hidden" name="-lockvalue" value="' + (encode_html: (self -> 'db_lockvalue')) + '"' + #endslash + '>\n';
				else: (self -> 'database') -> keyfield != '' && (self -> 'db_keyvalue') != '';
					#output += '<input type="hidden" name="' + (self -> 'keyparamname') + '" value="' + (encode_html: (self -> 'db_keyvalue')) + '"' + #endslash + '>\n';
				/if;
			/if;
		/if;
		
		
		if: !(local_defined: 'start') && !(local_defined: 'end');

			(local_defined: 'name') && !((self -> 'fields') >> #name) 	? 	return;
			
			(local_defined: 'name')		? local: 'from'=#name, 'to'=#name;
			!(local_defined: 'from') 		? local: 'from'=1;
			!(local_defined: 'to') 			? local: 'to'=(self -> 'fields') -> size;
			!(local_defined: 'type')		? local: 'type'=(self -> 'validfieldtypes');
			!(local_defined: 'excludetype')	? local: 'excludetype'=map;
			#type -> type == 'string' 		? #type = (map: #type);
			#excludetype -> type == 'string' ? #excludetype = (map: #excludetype);

			// only render form inputs if we are not only rendering the form tags
		
			// use field name if #from is a string
			#from -> type == 'string' ? #from = integer: ((self -> 'fields') -> (findindex: #from) -> first);
			#from == 0 ? #from = 1;
			// negative numbers count from the end
			#from < 0 ? #from = (self -> 'fields') -> size + #from;
	
			// use field name if #to is a string
			#to -> type == 'string' ? #to = integer: ((self -> 'fields') -> (findindex: #to) -> last);
			#to == 0 ? #to = (self -> 'fields') -> size;
			// negative numbers count from the end
			#to < 0 ? #to = (self -> 'fields') -> size + #to;

			// sanity check
			#from > #to ? #to = #from;
	
			local: 'template'=( (self -> 'template') != '' 
				? (self -> 'template') 		
				| '#label# #field##required#<br' + #endslash + '>\n' );
			local: 'buttontemplate'=( (self -> 'buttontemplate') != '' 
				? (self -> 'buttontemplate') 
				| (self -> 'template') != '' 
				? (self -> 'template')
				| '#field#\n' );
			local: 'requiredmarker'=(self -> 'required');
			local: 'defaultclass'=( (self -> 'class') != '' 
				? (self -> 'class') 		
				| '');
			local: 'errorclass'=( (self -> 'errorclass') != '' 
				? ' class="' + (self -> 'errorclass') + '"' 
				| ' style="color: red;"');
			
			if: (local_defined: 'legend');
				(self -> 'render_fieldset2_open') = true;
				#output += '<fieldset>\n'
					+ '<legend>' + #legend + '</legend>\n';
			/if;

			iterate: (self -> 'fields'), (local: 'fieldpair');
				#onefield = #fieldpair -> value;
				#fieldvalue = (#onefield -> (find: 'value'));
				#fieldvalue_array = #fieldvalue;
				if: #fieldvalue_array -> type != 'array';
					if: #fieldvalue_array >> '\r'; // Filemaker value list with multiple checked
						#fieldvalue_array = #fieldvalue_array -> (split: '\r');
					else: #fieldvalue_array >> ','; // Other database with multiple checked
						#fieldvalue_array = #fieldvalue_array -> (split: ',');
					else;
						#fieldvalue_array = array: #fieldvalue_array;
					/if;
				/if;
				if: #onefield >> 'options';
					#options = (#onefield -> find: 'options');
					// convert types for pair 
					iterate: #options, (local: 'option');
						if: #option -> type != 'pair';
							#option = (pair: #option = #option);
						/if;
						// name must be string to make sure comparsions work
						(#option -> name) = (string: #option -> name);
					/iterate;
				/if;
				
				if: loop_count >= #from 
					&& loop_count <= #to
					&& #type >> #onefield -> (find: 'type')
					&& !(#excludetype >> #onefield -> (find: 'type'));
					if: (self -> 'unsavedwarning') == '';
						#nowarning=true;
					else;
						#nowarning=#onefield -> (find: 'nowarning');
					/if;
					
					if: #onefield -> (find: 'template') -> size;
						#renderrow = #onefield -> (find: 'template');
					else: (map: 'submit', 'reset', 'image') >> #onefield -> (find: 'type');
						#renderrow=#buttontemplate;
					else;
						#renderrow=#template;
					/if;
					local: 'id'= string;
					if: (#onefield -> (find: 'id')) !='';
						#id = (#onefield -> (find: 'id'));
					else;
						#id = #formid + '_' + (#onefield -> (find: 'name')) + loop_count;
					/if;
					if: (self -> 'errors') -> size == 0 && #focusfield == '' && #onefield -> (find: 'focus');
						// give this field focus
						#focusfield = #id;
					/if;
					
					// set field label, with error marker if field validation failed
					// if: (self -> 'exceptionfieldtypes') >> (#onefield -> (find: 'type')) && (#onefield -> (find: 'type')) != 'file';
					//	#renderrow -> (replace: '#label#', '');
					//else: 
					if: (self -> 'errors') -> type == 'array' && (self -> 'errors') >> (#onefield -> (find: 'name'));
						#renderrow -> (replace: '#label#', 
							'<label for="' + #id + '" id="' + #id + '_label" ' + #errorclass + '>' + (#onefield -> find('label')) + '</label>');
						if: #focusfield == '';
							#focusfield = #id;
						/if;
					else;
						#renderrow -> (replace: '#label#', '<label for="' + #id + '" id="' + #id + '_label">' + (#onefield -> find('label')) + '</label>');
					/if;
					
					// set markers for required fields 
					if: #onefield -> (find: 'required') && !((self -> 'exceptionfieldtypes') >> (#onefield -> (find: 'type')) );
						#renderrow -> (replace: '#required#', (encode_smart: #requiredmarker));
					else;
						#renderrow -> (replace: '#required#', '');
					/if;
					#renderfield=string;
					#renderfield_base = ' name="' + (encode_html: (#onefield -> (find: 'name'))) + '"'
						+ (#onefield >> 'class' ?  ' class="' + (#onefield -> (find: 'class')) + '"' 
							| (#defaultclass != '' ? ' class="' + #defaultclass + '"') )
						+ ' id="' +  (encode_html: #id) + '"'
						+ (#onefield >> 'raw'	?  ' ' + (#onefield -> (find: 'raw')) )
						+ (#onefield -> (find: 'disabled') ? ' disabled="disabled"');

					#fieldtype=(#onefield -> (find: 'type'));
					if: #fieldtype == 'search' && client_type !>> 'WebKit';
						// only show <input type=search" for WebKit based browsers like Safari
						#fieldtype = 'text';
					/if;
					select: #fieldtype;
					case: 'html';
						#renderrow = #template;
						#renderrow -> (replace: '#label#', '');
						#renderrow -> (replace: '#required#', '');
						#renderfield = #fieldvalue + '\n';
					case: 'legend';
						#renderrow = '';
						if: (self -> 'render_fieldset_open');
							#output += '</fieldset>\n';
							(self -> 'render_fieldset_open') = false;
						/if;
						#output += '<fieldset' 
							+ (#onefield >> 'class' ?  ' class="' + (#onefield -> (find: 'class')) + '"' 
								| (#defaultclass != '' ? ' class="' + #defaultclass + '"') )
							+ ((#onefield -> (find: 'id') != '') ? ' id="' + #id + '"') 
							+ '>\n';
						(self -> 'render_fieldset_open') = true;
						#output += '<legend>' + (encode_html: #fieldvalue) + '</legend>\n';
					case: 'fieldset';
						#renderrow = '';
						if: (self -> 'render_fieldset_open');
							#output += '</fieldset>\n';
							(self -> 'render_fieldset_open') = false;
						/if;
						if: #fieldvalue !== false;
							(self -> 'render_fieldset_open') = true;
							#output += '<fieldset' 
							+ (#onefield >> 'class' ?  ' class="' + (#onefield -> (find: 'class')) + '"' 
								| (#defaultclass != '' ? ' class="' + #defaultclass + '"') )
							+ ((#onefield -> (find: 'id') != '') ? ' id="' + #id + '"') 
							+ '>\n<legend>' + (encode_html: #fieldvalue) + '</legend>\n'; // must contain a legend
						/if;
					case: 'hidden';
						#renderfield += '<input type="hidden"' 
							+ #renderfield_base
							+ ' value="' + (encode_html: #fieldvalue) + '"';
						#renderfield += #endslash + '>';
						#renderrow = '';
						#output += #renderfield + '\n';
					case: 'text';
						#renderfield += '<input type="text"' 
							+ #renderfield_base
							+ ' value="' + (encode_html: #fieldvalue) + '"'
							+ (#onefield >> 'size' 	? ' size="' + (#onefield -> (find: 'size')) + '"' )
							+ (#onefield >> 'maxlength' 	? ' maxlength="' + (#onefield -> (find: 'maxlength')) + '"' );
						if: !(self -> 'noscript') && (#onefield -> (find: 'hint')) != '';
							#renderfield += ' onfocus="clearHint(this)" onblur="setHint(this, \'' (#onefield -> (find: 'hint')) '\')"';
							#usehint -> (insert: (#onefield -> find: 'name') = #id);
						/if;
						if: !(self -> 'noscript') && !#nowarning;
							#renderfield += ' onkeydown="dirtyvalue(this)" onkeyup="makedirty(this)"';
						/if;
						#renderfield += #endslash + '>';
					case: 'search';
						#renderfield += '<input type="search"' 
							+ #renderfield_base
							+ ' value="' + (encode_html: #fieldvalue) + '"'
							+ (#onefield >> 'size' 	? ' size="' + (#onefield -> (find: 'size')) + '"' );
						if: (#onefield -> (find: 'hint')) != '';
							#renderfield += ' placeholder="' + (encode_html: (#onefield -> (find: 'hint'))) + '"';
						/if;
						if: !(self -> 'noscript') && !#nowarning;
							#renderfield += ' onkeydown="dirtyvalue(this)" onkeyup="makedirty(this)"';
						/if;
						#renderfield += #endslash + '>';
					case: 'password';
						#renderfield += '<input type="password"' 
							+ #renderfield_base
							+ ' value="' + (encode_html: #fieldvalue) + '"'
							+ (#onefield >> 'size' 	? ' size="' + (#onefield -> (find: 'size')) + '"' );
						if: !(self -> 'noscript') && !#nowarning;
							#renderfield += ' onkeydown="dirtyvalue(this)" onkeyup="makedirty(this)"';
						/if;
						#renderfield += #endslash + '>';
					case: 'textarea';
						#renderfield += '<textarea' 
							+ #renderfield_base
							+ (#onefield >> 'cols' 	? ' cols="' + (#onefield -> (find: 'cols')) + '"')
							+ (#onefield >> 'rows' 	? ' rows="' + (#onefield -> (find: 'rows')) + '"');
						if: !(self -> 'noscript') && (#onefield -> (find: 'hint')) != '';
							#renderfield += ' onfocus="clearHint(this)" onblur="setHint(this, \'' (#onefield -> (find: 'hint')) '\')"';
							#usehint -> (insert: (#onefield -> find: 'name') = #id);
						/if;
						if: !(self -> 'noscript') && !#nowarning;
							#renderfield += ' onkeydown="dirtyvalue(this)" onkeyup="makedirty(this)"';
						/if;
						#renderfield += '>'
							+ (encode_html: #fieldvalue) + '</textarea>';
					case: 'checkbox';
						local: 'optioncount'=integer;
						#renderfield += '<div class="inputgroup'
							+ (#onefield >> 'class' ?  ' ' + (#onefield -> find('class'))
							| (#defaultclass != '' ? ' ' + #defaultclass) ) 
							+ '" id="' + #id + '">\n';
						iterate: #options, (local: 'option');
							#optioncount += 1;
							#renderfield += (#optioncount > 1 && (#onefield -> find: 'linebreak') ? '<br' + #endslash + '>') + '\n';
							if: #option -> name == '-optgroup';
								#renderfield += (!(#onefield -> find: 'linebreak') && #optioncount > 1 ? '\n<br' + #endslash + '>');
								if: #option -> value != '-optgroup';
									#renderfield += #option -> value
										+ (!(#onefield -> find: 'linebreak') ? '<br' + #endslash + '>\n');
								/if;
							else;
								#renderfield +=  '<input type="checkbox"'
									+ (string_replaceregexp: #renderfield_base, -find='id="(.+?)"', -replace=('id="\\1_' + #optioncount + '"'))
									+ ' value="' + (encode_html: #option -> name) + '"';
								if: #option -> name != '' && #fieldvalue_array >> #option -> name;
									#renderfield +=' checked="checked"';
								/if;
								if: !(self -> 'noscript') && !#nowarning;
									#renderfield += ' onclick="makedirty();"';
								/if;
								#renderfield += #endslash + '> <label for="' + #id + '_' + #optioncount 
									+ '" id="' + #id + '_' + #optioncount + '_label"';
								if: (self -> 'noscript') && !#nowarning;
									#renderfield += ' onclick="makedirty();"';
								/if;
								#renderfield += '>' + (#option -> value) + '</label> ';
							/if;
						/iterate;
						#renderfield += '</div>\n';
					case: 'radio';
						local: 'optioncount'=integer;
						#renderfield += '<div class="inputgroup'
							+ (#onefield >> 'class' ?  ' ' + (#onefield -> find('class'))
							| (#defaultclass != '' ? ' ' + #defaultclass) ) 
							+ '" id="' + #id + '">\n';
						iterate: #options, (local: 'option');
							#optioncount += 1;
							#renderfield += (#optioncount > 1 && (#onefield -> find: 'linebreak') ? '<br' + #endslash + '>') + '\n';
							if: #option -> name == '-optgroup';
								#renderfield += (!(#onefield -> find: 'linebreak') && #optioncount > 1 ? '\n<br' + #endslash + '>');
								if: #option -> value != '-optgroup';
									#renderfield += #option -> value
										+ (!(#onefield -> find: 'linebreak') ? '<br' + #endslash + '>\n');
								/if;
							else;
								#renderfield += '<input type="radio"'
									+ (string_replaceregexp: #renderfield_base, -find='id="(.+?)"', -replace=('id="\\1_' + #optioncount + '"'))
									+ ' value="' + (encode_html: #option -> name) + '"';
								if: #option -> name != '' && #fieldvalue_array >> #option -> name;
									#renderfield +=' checked="checked"';
								/if;
								if: !(self -> 'noscript') && !#nowarning;
									#renderfield += ' onclick="makedirty();"';
								/if;
								#renderfield += #endslash + '> <label for="' + #id + '_' + #optioncount 
									+ '" id="' + #id + '_' + #optioncount + '_label"';
								if: !(self -> 'noscript') && !#nowarning;
									#renderfield += ' onclick="makedirty();"';
								/if;
								#renderfield += '>' + (#option -> value) + '</label> ';
							/if;
						/iterate;
						#renderfield += '</div>\n';
					case: 'select';
						#renderfield += '<select '
							+ #renderfield_base
							+ (#onefield -> (find: 'multiple') ? ' multiple')
							+ (#onefield >> 'size' 	? ' size="' + (#onefield -> (find: 'size')) + '"' );
						if: !(self -> 'noscript') && !#nowarning;
							if: #renderfield >> 'onchange="';
								#renderfield -> (replace: 'onchange="', 'onchange="makedirty();');
							else;
								#renderfield += ' onchange="makedirty()"';
							/if;
						/if;
						#renderfield += '>\n';
						if: (#onefield -> (find: 'default')) != '' && (#onefield -> (find: 'size')) <= 1;
							#renderfield += '<option'
								+ ' value="">' + (encode_html: (#onefield -> (find: 'default'))) + '</option>\n';
							#renderfield += '<option'
								+ ' value=""></option>\n';
						/if;
						local: 'optgroup_open'=false;
						iterate: #options, (local: 'option');
							if: #option -> name == '-optgroup';
								if: #optgroup_open;
									#renderfield += '</optgroup>\n';
								/if;
								if: #option -> value != '-optgroup';
									#renderfield += '<optgroup label="' + (#option -> value) + '">\n';
									#optgroup_open = true;
								/if;
							else;
								#renderfield += '<option'
									+ ' value="' + (encode_html: #option -> name) + '"';
								if: #option -> name != '' && #fieldvalue_array >> #option -> name;
									#renderfield +=' selected="selected"';
								/if;
								#renderfield +=  '>' + (encode_html: #option -> value) + '</option>\n';
							/if;
						/iterate;
						if: #optgroup_open;
							#renderfield += '</optgroup>\n';
						/if;
						#renderfield += '</select>\n';
					case: 'submit';
						#renderfield += '<input type="submit"' 
							+ #renderfield_base
							+ ' value="' + (encode_html: #fieldvalue) + '"';
						if: (self -> formmode) == 'add' 
							&& !(#onefield -> (find: 'disabled')) // already disabled
							&& (#onefield -> (find: 'originaltype') == 'savebutton' || #onefield -> (find: 'originaltype') == 'deletebutton'
							|| #onefield -> (find: 'name') == 'button_save' || #onefield -> (find: 'name') == 'button_delete');
							#renderfield += ' disabled="disabled"';
						/if;
						if: !(self -> 'noscript') 
							&& (#onefield -> (find: 'name') == 'button_delete' 
								|| #onefield -> (find: 'originaltype') == 'deletebutton' 
								|| #onefield -> (find: 'confirmmessage') != '');
							local: 'confirmmessage'=(#onefield -> (find: 'confirmmessage') != '' 
								? #onefield -> (find: 'confirmmessage') | 'Really delete?');
							#confirmmessage -> (replace: '"', '&quot;');
							#confirmmessage -> (replace: '\'', '\\\'');
							#renderfield += ' onclick="return confirm(\'' + #confirmmessage +  '\')"';
						/if;
						#renderfield += #endslash + '>';
					case: 'reset';
						#renderfield += '<input type="reset"' 
							+ #renderfield_base
							+ ' value="' + (encode_html: #fieldvalue) + '"';
						if: !(self -> 'noscript') && #onefield -> (find: 'confirmmessage') != '';
							local: 'confirmmessage'=#onefield -> (find: 'confirmmessage');
							#confirmmessage -> (replace: '"', '&quot;');
							#confirmmessage -> (replace: '\'', '\\\'');
							#renderfield += ' onclick="if(confirm(\'' + #confirmmessage +  '\')){makeundirty();return true}else{return false};"';
						else: !(self -> 'noscript');
							#renderfield += ' onclick="makeundirty();"';
						/if;
						#renderfield += #endslash + '>';
					case: 'image';
						#renderfield += '<input type="image"' 
							+ #renderfield_base
							+ ' value="' + (encode_html: #fieldvalue) + '"';
						if: (self -> formmode) == 'add' && 
							(#onefield -> (find: 'originaltype') == 'savebutton' || #onefield -> (find: 'originaltype') == 'deletebutton'
							|| #onefield -> (find: 'name') == 'button_save' || #onefield -> (find: 'name') == 'button_delete');
							#renderfield += ' disabled="disabled"';
						/if;
						if: !(self -> 'noscript')
							&& (#onefield -> (find: 'name') == 'button_delete' 
								|| #onefield -> (find: 'originaltype') == 'deletebutton' 
								|| #onefield -> (find: 'confirmmessage') != '');
							local: 'confirmmessage'=(#onefield -> (find: 'confirmmessage') != '' 
								? #onefield -> (find: 'confirmmessage') | 'Really delete?');
							#confirmmessage -> (replace: '"', '&quot;');
							#confirmmessage -> (replace: '\'', '\\\'');
							#renderfield += ' onclick="return confirm(\'' + #confirmmessage +  '\')"';
						/if;
						#renderfield += #endslash + '>';
					case: 'file';
						#renderfield += '<input type="file"' 
							+ #renderfield_base;
						if: !(self -> 'noscript') && !#nowarning;
							if: #renderfield >> 'onchange="';
								#renderfield -> (replace: 'onchange="', 'onchange="makedirty();');
							else;
								#renderfield += ' onchange="makedirty()"';
							/if;
						/if;
						#renderfield += #endslash + '>';
					/select;
					#renderrow -> (replace: '#field#', #renderfield);
					#output += #renderrow;
				/if; 
			/iterate;
			
				
		/if; //  !(local_defined: 'start') && !(local_defined: 'end');

		// Add just the needed scripts to support the client side functionality
		if(!(self -> 'noscript'));
			#output >> 'togglecontrol(' 
				? self -> (afterhandler: -endscript='function togglecontrol(obj){
					// toggles checkboxes and radios when clicking on label (for browsers that don´t support this already)
					switch (obj.type){
					case \'checkbox\':
						obj.checked=!obj.checked;
						break;
					case \'radio\':
						obj.checked=true;
						break;
					}
				}');
			#output >> 'setHint('
				? 		self -> (afterhandler: -endscript='function setHint(myField, hint) {
					if(myField.value==\'\') {
						if(myField.name.indexOf(\'off_\') != 0) {
							myField.name=\'off_\' + myField.name;
						}
						myField.value=hint;
						getStyleObject(myField.id).color=\'#aaa\';
					}
				}
				function clearHint(myField) {
					if(myField.name.indexOf(\'off_\') == 0) {
						myField.name=myField.name.substr(4);
						myField.value=\'\';
						getStyleObject(myField.id).color=\'black\';
					}
				}
				function getStyleObject(objectId) {
					if(document.getElementById && document.getElementById(objectId)) {
					return document.getElementById(objectId).style;
					} else {
					return false;
					}
				}');
		
			#output >> 'makedirty(' || #output >> 'validateform('
				? self -> (afterhandler: -endscript='
				var dirty=' + ((self -> 'errors') -> size ? 'true' | 'false') + ';
				var dirtycheckname=null;
				var dirtycheckvalue=null;
				var submitBlock=false;
				function validateform(myForm) {
					// perform validation of myForm here
					if(submitBlock){return false};
					makeundirty();
					return true;
				}
				
				function dirtyvalue(obj){ // to be called at keydown to track if a text field changes or if arrow keys/tab/cmd-keys are pressed
					 dirtycheckname = obj.name;
					 dirtycheckvalue = obj.value;
				}
				function makeundirty(){
					dirty=false; 
					dirtymarker();
					window.onbeforeunload=null;
				}
				function makedirty(obj){
					if(obj){ // if object is specified then we are tracking if the value changes through keydown/keyup 
						if (obj.value == dirtycheckvalue || obj.name != dirtycheckname) { // no change or tabbed to another field - return immediately
							return
						}
					}
					dirty=true; 
					dirtymarker();
				}
				function checkdirty(){
					if(dirty){
						return confirm(\'' + (self -> 'unsavedwarning') + '\')
					} else {return true}
				}
				
				function dirtymarker() {
					var obj = document.getElementById(\'' + (self -> 'unsavedmarker') + '\');
					if(dirty && obj){
						jscss(\'add\',obj,\'' + (self -> 'unsavedmarkerclass') + '\');
					}else if(obj) {
						jscss(\'remove\',obj,\'' + (self -> 'unsavedmarkerclass') + '\');
					}
				}
				function jscss(a,o,c1,c2){
					/*
						a = action: swap, add, remove, check
						o = object
						c1 = name of the class (first class for swap)
						c2 = for swap, name of the second class
						http://onlinetools.org/articles/unobtrusivejavascript/cssjsseparation.html
					*/
					switch (a){
						case \'swap\':
							o.className=!jscss(\'check\',o,c1)?o.className.replace(c2,c1): o.className.replace(c1,c2);
							break;
						case \'add\':
							if(!jscss(\'check\',o,c1)){o.className+=o.className?\' \'+c1:c1;}
							break;
						case \'remove\':
							var rep=o.className.match(\' \'+c1)?\' \'+c1:c1;
							o.className=o.className.replace(rep,\'\');
							break;
						case \'check\':
							return new RegExp(\'\\\\b\'+c1+\'\\\\b\').test(o.className);
							break;
					}
				}
				if(dirty) {makedirty()};');
				
				(self -> 'unsavedwarning') != '' 
					? self -> (afterhandler: -endscript='function beforeunload() {
					if(dirty) {return \'' + (self -> 'unsavedwarning') + '\';}
				}
				window.onbeforeunload=beforeunload;');
				
				#output >> 'submitOk'
					? self -> (afterhandler: -endscript='function submitOk(e) { // prevents submit-on-enter
					var keynum;
					var elTarget;
					var elType;
		
					// get keycode for the event 
					if(window.event) keynum = e.keyCode; // IE
					else if(e.which) keynum = e.which; // DOM
		
					// get target
					if (e.target) elTarget = e.target;
					else if (e.srcElement) elTarget = e.srcElement;
		
					if(elTarget.tagName.toLowerCase()  == \'input\') elType = elTarget.getAttribute(\'type\').toLowerCase();
					submitBlock=false;
					if (elType != \'submit\' && elType != \'image\' && elType != \'reset\') {
						// allow enter submit when submit button/image or reset button has focus
						if (keynum==13) submitBlock=true;
					}
					return true;
				}');
			/if; // noscript

		if: false && $knop_form_renderform_counter <= 1;
										self -> (afterhandler: -headscript=
											'function getStyleObject(objectId) {
												if(document.getElementById && document.getElementById(objectId)) {
												return document.getElementById(objectId).style;
												} else {
												return false;
												}
											}
							
											function jscss(a,o,c1,c2){
												/*
													a = action: swap, add, remove, check
													o = object
													c1 = name of the class (first class for swap)
													c2 = for swap, name of the second class
													http://onlinetools.org/articles/unobtrusivejavascript/cssjsseparation.html
												*/
												switch (a){
													case \'swap\':
														o.className=!jscss(\'check\',o,c1)?o.className.replace(c2,c1): o.className.replace(c1,c2);
														break;
													case \'add\':
														if(!jscss(\'check\',o,c1)){o.className+=o.className?\' \'+c1:c1;}
														break;
													case \'remove\':
														var rep=o.className.match(\' \'+c1)?\' \'+c1:c1;
														o.className=o.className.replace(rep,\'\');
														break;
													case \'check\':
														return new RegExp(\'\\\\b\'+c1+\'\\\\b\').test(o.className);
														break;
												}
											}
											
											function togglecontrol(obj){
												// toggles checkboxes and radios when clicking on label (for browsers that don´t support this already)
												switch (obj.type){
												case \'checkbox\':
													obj.checked=!obj.checked;
													break;
												case \'radio\':
													obj.checked=true;
													break;
												}
											}
											
											function setHint(myField, hint) {
												if(myField.value==\'\') {
													if(myField.name.indexOf(\'off_\') != 0) {
														myField.name=\'off_\' + myField.name;
													}
													myField.value=hint;
													getStyleObject(myField.id).color=\'#aaa\';
												}
											}
											function clearHint(myField) {
												if(myField.name.indexOf(\'off_\') == 0) {
													myField.name=myField.name.substr(4);
													myField.value=\'\';
													getStyleObject(myField.id).color=\'black\';
												}
											}
											var dirty=' + ((self -> 'errors') -> size ? 'true' | 'false') + ';
											var dirtycheckname=null;
											var dirtycheckvalue=null;
											var submitBlock=false;
							
											function validateform(myForm) {
												// perform validation of myForm here
												if(submitBlock){return false};
												makeundirty();
												return true;
											}
											
											function dirtyvalue(obj){ // to be called at keydown to track if a text field changes or if arrow keys/tab/cmd-keys are pressed
												 dirtycheckname = obj.name;
												 dirtycheckvalue = obj.value;
											}
											function makeundirty(){
												dirty=false; 
												dirtymarker();
												window.onbeforeunload=null;
											}
											function makedirty(obj){
												if(obj){ // if object is specified then we are tracking if the value changes through keydown/keyup 
													if (obj.value == dirtycheckvalue || obj.name != dirtycheckname) { // no change or tabbed to another field - return immediately
														return
													}
												}
												dirty=true; 
												dirtymarker();
											}
											function checkdirty(){
												if(dirty){
													return confirm(\'' + (self -> 'unsavedwarning') + '\')
												} else {return true}
											}
											function beforeunload() {
												if(dirty) {
													return \'' + (self -> 'unsavedwarning') + '\';
												}
											}
											
											function dirtymarker() {
												var obj = document.getElementById(\'' + (self -> 'unsavedmarker') + '\');
												if(dirty && obj){
													jscss(\'add\',obj,\'' + (self -> 'unsavedmarkerclass') + '\');
												}else if(obj) {
													jscss(\'remove\',obj,\'' + (self -> 'unsavedmarkerclass') + '\');
												}
											}
											' + ((self -> 'unsavedwarning') != '' ? 'window.onbeforeunload=beforeunload;') + '
											
											function submitOk(e) { // prevents submit-on-enter
												var keynum;
												var elTarget;
												var elType;
							
												// get keycode for the event 
												if(window.event) keynum = e.keyCode; // IE
												else if(e.which) keynum = e.which; // DOM
							
												// get target
												if (e.target) elTarget = e.target;
												else if (e.srcElement) elTarget = e.srcElement;
							
												if(elTarget.tagName.toLowerCase()  == \'input\') elType = elTarget.getAttribute(\'type\').toLowerCase();
												submitBlock=false;
												if (elType != \'submit\' && elType != \'image\' && elType != \'reset\') {
													// allow enter submit when submit button/image or reset button has focus
													if (keynum==13) submitBlock=true;
												}
												return true;
											}
											
											');
		/if;
		if: !(self -> 'noscript') && #usehint -> size > 0;
			local: 'hintscript'=string;
			// #usehint is a pair array with name=id
			iterate: #usehint, (local: 'hintfield');
				if: (self -> 'fields') >> #hintfield -> name;
					#onefield = (self -> 'fields') -> (find: #hintfield -> name) -> first -> value;
					#hintscript += 'setHint(document.getElementById(\'' + (encode_html: #hintfield -> value) + '\'), \'' 
						+ (#onefield -> (find: 'hint')) + '\');\n';
				/if;
			/iterate;
			
			self -> (afterhandler: -endscript=#hintscript);
		/if;
		if: !(self -> 'noscript') && #focusfield != '';
			self -> (afterhandler: -endscript='document.getElementById(\'' + #focusfield + '\').focus();document.getElementById(\'' + #focusfield + '\').select();');
		/if;

		if: (self -> 'render_fieldset_open') && (params -> size == 0 || (local_defined: 'end'));
			// inner fieldset is open
			(self -> 'render_fieldset_open') = false;
			#output += '</fieldset>\n';
		/if;
		if: (self -> 'render_fieldset2_open') && (local_defined: 'legend');
			// inner fieldset is open
			(self -> 'render_fieldset2_open') = false;
			#output += '</fieldset>\n';
		/if;

		if: #renderformEndTag;
			if: (self -> 'fieldset');
				#output += '</fieldset>\n';
			/if;
		
			// render closing form tag
			#output += '</form>';
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @#output;
	/define_tag;

	define_tag: 'renderhtml', -description='Outputs form data as plain HTML, a specific field, a range of fields or all fields of a specific type. \
			Some form field types are excluded, such as submit, reset, file etc. \
			Use form -> setformat first to specify the html format, otherwise default format #label#: #field#<br> is used.\n\
			Parameters:\n\
			-name (optional) Render only the specified field\n\
			-from (optional) Render fields from the specified number index or field name\n\
			-to (optional) Render fields to the specified number index or field name\n\
			-type (optional) Only render fields of this or these types (string or array)\n\
			-excludetype (optional) Render fields except of this or these types (string or array)\n\
			-legend (optional) Groups the rendered fields in a fieldset and outputs a legend for the fieldset\n\
			-xhtml (optional flag) XHTML valid output',
		-optional='name', -copy,	// field name
		-optional='from', -copy, 	// number index or field name
		-optional='to', -copy, 		// number index or field name
		-optional='type', -copy,	// only output fields of this or these types (string or array)
		-optional='excludetype', -copy,	// do not output fields of this or these types (string or array)
		-optional='legend',			// groups the rendered fields in a fieldset and outputs a legend for the fieldset
		-optional='xhtml';			// boolean, if set to true adjust output for XHTML
		local: 'timer'=knop_timer; 
		
		local: 'output'=string, 
			'onefield'=map, 
			'renderfield'=string, 
			'renderfield_base'=string, 
			'renderrow'=string,
			'fieldvalue'=string,
			'fieldvalue_array'=array,
			'options'=array,
			'usehint'=array;

		
		// local var that adjust tag endings if rendered for XHTML
		local: 'endslash' = ((self -> (xhtml: params)) ? ' /' | '');

		(local_defined: 'name') && !((self -> 'fields') >> #name) 	? 	return;

		(local_defined: 'name')		? local: 'from'=#name, 'to'=#name;
		!(local_defined: 'from') 		? local: 'from'=1;
		!(local_defined: 'to') 			? local: 'to'=(self -> 'fields') -> size;
		!(local_defined: 'type')		? local: 'type'=(self -> 'validfieldtypes');
		!(local_defined: 'excludetype')	? local: 'excludetype'=map;
		#type -> type == 'string' 		? #type = (map: #type);
		#excludetype -> type == 'string' ? #excludetype = (map: #excludetype);

		// use field name if #from is a string
		#from -> type == 'string' ? #from = integer: ((self -> 'fields') -> (findindex: #from) -> first);
		#from == 0 ? #from = 1;
		// negative numbers count from the end
		#from < 0 ? #from = (self -> 'fields') -> size + #from;

		// use field name if #to is a string
		#to -> type == 'string' ? #to = integer: ((self -> 'fields') -> (findindex: #to) -> last);
		#to == 0 ? #to = (self -> 'fields') -> size;
		// negative numbers count from the end
		#to < 0 ? #to = (self -> 'fields') -> size + #to;

		//Sanity check
		#from > #to ? #to = #from;

		local: 'template'=( (self -> 'template') != '' 
			? (self -> 'template') 		
			| '#label#: #field#<br' + #endslash + '>\n' );
		local: 'buttontemplate'=( (self -> 'buttontemplate') != '' 
			? (self -> 'buttontemplate') 		
			| (self -> 'template') != '' 
			? (self -> 'template')
			| '#field#\n' );
		local: 'defaultclass'=( (self -> 'class') != '' 
			? (self -> 'class') 		
			| '');
		if: (local_defined: 'legend');
			#output += '<fieldset>\n'
				+ '<legend>' + #legend + '</legend>\n';
			(self -> 'render_fieldset2_open') = true;
		/if;
		iterate: (self -> 'fields'), (local: 'fieldpair');
			#onefield = #fieldpair -> value;
			#fieldvalue = (#onefield -> (find: 'value'));
			#fieldvalue_array = #fieldvalue;
			if: #fieldvalue_array -> type != 'array';
				if: #fieldvalue_array >> '\r'; // Filemaker value list with multiple checked
					#fieldvalue_array = #fieldvalue_array -> (split: '\r');
				else: #fieldvalue_array >> ','; // Other database with multiple checked
					#fieldvalue_array = #fieldvalue_array -> (split: ',');
				else;
					#fieldvalue_array = array: #fieldvalue_array;
				/if;
			/if;
			if: #onefield >> 'options';
				#options = (#onefield -> find: 'options');
				// convert types for pair 
				iterate: #options, (local: 'option');
					if: #option -> type != 'pair';
						#option = (pair: #option = #option);
					/if;
					// name must be string to make sure comparsions work
					(#option -> name) = (string: #option -> name);
				/iterate;
			/if;
			if: loop_count >= #from 
				&& loop_count <= #to
				&& #type >> #onefield -> (find: 'type')
				&& !(#excludetype >> #onefield -> (find: 'type'));

				if: #onefield -> (find: 'template') -> size;
					#renderrow = #onefield -> (find: 'template');
				else: (map: 'submit', 'reset', 'image') >> #onefield -> (find: 'type');
					#renderrow=#buttontemplate;
				else;
					#renderrow=#template;
				/if;

				if: (self -> 'exceptionfieldtypes') >> (#onefield -> (find: 'type'));
					#renderrow -> (replace: '#label#:', '');
					#renderrow -> (replace: '#label#', '');
				else: (#onefield -> (find: 'label')) != '';
					#renderrow -> (replace: '#label#', (#onefield -> (find: 'label') ));
				else: 
					#renderrow -> (replace: '#label#:', '');
					#renderrow -> (replace: '#label#', '');
				/if;
				if: (map: 'radio', 'checkbox', 'select') >> (#onefield -> (find: 'type'));
					#renderfield = string;
					iterate: #fieldvalue_array, local: 'onefieldvalue';
						if: loop_count > 1;
							#renderfield += ( (#onefield -> find: 'linebreak') ? '<br' + #endslash + '>\n' | ', ');
						/if;
						if: #options >> #onefieldvalue;
							// show the display text for a selected option
							#renderfield += (encode_break: (#options -> (find: #onefieldvalue) -> first -> value));
						else;
							// show the option value itself
							#renderfield += (encode_break: #onefieldvalue);
						/if;
					/iterate;
				else: (#onefield -> (find: 'type')) == 'html';
					#renderrow = #template;
					#renderrow -> (replace: '#label#:', '');
					#renderrow -> (replace: '#label#', '');
					#renderrow -> (replace: '#required#', '');
					#renderfield = #fieldvalue + '\n';
				else: (#onefield -> (find: 'type')) == 'legend';
					#renderrow = '';
					if: (self -> 'render_fieldset_open');
						#output += '</fieldset>\n';
						(self -> 'render_fieldset_open') = false;
					/if;
					#output += '<fieldset>\n';
					#output += '<legend>' + (encode_html: #fieldvalue) + '</legend>';
					(self -> 'render_fieldset_open') = true;
				else: (#onefield -> (find: 'type')) == 'fieldset';
					#renderrow = '';
					if: (self -> 'render_fieldset_open');
						#output += '</fieldset>\n';
						(self -> 'render_fieldset_open') = false;
					/if;
					if: #fieldvalue != false;
						#output += '<fieldset>\n<legend></legend>'; // must contain a legend
						(self -> 'render_fieldset_open') = true;
					/if;
				else;
					#renderfield = (encode_break: #fieldvalue);
				/if;
				#renderrow -> (replace: '#field#', #renderfield);
				#output += #renderrow;
			/if;
		/iterate;
		if: (local_defined: 'legend') && (self -> 'render_fieldset2_open');
			// inner fieldset is open
			(self -> 'render_fieldset2_open') = false;
			#output += '</fieldset>\n';
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: #output;
	/define_tag;

	define_tag: 'getvalue', -description='Returns the current value of a form field. Returns an array for repeated form fields. ',
		-required='name', -type='string',
		-optional='index', -type='integer', -copy;
		!local_defined('index') ? local('index' = 1);
		#index < 1 ? #index = 1;
		if: (self -> 'fields') >> #name;
			if(#index > (self -> 'fields') -> find(#name) -> size);
				return;
			/if;
			return: (self -> 'fields') -> (find: #name) -> get(#index) -> value -> (find: 'value');
		/if;
	/define_tag;

	define_tag: 'getlabel', -description='Returns the label for a form field. ',
		-required='name', -type='string';
		if: (self -> 'fields') >> #name;
			return: (self -> 'fields') -> (find: #name) -> first -> value -> (find: 'label');
		/if;
	/define_tag;

	define_tag: 'setvalue', -description='Sets the value for a form field. \
				Either form -> (setvalue: fieldname=newvalue) or form -> (setvalue: -name=fieldname, -value=newvalue)',
		-required='name',
		-optional='value',
		-optional='index', -type='integer', -copy;
		local: 'timer'=knop_timer; 
		// either -> (setvalue: 'fieldname'='newvalue') or -> (setvalue: -name='fieldname', -value='newvalue')
		local: '_name'=#name, '_value'=(local: 'value');
		!local_defined('index') ? local('index' = 1);
		#index < 1 ? #index = 1;
		if: #name -> type == 'pair';
			#_name = #name -> name;
			#_value = #name -> value;
		/if;
		if: (self -> 'fields') >> #_name;
			if(#index > (self -> 'fields') -> find(#_name) -> size);
				return;
			/if;
			// first remove value to break reference
			((self -> 'fields') -> get((self -> 'fields') -> (findindex: #_name) -> get(#index)) -> value) -> (remove: 'value');
			((self -> 'fields') -> get((self -> 'fields') -> (findindex: #_name) -> get(#index)) -> value) -> (insert: 'value'=#_value);
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'removefield', -description='Removes all form elements with the specified name from the form',
		-required='name', -type='string';
		local: 'timer'=knop_timer; 
		(self -> 'fields') -> (removeall: #name);
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	define_tag: 'keys', -description='Returns an array of all field names';
		local: 'timer'=knop_timer; 
		local: 'output'=array;
		iterate: (self -> 'fields'), (local: 'fieldpair');
			#output -> (insert: #fieldpair -> name);
		/iterate;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: #output;
	/define_tag;


	define_tag: 'keyvalue'; 			return: (self -> 'db_keyvalue');		/define_tag;
	define_tag: 'lockvalue'; 			return: (self -> 'db_lockvalue');		/define_tag;
	define_tag: 'lockvalue_decrypted';
		(self -> 'database') -> type != 'database' ? return;
		return: (decrypt_blowfish: (self -> 'db_lockvalue'), -seed=(self -> 'database' -> 'lock_seed'));
	/define_tag;
	define_tag: 'database'; 			return: (self -> 'database');			/define_tag;

	define_tag: 'formmode', -description='Returns add or edit after for -> init has been called';
		local: 'timer'=knop_timer; 
		if: (self -> getbutton) == 'add';
			// this is needed to keep the right form mode after a failed add
			(self -> 'formmode') = 'add';
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
 		return: (self -> 'formmode');
 	/define_tag;

	define_tag: 'error_code';
		// custom error_code for knop_form
		if: (self -> 'error_code');
			return: integer: (self -> 'error_code');
		else: (self -> 'errors') -> type == 'array' && (self -> 'errors') -> size > 0;
			(self -> 'error_code') = 7101;
			return: (self -> 'error_code');
		else;
			return: 0;
		/if;
	/define_tag;


	
	define_tag: 'afterhandler', -description='Internal member tag. Adds needed javascripts through an atend handler that will be processed when the entire page is done. \n\
			Parameters:\n\
			-headscript (optional) A single script, will be placed before </head>  (or at top of page if </head> is missing)\n\
			-endscript (optional) Multiple scripts (no duplicates), will be placed before </body> (or at end of page if </body> is missing)',
		-optional='headscript', -type='string',
		-optional='endscript', -type='string';
		// adds needed javascripts through an atend handler that will be processed when the entire page is done

		if: !(var_defined: 'knop_afterhandler_data');
			var: 'knop_afterhandler_data'=map;
			define_atend: { // this will run after the page is done processing
				if: $knop_afterhandler_data >> 'headscript';
					// put before </head> or at beginning of page
					local: 'scriptdata'='<script language="javascript" type="text/javascript">\n/*<![CDATA[ */\n' 
						+ ($knop_afterhandler_data -> (find: 'headscript') -> join('\n')) 
						+ '\n/* ]]> */\n</script>\n';
					if: content_body >> '</head>';
						content_body -> (replace: '</head>', #scriptdata + '</head>');
					else;
						content_body = #scriptdata + content_body;
					/if;
				/if;
				if: $knop_afterhandler_data >> 'endscript';
					// put before </body> or at end of page
					local: 'scriptdata'='\n\n\n\n<script language="javascript" type="text/javascript">\n/* <![CDATA[ */\n' 
						+ ($knop_afterhandler_data -> (find: 'endscript') -> join('\n')) 
						+ '\n/* ]]> */\n</script>\n';
					if: content_body >> '</body>';
						content_body -> (replace: '</body>', #scriptdata + '</body>');
					else;
						content_body += #scriptdata;
					/if;
				/if;
			};
		/if;
		
		if: (local_defined: 'headscript');
			// add to current headscript
			if: $knop_afterhandler_data !>> 'headscript';
				$knop_afterhandler_data -> (insert: 'headscript' = array);
			/if;
			if: $knop_afterhandler_data -> (find: 'headscript') !>> #headscript;
				$knop_afterhandler_data -> (find: 'headscript') -> insert(#headscript);
			/if;
		/if;
		if: (local_defined: 'endscript');
			// add to current endscript
			if: $knop_afterhandler_data !>> 'endscript';
				$knop_afterhandler_data -> (insert: 'endscript' = array);
			/if;
			if: $knop_afterhandler_data -> (find: 'endscript') !>> #endscript;
				$knop_afterhandler_data -> (find: 'endscript') -> insert(#endscript);
			/if;
		/if;
	/define_tag;
/define_type;


?>
[
//------------------------------------------------------------------
//    End knop_form
//------------------------------------------------------------------

//##################################################################

][
//------------------------------------------------------------------
//    Begin knop_grid
//------------------------------------------------------------------

]<?LassoScript

define_type: 'grid',
	'knop_base',
	-namespace='knop_';
//	-prototype;

	local: 'version'='2011-01-01',
		'description'='Custom type to handle data grids (record listings).';

/*

CHANGE NOTES
2011-01-01	SP	Correction of invalid HTML in <thead> and <tr>
2010-12-23	SP	Corrected pagination bug for -numbered.
2010-11-17	JC	Added -startwithfooter flag to grid->renderhtml.  This moves the footer before the column titles in the table header.
2010-11-17	JC	Changed rawheader inclusion to work even if there's no quicksearch for a grid
2010-05-14	JC	Added span separation on grid footer for better styling
2010-03-06	SP	Changed default behavior of ->sortparams and ->quicksearch with -sql to add backticks between the table and column names.  Now JOINs may be used.
2010-03-06	SP	Added ->sortparams and ->quicksearch with -removedotbackticks for backward compatibility for fields that contain periods.  If you use periods in a fieldname then you cannot use a JOIN in Knop.
2010-01-27	JC	Adjusted the id support
2010-01-25	JC	Added support for optional id, used by table, quicksearch and quicksearch_reset
2009-09-18	JS	Syntax adjustments for Lasso 9
2009-08-26	JS	Corrected prev/next links when no nav is defined for the grid object
2009-06-29	JS	->renderlisting: documentation correction (renderlisting never calls renderfooter). 
2009-01-11	JS	->renderlisting: Made sure that template isn't applied on NULL field values since that would cause an error with ->replace 
2008-12-29	JC	Support for optional classes in table header
2008-11-27	JC	-> implemented support for td specific classes. The class is inserted in both the TH and TD tag for the specified field
2008-09-24	JS	->sortparams: fieldnames specified by the -sort parameter are now validated so they exist in the database
2008-09-24	JS	->sortparams, ->quicksearch: Added protection against backtick sql injection in MySQL object names
2008-09-10	JS	-numbered can now be specified at oncreate in addition to  ->renderhtml and ->renderfooter. 
2008-09-08	JS	->sortfield changed so defaultsort is honored even if -nosort is specified (-nosort is only used to prevent the user from changing sort order on the fly)
2008-09-08	JS	->renderlisting and ->renderfooter optimized by caching the result of nav->url. 
2008-05-15	JS	->renderfooter: minor adjustment to numbered pagination links
2008-05-14	JS	->renderfooter (and ->renderhtml): added -numbered as option to get google style numbered pagination links. Render the grid with -numbered (defaults to show 6 page number links plus the far ends) or -numbered=10 or another number. 
2008-05-13	JS	Added ->renderlisting which is now part of ->renderhtml for more flexibility
2008-02-25	JS	->renderheader and ->renderfooter calls nav -> url with -getargs 
2008-01-22	JS	->renderheader, ->renderfooter, ->renderhtml: addded -autoparams to nav -> url used in links since url was changed to default to no autoparams. 
2008-01-22	JS	->renderheader, ->renderfooter, ->renderhtml: improved support for param based navigation method  in links, cleaned up linking to use nav -> url instead of self -> urlparams when nav is available
2008-01-22	JC	->quicksearch: Changed wordseparators so that \r and \n aren't placed as \r\n, otherwise they are treated as a single character by ->split. 
2007-12-11	JS	Added documentation as -description to most member tags, to be used by the new ->help tag
2007-12-11	JS	Moved ->help to knop_base
2007-12-03	JS	Added optional -language parameter to set the initial language for the grid object
2007-12-03	JS	Added -> lang to provide a reference to the knop_lang object that is used for localized strings
2007-12-03	JS	Added localized strings for English and Swedish
2007-12-03	JS	Added knop_lang to handle localized strings
2007-11-11	JC	Added optional -rawheader for extra header content to be inserted before the Quicksearch form
2007-10-23	JS	->renderheader: added class="grid" to the opening table tag to be able to isolate the css specifications
2007-10-03	JS	->renderfooter: added unique classes for each type of prev/next link to be able to replace with images using css
2007-10-03	JS	->renderfooter: Changed prev/next link texts to simple |< << >> >| instead of unicode glyphs
2007-09-20	JS	Postparams are only sent along for nav params (not "-" params)
2007-09-07	JS	Also send postparams along in prev/next links (not only getparams)
2007-09-06	JS	If nav has params defined, those params will be sent along with prev/next links
2007-09-06	JS	Added encode_url for link params
2007-09-06	JS	Changed -action to -formaction in the quicksearch form
2007-08-08	JS	->urlargs: Added exception for -session
2007-06-18	JS	Added tag timer to most member tags
2007-06-13	JS	added inheritance from knop_base
2007-06-11	JC	added handling of xhtml output
2007-05-30	JS	->quicksearch: added \r and \n as word separators. 
2007-05-03	JS	->addfield: Added check for empty dbfield name before adding to dbfieldmap
2007-04-19	JS	->quicksearch: added  -value (flag) that makes quicksearch output just the value of the quicksearch field instead of a query
2007-04-13	JS	->oncreate: added -nosort as global flag (overrides column specific sort options)
2007-04-13	JS	->renderfooter: Added tbody to footer to make it pass validation, but it's still not semantically correct. 
2007-04-13	JS	Changed field type for quicksearch for non-WebKit based browsers
2007-04-10	JS	->quicksearch: Added @ as word separator for "word begins with" search
2007-04-10	JS	->quicksearch: Added -contains as option to perform a simple contains search instead of emulating "word begins with" search
2007-04-10	JS	->renderheader: When grid has a defaultsort, there should be no "unsort" option in the sortable headings
2007-04-10	JS	->insert: (-defaultsort='desc') or (-defaultsort='descending') makes the default sort order sort in descending order
2007-04-04	JS	->addfield: -template can now also be a compound expression
2007-04-03	JS	Changed namespace from mt_ to knop_
2007-03-01	JS	Added support for FileMaker with quicksearch (untested)
2007-03-01	JS	Changed all texts to English
2007-02-07	JS	Removed classs="first" and class="notopborder" since CSS border-collapse: collapse; eliminates the need
2007-02-05	JS The -keyvalue parameter for url edit links can be given another name by specifying -keyparamname in addfield
2007-01-31	JS	->addfield: #value# can be used in -url, as a way to provide mailto links etc in lists
2007-01-30	JS	Sortable columns now thave three states instead of two: sorted ascending, sorted descending and unsorted. 
2007-01-30	JS	Improvements to quicksearch, to emulate "word begins with"  searches
2007-01-26	JS	Added support for quicksearch field in grid header. Specify -quicksearch at ->oncreate, and tell what fields to search by specifying -quicksearch for those fields at ->addfield. 
				Fields can be search only (no display) by leaving out name and label. All specified fields are searched word for word of thew search string. 
				Use ->quicksearch to get the search parameters (optionally -sql)
2007-01-26	JS	Added ->urlargs to get a querystring with all "-" GET params, except those in optional -except string or array parameter. 
2007-01-22	JS	Adjustments to highlighting and "(redigera)"-link for records with empty values in the linked field
2007-01-19	JS	Added href titles to sort links and prevnext links
2007-01-19	JS	Corrected sortparams when no sortfield is specified
2007-01-19	JS	Addded go to first page, go to last page in footer
2007-01-19	JS	Added member tag page_skiprecords to help correcting out of bounds page numbers
2007-01-19	JS	Added member tag lastpage
2007-01-18	JS	Support for highlighting of affected record after edit or update (class name "highlight")
2007-01-17	JS	Added addfield: -template

TODO
Make it possible for knop_grid to work independently of a knop_database object so other types of listings can bre created. 
Language of quicksearch buttons can't be changed after the grid has been created
tbody is used in renderfooter, which is not semantically correct. can't use tfoot though since the footer is rendered twice. 
Move templates to a member tag to be make it easier to subclass
Change ->addfield to ->insert and make ->addfield deprecated

*/

	// instance variables
	local: 'fields'=array,
		'dbfieldmap'=map,
		'sortfield'=string,
		'defaultsort'=string,
		'page'=integer,
		'sortdescending'=false,
		'database'=null,
		'nav'=null,
		'debug_trace'=array,
		'quicksearch'=string,
		'quicksearch_form',
		'quicksearch_form_reset',
		'rawheader' = string, // added by Jolle 071111
		'class' = string, // Added by Jolle 081229
		'tbl_id' = 'grid', // Added by Jolle 100125
		'qs_id' = 'quicksearch', // Added by Jolle 100125
		'qsr_id' = 'qs_reset', // Added by Jolle 100125
		'quicksearch_fields'=array,
		'footer' = string, // Added by Jolle 101117
		'lang'=(knop_lang: -default='en', -fallback),		// language strings object 
		'error_lang'=(knop_lang: -default='en', -fallback),
		'numbered'=false,
		'nosort';

	#lang -> (addlanguage: -language='en', -strings=(map: 
		'quicksearch_showall' = 'Show all',
		'quicksearch_search' = 'Search',
		'linktext_edit' = '(edit)',
		'linktitle_showunsorted' = 'Show unsorted',
		'linktitle_changesort' = 'Change sort order to',
		'linktitle_ascending' = 'ascending',
		'linktitle_descending' = 'descending',
		'linktitle_sortascby' = 'Sort ascending by',
		'linktitle_gofirst' = 'Go to first page',
		'linktitle_goprev' = 'Go to previous page',
		'footer_shown' = '#1# - #2# of',
		'footer_found'='found',
		'linktext_gotopage'='Go to page', // SP customization
		'linktitle_gonext' = 'Go to next page',
		'linktitle_golast' = 'Go to last page',

		// language neutral strings, only need to be set for the default language
		'linktext_first'='|&lt;',
		'linktext_prev'='&lt;&lt;',
		'linktext_next'='&gt;&gt;',
		'linktext_last'='&gt;|'
			));
			
	#lang -> (addlanguage: -language='sv', -strings=(map: 
		'quicksearch_showall' = 'Visa alla',
		'quicksearch_search' = 'Sök',
		'linktext_edit' = '(redigera)',
		'linktitle_showunsorted' = 'Visa osorterade',
		'linktitle_changesort' = 'Ändra sorteringsordning till',
		'linktitle_ascending' = 'stigande',
		'linktitle_descending' = 'fallande',
		'linktitle_sortascby' = 'Sortera i stigande ordning efter',
		'linktitle_gofirst' = 'Gå till första sidan',
		'linktitle_goprev' = 'Gå till föregående sida',
		'footer_shown' = '#1# - #2# av',
		'footer_found'='hittade',
		'linktext_gotopage'='Gå till sida', // SP cüstømizätiøn
		'linktitle_gonext' = 'Gå till nästa sida',
		'linktitle_golast' = 'Gå till sista sidan'
	));
	
	define_tag: 'oncreate', -description='Parameters:\n\
			-database (required database) Database object that the grid object will interact with\n\
			-nav (optional nav) Navigation object to interact with\n\
			-quicksearch (optional) Label text for the quick search field\n\
			-rawheader (optional) Extra html to be inserted in the grid header\n\
			-class (optional) Extra classes to be inserted in the grid header. The standard class "grid" is always inserted\n\
			-id (optional) Creates a custom id used for table, quicksearch and quicksearch_reset\n\
			-nosort (optional flag) Global setting for the entire grid (overrides column specific sort options)\n\
			-language (optional) Language to use for the grid, defaults to the browser\'s preferred language\n\
			-numbered (optional flag or integer) If specified, pagination links will be shown as page numbers instead of regular prev/next links. Defaults to 6 links, specify another number (minimum 6) if more numbers are wanted. Can be specified in ->renderhtml as well. ',
		-required='database', -type='database',
		-optional='nav', -type='nav',
		-optional='quicksearch',
		-optional='rawheader',
		-optional='class',
		-optional='id',
		-optional='nosort',
		-optional='language',
		-optional='numbered';
		local: 'timer'=knop_timer; 
		
		local: 'lang'=@(self -> 'lang');
		
		if: (local_defined: 'language');
			#lang -> (setlanguage: #language);
		/if;
		
		// the following params are stored as reference, so the values of the params can be altered after adding a field simply by changing the referenced variable. 
		local_defined('database') ? (self -> 'database' = @#database);
		local_defined('nav') ? (self -> 'nav' = @#nav);

		(self -> 'nosort')=local_defined('nosort');

		if(local_defined('numbered'));
			(self -> 'numbered')=((#numbered !== false) ? integer(#numbered) | false);
		/if;
		(local_defined('class') && #class -> type == 'string') ?
			(self -> 'class') = #class;
		
		if(local_defined('id') && #id -> type == 'string');
			(self -> 'tbl_id') = #id + '_grid';
			(self -> 'qs_id') = #id + '_quicksearch';
			(self -> 'qsr_id') = #id + '_qs_reset';
		/if;


		local: 'clientparams'=client_getparams;
		#clientparams -> (merge: client_postparams);
		if: !(self -> 'nosort');
			(self -> 'sortfield') = (#clientparams >> '-sort' ? (#clientparams -> (find: '-sort') -> first -> value) | string);
			(self -> 'sortdescending') = (#clientparams >> '-desc');
		/if;
		(self -> 'page') = (#clientparams >> '-page' ? (integer: (#clientparams -> (find: '-page') -> first -> value)) | 1);
		(self -> 'page') < 1 ? (self -> 'page') = 1;

		if: (local_defined: 'quicksearch');
			if: #quicksearch != '';
				(self -> 'quicksearch') = #quicksearch;
			else;
				(self -> 'quicksearch') = 'Quicksearch';
			/if;
			(self -> 'quicksearch_form') = (knop_form: -name='quicksearch', -id = (self -> 'qs_id'), -formaction='./', -method='get', -template='#field#\n', -noautoparams);
			(self -> 'quicksearch_form_reset') = (knop_form: -name='quicksearch_reset', -id = (self -> 'qsr_id'), -formaction='./', -method='get', -template='#field#\n', -noautoparams);
			local: 'autosavekey'=server_name + response_path;
			if: (self -> 'nav') -> type =='nav' && (self -> 'nav') -> 'navmethod'=='param';
				(self -> 'quicksearch_form') -> (addfield: -type='hidden', -name='-path', -value=((self -> 'nav') -> path));
				(self -> 'quicksearch_form_reset') -> (addfield: -type='hidden', -name='-path', -value=((self -> 'nav') -> path));
				#autosavekey -> (removetrailing: '/');
				#autosavekey += '/' + ((self -> 'nav') -> path);
			/if;
			if: (self -> 'sortfield') != '' && !(self -> 'nosort');
				(self -> 'quicksearch_form') -> (addfield: -type='hidden', -name='-sort', -value=(self -> 'sortfield'));
				(self -> 'quicksearch_form_reset') -> (addfield: -type='hidden', -name='-sort', -value=(self -> 'sortfield'));
				if: (self -> 'sortdescending');
					(self -> 'quicksearch_form') -> (addfield: -type='hidden', -name='-desc');
					(self -> 'quicksearch_form_reset') -> (addfield: -type='hidden', -name='-desc');
				/if;
			/if;
			if: client_type >> 'WebKit';
				// only use<input type=search" for WebKit based browsers like Safari
				(self -> 'quicksearch_form') -> (addfield: -type='search', -name='-q', -hint=(self -> 'quicksearch'), 
					-size=15, -id = (self -> 'qs_id') + '_q', -raw='autosave="' + #autosavekey + '" results="10"');
			else;
				(self -> 'quicksearch_form') -> (addfield: -type='text', -name='-q', -hint=(self -> 'quicksearch'), 
					-size=15, -id = (self -> 'qs_id') + '_q');
			/if;
			(self -> 'quicksearch_form') -> (addfield: -type='submit', -name='s', -value=(#lang -> quicksearch_search));
			if: #clientparams >> '-q';
				(self -> 'quicksearch_form') -> (setvalue: '-q'=(#clientparams -> (find: '-q') -> first -> value));
				(self -> 'quicksearch_form_reset') -> (addfield: -type='submit', -name='a', -value=(#lang -> quicksearch_showall));
			else;
				(self -> 'quicksearch_form_reset') -> (addfield: -type='submit', -name='a', -value=(#lang -> quicksearch_showall), -disabled);
			/if;
		/if;

		/* Added by JC 071111 to handle extra form included in the header */
		if: (local_defined: 'rawheader');
			if: #rawheader -> type == 'string';
				(self -> 'rawheader') = #rawheader;
			else;
				(self -> 'rawheader') = '';
			/if;
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'onassign', -required='value', -description='Internal, needed to restore references when ctype is defined as prototype';
		// recreate references here
		(self->'database') = @(#value -> 'database');
		(self->'nav') = @(#value -> 'nav');
	/define_tag;

	define_tag: 'lang', -description='Returns a reference to the language object';
		return: @(self -> 'lang');
	/define_tag;

	
	define_tag: 'addfield', -description='Adds a column to the record listing. \n\
			Parameters:\n\
			-name (optional) Name of the field. If not specified, the field will be omitted from the grid. \
				Useful to be able to quicksearch in fields not shown in the grid. \
				In that case -dbfield must be specified. \n\
			-label (optional) Column heading\n\
			-dbfield (optional) Corresponding database field name (name is used if dbfield is not specified)\n\
			-width (optional) Pixels (CSS width)\n\
			-url (optional) Columns will be linked with this url as base. Can contain #value# for example to create clickable email links. \n\
			-keyparamname (optional) Param name to use instead of the default -keyvalue for edit links\n\
			-defaultsort (optional flag) This field will be the default sort field\n\
			-nosort (optional flag) The field header should not be clickable for sort\n\
			-template (optional) Either string to format values, compound expression or map containing templates to display individual values in different ways, use -default to display unknown values, use #value# to insert the actual field value in the template. \n\t\
				If a compound expression is specified, the field value is passed as param to the expression and can be accessed as params. \n\t\
				Example expressions: \n\t\
				{return: params} to return just the field value as is\n\t\
				{return: (date: (field: "moddate")) -> (format: "%-d/%-m")} to return a specific field as formatted date\n\
			-quicksearch (optional flag) If specified, the field will be used for search with quicksearch',
		-optional='name',
		-optional='label',
		-optional='dbfield',
		-optional='width',
		-optional='class',
		// -optional='raw',		// TODO: not implemented 
		-optional='url',
		-optional='keyparamname',
		-optional='defaultsort',
		-optional='nosort',
		-optional='template',
		-optional='quicksearch';
		local: 'timer'=knop_timer; 
		
		fail_if: (local_defined: 'template') && #template -> type != 'string' 
			&& #template -> type != 'map'  
			&& #template -> type != 'tag', -1, 'Template must be either string, map or compound expression';
		local: 'field'=map;

		local_defined('name') ? #field -> insert('name' = #name); 
		local_defined('class') ? #field -> insert('class' = #class); 
		local_defined('url') ? #field -> insert('url' = #url); 
		local_defined('keyparamname') ? #field -> insert('keyparamname' = #keyparamname); 
		local_defined('width') ? #field -> insert('width' = #width);
		#field !>> 'keyparamname' ? #field -> (insert: 'keyparamname' = '-keyvalue');
		
		if: (local_defined: 'template');
			#field -> (insert: 'template'=(#template -> type == 'string' ? (map: '-default'=#template) | #template));
		/if;
		if: (local_defined: 'name');
			#field -> (insert: 'label'=( (local_defined: 'label') ? #label | #name ) );
			#field -> (insert: 'dbfield'=( (local_defined: 'dbfield') ? #dbfield | #name ) );
			#field -> (insert: 'nosort'=(local_defined: 'nosort'));
			if(local_defined('defaultsort')
				//&& !(local_defined: 'nosort') 
				&& (self -> 'defaultsort') == ''
				//&& !(self -> 'nosort')
				); 
				(self -> 'defaultsort') = #name;
				if: (self -> 'sortfield') == '';
					(self -> 'sortfield') = (self -> 'defaultsort');
					if: #defaultsort == 'desc' || #defaultsort == 'descending';
						(self -> 'sortdescending') = true;
					/if;
				/if;
			/if;
			(self -> 'dbfieldmap') -> (insert: #name = ((local_defined: 'dbfield') && #dbfield != '' ? #dbfield | #name));
		/if;
		(local_defined: 'quicksearch') ? (self -> 'quicksearch_fields') -> (insert: ((local_defined: 'dbfield') ? #dbfield | (local: 'name')));
		
		if(local_defined('name') || local_defined('label'));
			(self -> 'fields') -> (insert: #field);
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	define_tag: 'sortparams', -description='Returns a Lasso-style pair array with sort parameters to use in the search inline.\n\
		Parameters:\n\
		-sql (optional)\n\
		-removedotbackticks (optional flag) Use with -sql for backward compatibility for fields that contain periods.  If you use periods in a fieldname then you cannot use a JOIN in Knop.',
		-optional='sql',
		-optional='removedotbackticks';
		local: 'timer'=knop_timer; 
		if: local_defined: 'sql';
			fail_if: (self -> 'database') -> 'isfilemaker', 7009, '-sql can not be used with FileMaker';
			(self -> 'sortfield') == '' ? return;
			local('output'=string);
			if: (self -> 'dbfieldmap') >> (self -> 'sortfield');
				#output=' ORDER BY ';
				if(local_defined('removedotbackticks'));
					#output += '`' + knop_stripbackticks((self -> 'dbfieldmap') -> find(self -> 'sortfield')) + '`';
				else;
					#output += '`' + string_replace(knop_stripbackticks((self -> 'dbfieldmap') -> find(self -> 'sortfield')), -find='.', -replace='`.`') + '`';
				/if;
				if: (self -> 'sortdescending');
					#output += ' DESC';
				/if;
			/if;
		else;
			local: 'output'=array;
			(self -> 'sortfield') == '' ? return: @#output;
			if: (self -> 'dbfieldmap') >> (self -> 'sortfield');
				#output -> (insert: -sortfield=(self -> 'dbfieldmap') -> (find: (self -> 'sortfield')) );
				if: (self -> 'sortdescending');
					#output -> (insert: -sortorder='descending');
				/if;
			/if;
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @#output;
	/define_tag;

	define_tag: 'quicksearch', -description='Returns a pair array with fieldname=value to use in a search inline. If you specify several fields in the grid as -quicksearch (visible or not), they will be treated as if they were one single concatenated field. Quicksearch will take each word entered in the search field and search for them in the combined set of quicksearch fields, performing a "word begins with" match (unless you specify -contains when calling -> quicksearch).\n\
			So if you enter dev joh it will find records with firstname=Johan, occupation=Developer.\n\
			If you\'re familiar with how FileMaker performs text searches, this is the way quicksearch tries to behave.\n\
			Parameters:\n\
			-sql (optional flag) Return an SQL string for the search parameters instead.\n\
			-contains (optional flag) Perform a simple contains search instead of emulating "word begins with" search\n\
			-value (optional flag) Output just the search value of the quicksearch field instead of a pair array or SQL string\n\
			-removedotbackticks (optional flag) Use with -sql for backward compatibility for fields that contain periods.  If you use periods in a fieldname then you cannot use a JOIN in Knop.',
		-optional='sql',
		-optional='contains',
		-optional='value',
		-optional='removedotbackticks';
		local: 'timer'=knop_timer; 
		
		local: 'output'=array,
			'output_temp'=array,
			'_sql'=(local_defined: 'sql'), 
			'wordseparators'=',.- ("@\n\r', // \r and \n must not come after each other as \r\n, but \n\r is fine. 
			'fieldvalue', 'onevalue', 'field', 'wordseparator';
		fail_if: #_sql && (self -> 'database') -> 'isfilemaker', 7009, '-sql can not be used with FileMaker';

		#wordseparators = #wordseparators -> (split: '');
		if: (self -> 'quicksearch_form') -> type != 'form';
			if: #_sql; 
				return: string;
			else;
				return: array;
			/if;
		/if;
		if: local_defined: 'value';
			return: string: ((self -> 'quicksearch_form') -> (getvalue: '-q'));
		/if;
		
		#fieldvalue = string: ((self -> 'quicksearch_form') -> (getvalue: '-q'));
		if: #fieldvalue != '';
			if: (self -> 'database') -> 'isfilemaker';
				#output -> (insert: -logicaloperator='or');
				iterate: (self -> 'quicksearch_fields'), #field;
					if: (local_defined: 'contains');
						#output -> (insert:  -op = 'cn');
					/if;
					#output -> (insert:  #field = #fieldvalue);
				/iterate;
			else;
				// search each word separately
				#fieldvalue = #fieldvalue -> (split: ' ');
				iterate: #fieldvalue, #onevalue;
					#output_temp = array;
					iterate: (self -> 'quicksearch_fields'), #field;
						if: #_sql;
							if: (local_defined: 'contains');
								if(local_defined('removedotbackticks'));
									#output_temp -> insert('`' + knop_stripbackticks(encode_sql(#field)) + '`' 
										+ ' LIKE "%' + encode_sql(#onevalue ) + '%"');
								else;
									#output_temp -> insert('`' + string_replace(knop_stripbackticks(encode_sql(#field)), -find='.', -replace='`.`') + '`' 
										+ ' LIKE "%' + encode_sql(#onevalue ) + '%"');
								/if;
							else;
								if(local_defined('removedotbackticks'));
									#output_temp -> insert('`' + knop_stripbackticks(encode_sql(#field)) + '`' 
									+ ' LIKE "' + encode_sql(#onevalue ) + '%"');
								else;
									#output_temp -> insert('`' + string_replace(knop_stripbackticks(encode_sql(#field)), -find='.', -replace='`.`') + '`' 
									+ ' LIKE "' + encode_sql(#onevalue ) + '%"');
								/if;
								// basic emulation of "word begins with"
								iterate: #wordseparators, #wordseparator;
									if(local_defined('removedotbackticks'));
										#output_temp -> insert('`' + knop_stripbackticks(encode_sql(#field)) + '`' 
											+ ' LIKE "%' + encode_sql(#wordseparator + #onevalue ) + '%"');
									else;
										#output_temp -> insert('`' + string_replace(knop_stripbackticks(encode_sql(#field)), -find='.', -replace='`.`') + '`' 
											+ ' LIKE "%' + encode_sql(#wordseparator + #onevalue ) + '%"');
									/if;
								/iterate;
							/if;
						else;
							if: (local_defined: 'contains');
								#output_temp -> (insert:  -op='cn');
								#output_temp -> (insert:  #field = #onevalue );
							else;
								#output_temp -> (insert:  -op='bw');
								#output_temp -> (insert:  #field = #onevalue );
								if: !(self -> 'database' -> 'isfilemaker');
									// this variant is not needed for FileMaker since it already searches with "word begins with" as default							#output_temp -> (insert:  -op='cn');
									iterate: #wordseparators, #wordseparator;
										#output_temp -> (insert:  -op='cn');
										#output_temp -> (insert:  #field = #wordseparator + #onevalue );
									/iterate;
								/if;
							/if;
						/if;
					/iterate;
					if: #_sql;
						if: #output_temp -> size > 1;
							#output_temp = '(' + #output_temp -> (join: ' OR ') + ')';
						else;
							#output_temp = #output_temp -> first;
						/if;
						#output -> (insert: #output_temp);
					else;
						if: #output_temp -> size > 2;
							#output_temp -> (insert: -opbegin='or', 1);
							#output_temp -> (insert: -opend='or');
						/if;
						#output -> (merge: #output_temp);
					/if;
				/iterate;
				
				if: #_sql;
					if: #output -> size;
						#output = '(' + #output -> (join: ' AND ') + ')';
					else;
						#output = string;
					/if;
				else;
					if: #output -> size;
						#output -> (insert: -opbegin='and', 1);
						#output -> (insert: -opend='and');
					/if;
				/if;
			/if; // isfilemaker
		/if; // #fieldvalue != ''
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @#output;
	/define_tag;
	
	define_tag: 'urlargs', -description='returns all get params that begin with - as a query string, for internal use in links in the grid. \n\
			Parameters:\n\
			-except (optional) Exclude these parameters (string or array)\n\
			-prefix (optional) For example ? or &amp; to include at the beginning of the querystring \n\
			-suffix (optional) For example &amp; to include at the end of the querystring',
		-optional='except', -copy,
		-optional='prefix',	// for example ? or &amp; to include at the beginning of the querystring 
		-optional='suffix';	// for example &amp; to include at the end of the querystring
		local: 'timer'=knop_timer; 
		local: 'output'=array, 'param'=null;

		// only getparams to not send along -action etc
		local: 'clientparams'=client_getparams;

		!(local_defined: 'except') ? local: 'except'=array;
		#except -> type != 'array' ? #except = array: #except;
		#except -> (insert: -session);
		
		// add getparams that begin with -
		iterate: #clientparams, #param;
			if: #param -> type == 'pair';
				if: #param -> name -> (beginswith: '-') && #except !>> #param -> name;
					#output -> (insert: (encode_stricturl: #param -> name) + '=' + (encode_stricturl: #param -> value));
				/if;
			else; // just a string param (no pair)
				if: #param -> (beginswith: '-') && #except !>> #param;
					#output -> (insert: encode_stricturl: #param);
				/if;
			/if;
		/iterate;

		if: self -> 'nav' -> isa('nav');
			// send params that have been defined as -params in nav
			local: 'navitem'=self -> 'nav' -> getnav;
			// add post params
			#clientparams -> (merge: client_postparams);

			iterate: #navitem -> (find: 'params'), #param;
				if: #clientparams >> #param && #clientparams -> (find: #param) -> first -> type == 'pair';
					#output -> (insert: (encode_stricturl: #clientparams -> (find: #param) -> first -> name) +  '=' + (encode_stricturl: #clientparams -> (find: #param) -> first -> value));
				else: #clientparams >> #param;
					#output -> (insert: encode_stricturl: #clientparams -> (find: #param) -> first);
				/if;
			/iterate;
		/if;
		#output = string: (#output -> (join: '&amp;'));
		// restore / in paths
		#output -> replace('%2F', '/');


		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		if: #output -> size;
			return: (local: 'prefix') + #output + (local: 'suffix');
		/if;

	/define_tag;
	
	
	define_tag: 'renderhtml', -description='Outputs the complete record listing. Calls renderheader, renderlisting and renderfooter as well. \
			If 10 records or more are shown, renderfooter is added also just below the header.\n\
			Parameters:\n\
			-inlinename (optional) If not specified, inlinename from the connected database object is used\n\
			-numbered (optional flag or integer) If specified, pagination links will be shown as page numbers instead of regular prev/next links. Defaults to 6 links, specify another number (minimum 6) if more numbers are wanted.',
		-optional='inlinename',
		-optional='xhtml',
		-optional='numbered',
		-optional='startwithfooter';
		local: 'timer'=knop_timer; 

		local: 'output'=string,
			'db'=@(self -> 'database'), 
			'_xhtml' = (((local_defined: 'xhtml') && #xhtml != false) ? true | false);
		if(local_defined('numbered'));
			local('numberedpaging'=((#numbered !== false) ? integer(#numbered) | false));
		else;
			local('numberedpaging'=(((self -> 'numbered') !== false) ? integer(self -> 'numbered') | false));
		/if;

		local_defined('startwithfooter') && #startwithfooter != false ? #startwithfooter = true | local('startwithfooter') = false;

		!(local_defined: 'inlinename') ? local: 'inlinename'=string;
		self -> 'footer' = (self -> (renderfooter: -xhtml = #_xhtml, -numbered=#numberedpaging));
		#output += (self -> renderheader(-start, -xhtml = #_xhtml, -startwithfooter = #startwithfooter));
		if: #db -> shown_count >= 10 && !#startwithfooter;
			#output += self -> 'footer';
		/if;
		#output += (self -> (renderlisting: -inlinename=#inlinename, -xhtml=#_xhtml));

		#output += self -> 'footer' + '</table>\n';
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @#output;
	/define_tag;


	define_tag: 'renderlisting', -description='Outputs just the actual record listing. Is called by renderhtml. \
			Parameters:\n\
			-inlinename (optional) If not specified, inlinename from the connected database object is used',
		-optional='inlinename',
		-optional='xhtml';
		local: 'timer'=knop_timer; 

		local: '_inlinename'=string, 
			'output'=string, 
			'fields'=(self -> 'fields'), 
			'field'=string,
			'keyfield'=null,
			'affectedrecord_keyvalue'=null,
			'record_loop_count'=integer,
			'db'=@(self -> 'database'), 
			'nav'=@(self -> 'nav'),
			'dbfieldmap'=@(self -> 'dbfieldmap'),
			'classarray'=array,
			'fieldname'=string,
			'value'=string,
			'keyparamname',
			'url',
			'url_cached_temp',
			'_xhtml' = (((local_defined: 'xhtml') && #xhtml != false) ? true | false),
			'lang'=@(self -> 'lang');
			
		if: (local: 'inlinename')!='';
			#_inlinename = #inlinename;
		else: #db -> type == 'database';
			#_inlinename=#db -> 'inlinename';
			#keyfield=#db -> 'keyfield';
			#affectedrecord_keyvalue = #db -> 'affectedrecord_keyvalue';
		/if;
		#output += '\n<tbody>\n';
		if: #nav -> (isa: 'nav');
			iterate: #fields, #field;
				if: (#field -> (find: 'url')) != '';
					#url = (#field -> (find: 'url'));
					#keyparamname = (#field -> (find: 'keyparamname'));
					#field -> insert('url_cached' = (#nav -> (url: -path=#url, 
						#keyparamname='###keyvalue###',
						-autoparams,
						-except=(array: -path))));
				/if;
			/iterate;
		/if;
		records: -inlinename=#_inlinename;
			#record_loop_count = loop_count;
			#output += '\n<tr>';
			iterate: #fields, #field;
				#fieldname = (#dbfieldmap -> (find: (#field -> (find: 'name'))));
				#keyparamname = (#field -> (find: 'keyparamname'));
				#value=(field: #fieldname);
				if: #field -> (find: 'template') -> type == 'map';
					#value=string(#value);
					if: #field -> (find: 'template') >> #value;
						#value = #field -> (find: 'template') -> (find: #value);
					else: #field -> (find: 'template') >> '-default';
						#value = #field -> (find: 'template') -> (find: '-default');
					else;
						// show fieldvalue as is
					/if;
					// substitute field value in the display template
					#value -> (replace: '#value#', (field: #fieldname));
				else: #field -> (find: 'template') -> (isa: 'tag');
					#value = #field -> (find: 'template') -> (run: -params=#value);
				/if;
				#classarray=array;
				if:  #affectedrecord_keyvalue == (field: #keyfield) && (field: #keyfield) != '';
					// hightlight affected row
					#classarray -> (insert: 'highlight');
				else;
					(#record_loop_count - 1)  % 2 == 0 ? #classarray -> (insert: 'even');
				/if;
				// Added by JC 081127 to handle td specific classes
				(#field -> find('class') -> size ? #classarray -> insert( #field -> find('class')));
				#output += '<td';
				if: #classarray -> size;
					#output += ' class="' + #classarray -> (join: ' ') + '"';
				/if;
				#output += '>';
				if: (#field -> (find: 'url')) != '';
					#url = (#field -> (find: 'url'));
					/*
					if: #nav -> (isa: 'nav') && #url !>> '#value#';
						#output += '<a href="' + (#nav -> (url: -path=#url, 
							#keyparamname=(field: #keyfield),
							-autoparams,
							-except=(array: -path)));
					*/
					if: #field -> find('url_cached') -> size && #url !>> '#value#';
						#url_cached_temp = #field -> find('url_cached');
						#url_cached_temp -> replace('###keyvalue###', field(#keyfield));
						#output += '<a href="' + #url_cached_temp;
						#output += '">' +  #value 
							// show something to click on even if the field is empty
							+ ((string_findregexp: #value, -find='\\w*') -> size == 0 ? (#lang -> linktext_edit)) 
							+ '</a>';
					else;
						#url -> (replace: '#value#', (field: #fieldname));
						#output += '<a href="' + #url + '"'; 
						#url -> (beginswith: 'http://') || #url -> (beginswith: 'https://') || #url -> (beginswith: 'mailto:')
							? #output += ' class="ext"';
						#output += '>' +  #value + '</a>';
					/if;
				else;
					#output += #value;
				/if;
				#output += '</td>\n';
			/iterate;
			#output += '</tr>\n';
		/records;
		#output += '\n</tbody>\n';
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @#output;
	/define_tag;
	
	
	define_tag: 'renderheader', -description='Outputs the header of the grid with the column headings. \
			Automatically included by ->renderhtml. \n\
			Parameters:\n\
			-start (optional flag) Also output opening <table> tag',
		-optional='start',
		-optional='xhtml',
		-optional = 'startwithfooter';
		local: 'timer'=knop_timer; 
		local: 'output'=string, 
			'db'=@(self -> 'database'), 
			'nav'=@(self -> 'nav'),
			'fields'=@(self -> 'fields'), 
			'field'=string,
			'classarray'=array,
			'_xhtml' = ((local_defined: 'xhtml') && #xhtml != false) ? true | false,
			'lang'=@(self -> 'lang');
		local_defined('startwithfooter') && #startwithfooter != false ? #startwithfooter = true | local('startwithfooter') = false;


		(local_defined: 'start') ? #output += '<table id="' + (self -> 'tbl_id') + '" class="grid' + (self -> 'class' -> size > 0 ? ' ' + self -> 'class') + '">';
		#output += '<thead>\n<tr>';
		if: (self -> 'quicksearch_form') -> type == 'form';
			#output += '<th colspan="' + (#fields -> size) + '" class="quicksearch';
				((self -> 'quicksearch_form') -> (getvalue: '-q') != '') ? #output += ' highlight';
			#output += '">';

			if: (self -> 'rawheader') -> size > 0 ;
				#output += (self -> 'rawheader');
			/if;


			#output += (self -> 'quicksearch_form') -> (renderform: -xhtml = #_xhtml);
			if: (self -> 'quicksearch_form_reset') -> type =='form';
				#output += (self -> 'quicksearch_form_reset') -> (renderform: -xhtml = #_xhtml);
			/if;
			#output += '</th></tr>\n';
		else((self -> 'rawheader') -> size > 0);
			#output += '<th colspan="' + (#fields -> size) + '">';
			#output += (self -> 'rawheader');
			#output += '</th></tr>\n';
		/if;

		if(#startwithfooter);
			#output += self -> 'footer';
		/if;

		#output += '<tr>';

		iterate: #fields, #field;
			#classarray=array;
			//(self -> 'quicksearch_form') -> type == 'form' ? #classarray -> (insert: 'notopborder');
			if: !(self -> 'nosort');
				(self -> 'sortfield') == (#field -> (find: 'name')) 
					&& !(#field -> (find: 'nosort')) ? #classarray -> (insert: 'sort');
			/if;
			#output += '<th';
			if: #field -> (find: 'width') > 0;
				#output += ' style="width: ' + (integer: #field -> (find: 'width')) + 'px;"';
			/if;
			// Added by Jolle 081127 to handle td specific classes
			(#field -> find('class') -> size ? #classarray -> insert( #field -> find('class')));
			if: #classarray -> size;
				#output += ' class="' + #classarray -> (join: ' ') + '"';
			/if;
			#output += '>';
			if: (#field -> (find: 'nosort')) || (self -> 'nosort');
				#output += '<div>' + (#field -> (find: 'label')) '</div>';
			else;
				if: (#classarray >> 'sort' && (self -> 'sortdescending') && (self -> 'defaultsort') == '');
					// create link to change to unsorted
					if: #nav -> isa('nav');
						#output += '<a href="' + #nav -> url(-autoparams, -getargs, -except=(array: -sort, -desc, -page, -path)) + '"'
							+ ' title="' + (#lang -> linktitle_showunsorted) + '">';
					else;
						#output += '<a href="./'
							+ (self -> (urlargs: -except=(array: -sort, -desc, -page), -prefix='?')) + '"'
							+ ' title="' + (#lang -> linktitle_showunsorted) + '">';
					/if;
				else;
					// create link to toggle sort mode
					if: #nav -> isa('nav');
						#output += '<a href="' + #nav -> url(-autoparams, -getargs, -except=(array: -sort, -desc, -page, -path), 
							-urlargs='-sort=' + (#field -> (find: 'name'))
								+ (#classarray >> 'sort' && !(self -> 'sortdescending') ? '&amp;-desc')) + '"'
							+ ' title="' + (#classarray >> 'sort' 
								?  (#lang -> linktitle_changesort) + ' ' 
									+ ((self -> 'sortdescending') ? (#lang -> linktitle_ascending) | (#lang -> linktitle_descending))
								| (#lang -> linktitle_sortascby) + ' ' + (encode_html: (#field -> (find: 'label'))) ) + '">';
					else;
						#output += '<a href="./?-sort=' + (#field -> (find: 'name')) 
							+ (#classarray >> 'sort' && !(self -> 'sortdescending') ? '&amp;-desc') 
							+ (self -> (urlargs: -except=(array: -sort, -desc, -page), -prefix='&amp;')) + '"'
							+ ' title="' + (#classarray >> 'sort' 
								?  (#lang -> linktitle_changesort) + ' ' 
									+ ((self -> 'sortdescending') ? (#lang -> linktitle_ascending) | (#lang -> linktitle_descending))
								| (#lang -> linktitle_sortascby) + ' ' + (encode_html: (#field -> (find: 'label'))) ) + '">';
					/if;
				/if;
				#output += (#field -> (find: 'label'));
				if: (string_findregexp: (#field -> (find: 'label')), -find='\\S') -> size == 0;
					#output += '&nbsp;'; // to show sort link as block element properly even for empty label
				/if;
				if: #classarray >> 'sort';
					#output += ' <span class="sortmarker"> ' + ((self -> 'sortdescending') ? '&#9660;' | '&#9650;') + '</span>';
				/if;
				#output += '</a>';
			 /if;
			 #output += '</th>\n';
		/iterate;
		#output += '</tr>\n</thead>\n';
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @#output;
	/define_tag;

	define_tag: 'renderfooter', -description='Outputs the footer of the grid with the prev/next links and information about found count. \
			Automatically included by ->renderhtml\n\
			Parameters:\n\
			-end (optional flag) Also output closing </table> tag\n\
			-numbered (optional flag or integer) If specified, pagination links will be shown as page numbers instead of regular prev/next links. Defaults to 6 links, specify another number (minimum 6) if more numbers are wanted.',
		-optional='end',
		-optional='numbered',
		-optional='xhtml';
		local: 'timer'=knop_timer; 
		local: 'output'=string, 
			'db'=@(self -> 'database'), 
			'nav'=@(self -> 'nav'),
			'fields'=@(self -> 'fields'), 
			'field'=string,
			//'numberedpaging'=(((local_defined: 'numbered') && #numbered !== false) ? integer(#numbered) | false),
			'_xhtml' = ((local_defined: 'xhtml') && #xhtml != false) ? true | false,
			'lang'=@(self -> 'lang'),
			'page'=(self -> page),
			'lastpage'=(self -> lastpage),
			'url_cached',
			'url_cached_temp';
		if(local_defined('numbered'));
			local('numberedpaging'=((#numbered !== false) ? integer(#numbered) | false));
		else;
			local('numberedpaging'=(((self -> 'numbered') !== false) ? integer(self -> 'numbered') | false));
		/if;
			
		if(#nav -> isa('nav'));
			#url_cached=#nav -> url(-autoparams, -getargs, -except=(array: -page, -path), 
					-urlargs='-page=###page###');
		/if;
		if: #numberedpaging !== false && #numberedpaging < 6;
			// show 10 page numbers as default
			#numberedpaging = 6;
		/if;
		if: #numberedpaging;
			// make sure we have an even number
			#numberedpaging += #numberedpaging % 2;
		/if;
		
		#output += '\n<tr><th colspan="' + (#fields -> size) + '" class="footer first'  + '">';
		/* not used
		if: #nav -> isa('nav');
			local: 'url'=#nav -> url(-autoparams, -getargs, -except=(array: -page, -path), -urlargs='-page='),
				'url_prefix'=(#nav -> 'navmethod' == 'param' ? '&amp;' | '?');
		else;
			local: 'url'='./' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')),
				'url_prefix'='?';
		/if;
		*/
		// JC 2010-05-14 added span separation for better styling
		if: #numberedpaging;
			local: 'page_from'=1,
				'page_to'=#lastpage;
			if: #lastpage > #numberedpaging;
				#page_from=#page - (#numberedpaging/2 - 1);
				#page_to=#page + (#numberedpaging/2);
				if: #page_from < 1;
					#page_to += (1 - #page_from);
					#page_from = 1;
				/if;
				if: #page_to > #lastpage;
					#page_from = #lastpage - (#numberedpaging - 1);
					#page_to = #lastpage;
				/if;
			/if;
			#output += '<span class="foundcount">' + #db -> found_count + ' ' + (#lang -> footer_found) + '</span> &#8212; <span class="pagination">' + (#lang -> linktext_gotopage) + ': '; // SP customization
			if: #page > 1;
				if: #url_cached -> size;
					#url_cached_temp=#url_cached;
					// 2010-12-23	SP	Corrected pagination bug for -numbered.
					#url_cached_temp -> replace('-page=###page###', '-page=' + 1);
					/*#output += ' <a href="' + #nav -> url(-autoparams, -getargs, -except=(array: -page, -path), 
						-urlargs='-page=' + (#page - 1)) + '" class="prevnext prev"'
						+ ' title="' + (#lang -> linktitle_goprev) + '">' + (#lang -> linktext_prev) + '</a> ';*/
					#output += ' <a href="' + #url_cached_temp + '" class="prevnext first"'
						+ ' title="' + (#lang -> linktitle_gofirst) + '">' + (#lang -> linktext_first) + '</a> ';

					#url_cached_temp=#url_cached;
					#url_cached_temp -> replace('-page=###page###', '-page=' + (#page - 1));
					#output += ' <a href="' + #url_cached_temp + '" class="prevnext prev"'
						+ ' title="' + (#lang -> linktitle_goprev) + '">' + (#lang -> linktext_prev) + '</a> ';
				else;
					#output += ' <a href="./?' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')) 
						+ '-page=1" class="prevnext first"'
						+ ' title="' + (#lang -> linktitle_gofirst) + '">' + (#lang -> linktext_first) + '</a> ';
					#output += ' <a href="./?' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')) 
						+ '-page=' + (#page - 1) + '" class="prevnext prev"'
						+ ' title="' + (#lang -> linktitle_goprev) + '">' + (#lang -> linktext_prev) + '</a> ';
				/if;
			else;
				//#output += ' <span class="prevnext prev dim">' + (#lang -> linktext_prev) + '</span> ';
			/if;
			if: #page_from > 1;
				if: #url_cached -> size;
					#url_cached_temp=#url_cached;
					#url_cached_temp -> replace('-page=###page###', '-page=' + 1);
					/*#output += ' <a href="' + #nav -> url(-autoparams, -getargs, -except=(array: -page, -path),
						-urlargs='-page=1') + '" class="prevnext numbered first">1</a>';*/
					#output += ' <a href="' + #url_cached_temp + '" class="prevnext numbered first">1</a>';
				else;
					#output += ' <a href="./?' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')) 
						+ '-page=1" class="prevnext numbered first">1</a> ';
				/if;
				if: #page_from > 2;
					#output +='...';
				/if;
			/if;
			loop: -from=#page_from, -to=#page_to;
				if: loop_count == #page;
					#output += ' <span class="numbered current">' + loop_count + '</span> ';
				else;
					if: #url_cached -> size;
						#url_cached_temp=#url_cached;
						#url_cached_temp -> replace('-page=###page###', '-page=' + loop_count);
						/*#output += ' <a href="' + #nav -> url(-autoparams, -getargs, -except=(array: -page, -path),
							-urlargs='-page=' + loop_count) + '" class="prevnext numbered">' + loop_count + '</a> ';*/
						#output += ' <a href="' + #url_cached_temp + '" class="prevnext numbered">' + loop_count + '</a> ';
					else;
						#output += ' <a href="./?' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')) 
							+ '-page=' + loop_count + '" class="prevnext numbered">' + loop_count + '</a> ';
					/if;
				/if;
			/loop;
			if: #page_to < #lastpage;
				if: #page_to < (#lastpage - 1);
					#output += '...';
				/if;
				if: #url_cached -> size;
					#url_cached_temp=#url_cached;
					#url_cached_temp -> replace('-page=###page###', '-page=' + #lastpage);
					/*#output += ' <a href="' + #nav -> url(-autoparams, -getargs, -except=(array: -page, -path),
						-urlargs='-page=' + #lastpage) + '" class="prevnext numbered last">' + #lastpage + '</a> ';*/
					#output += ' <a href="' + #url_cached_temp + '" class="prevnext numbered last">' + #lastpage + '</a> ';
				else;
					#output += ' <a href="./?' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')) 
						+ '-page=' + #lastpage + '" class="prevnext numbered last">' + #lastpage + '</a> ';
				/if;
			/if;
	
			if: #page < #lastpage;
				if: #url_cached -> size;
					#url_cached_temp=#url_cached;
					#url_cached_temp -> replace('-page=###page###', '-page=' + (#page + 1));
					/*#output += ' <a href="' + #nav -> url(-autoparams, -getargs, -except=(array: -page, -path), 
						-urlargs='-page=' + (#page + 1)) + '" class="prevnext next"'
						+ ' title="' + (#lang -> linktitle_gonext) + '">' + (#lang -> linktext_next) + '</a> ';*/
					#output += ' <a href="' + #url_cached_temp + '" class="prevnext next"'
						+ ' title="' + (#lang -> linktitle_gonext) + '">' + (#lang -> linktext_next) + '</a> ';

					#url_cached_temp=#url_cached;
					#url_cached_temp -> replace('-page=###page###', '-page=' + #lastpage);
					#output += ' <a href="' + #url_cached_temp + '" class="prevnext last"'
						+ ' title="' + (#lang -> linktitle_golast) + '">' + (#lang -> linktext_last) + '</a> ';
				else;
					#output += ' <a href="./?' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')) 
						+ '-page=' + (#page + 1) + '" class="prevnext next"'
						+ ' title="' + (#lang -> linktitle_gonext) + '">' + (#lang -> linktext_next) + '</a> ';
					#output += ' <a href="./?' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')) 
						+ '-page=' + #lastpage + '" class="prevnext last"'
						+ ' title="' + (#lang -> linktitle_golast) + '">' + (#lang -> linktext_last) + '</a> ';
				/if;
			else;
				//#output += ' <span class="prevnext next dim">' + (#lang -> linktext_next) + '</span> ';
			/if;
		
			#output += '</span> ';
		
		
		else;  // regular prev/next links
		
		
			if: #page > 1;
				if: #url_cached -> size;
					#url_cached_temp=#url_cached;
					#url_cached_temp -> replace('-page=###page###', '-page=' + 1);
					/*#output += ' <a href="' + #nav -> url(-autoparams, -getargs, -except=(array: -page, -path),
						-urlargs='-page=1') + '" class="prevnext first"'
						+ ' title="' + (#lang -> linktitle_gofirst) + '">' + (#lang -> linktext_first) + '</a> ';*/
					#output += ' <a href="' + #url_cached_temp + '" class="prevnext first"'
						+ ' title="' + (#lang -> linktitle_gofirst) + '">' + (#lang -> linktext_first) + '</a> ';
		
					#url_cached_temp=#url_cached;
					#url_cached_temp -> replace('-page=###page###', '-page=' + (#page - 1));
					#output += ' <a href="' + #url_cached_temp + '" class="prevnext prev"'
						+ ' title="' + (#lang -> linktitle_goprev) + '">' + (#lang -> linktext_prev) + '</a> ';
				else;
					#output += ' <a href="./?' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')) 
						+ '-page=1" class="prevnext first"'
						+ ' title="' + (#lang -> linktitle_gofirst) + '">' + (#lang -> linktext_first) + '</a> ';
		
					#output += ' <a href="./?' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')) 
						+ '-page=' + (#page - 1) + '" class="prevnext prev"'
						+ ' title="' + (#lang -> linktitle_goprev) + '">' + (#lang -> linktext_prev) + '</a> ';
				/if;
			else;
				#output += ' <span class="prevnext first dim">' + (#lang -> linktext_first) + '</span> \
							<span class="prevnext prev dim">' + (#lang -> linktext_prev) + '</span> ';
			/if;
			if: #db -> found_count > #db -> shown_count;
				#output += (#lang -> (footer_shown: -replace=(array: (#db -> shown_first), (#db -> shown_last)))) + ' ';
			/if;
			#output += #db -> found_count + ' ' + (#lang -> footer_found);
			if: (#db -> shown_last) < (#db -> found_count);
				if: #url_cached -> size;
					#url_cached_temp=#url_cached;
					#url_cached_temp -> replace('-page=###page###', '-page=' + (#page + 1));
					/*#output += ' <a href="' + #nav -> url(-autoparams, -getargs, -except=(array: -page, -path), 
						-urlargs='-page=' + (#page + 1)) + '" class="prevnext next"'
						+ ' title="' + (#lang -> linktitle_gonext) + '">' + (#lang -> linktext_next) + '</a> ';*/
					#output += ' <a href="' + #url_cached_temp + '" class="prevnext next"'
						+ ' title="' + (#lang -> linktitle_gonext) + '">' + (#lang -> linktext_next) + '</a> ';

					#url_cached_temp=#url_cached;
					#url_cached_temp -> replace('-page=###page###', '-page=' + #lastpage);
					#output += ' <a href="' + #url_cached_temp + '" class="prevnext last"'
						+ ' title="' + (#lang -> linktitle_golast) + '">' + (#lang -> linktext_last) + '</a> ';
				else;
					#output += ' <a href="./?' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')) 
						+ '-page=' + (#page + 1) + '" class="prevnext next"'
						+ ' title="' + (#lang -> linktitle_gonext) + '">' + (#lang -> linktext_next) + '</a> ';
					#output += ' <a href="./?' + (self -> (urlargs: -except=(array: -page, -path), -suffix='&amp;')) 
						+ '-page=' + ((self -> lastpage)) + '" class="prevnext last"'
						+ ' title="' + (#lang -> linktitle_golast) + '">' + (#lang -> linktext_last) + '</a> ';
				/if;
			else;
				#output += ' <span class="prevnext next dim">' + (#lang -> linktext_next) + '</span>  \
							<span class="prevnext last dim">' + (#lang -> linktext_last) + '</span> ';
			/if;
		/if;
		#output += '</th></tr>\n';
		(local_defined: 'end') ? #output += '</table>\n';
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @#output;
	/define_tag;
	
	define_tag: 'page', -description='Returns the current page number';
		return: (self -> 'page');
	/define_tag;
	
	define_tag: 'lastpage', -description='Returns the number of the last page for the found records';
		if: (self -> 'database') -> 'found_count' > 0;
			return: (((self -> 'database') -> 'found_count' - 1) / (self -> 'database') -> 'maxrecords_value') + 1;
		else;
			return: 1;
		/if;
	/define_tag;
	
	define_tag: 'page_skiprecords', -description='Converts curent page value to a skiprecords value to use in a search. \n\
			Parameters:\n\
			-maxrecords (required integer) Needed to be able to do the calculation. Maxrecords_value can not be taken from the database object since taht value is not available until aftetr performing the search',
		-required='maxrecords', -type='integer'; // TODO: maxrecords_value can be taken from the database object so should not be required
		return: ((self -> 'page') - 1) * #maxrecords;
	/define_tag;
	

/define_type;

?>[
//------------------------------------------------------------------
//    End knop_grid
//------------------------------------------------------------------

//##################################################################

][
//------------------------------------------------------------------
//    Begin knop_lang
//------------------------------------------------------------------

]<?LassoScript

define_type: 'lang',
	'knop_base',
	-namespace='knop_';
//	-prototype;

	local: 'version'='2010-12-29',
			'description'='A knop_lang object holds all language strings for all supported languages. Strings are \
				stored under a unique text key, but the same key is of course used for the different language \
				versions of the same string. \n\
				Strings can be separated into different variables if it helps managing them for different contexts. \n\
				When the language of a string object is set, that language is used for all subsequent requests \
				for strings until another language is set. All other instances on the same page that don\'t have a \
				language set will also use the same language.  \n\
				If no language is set, knop_lang uses the browser\'s preferred language if it\'s available in \
				the knop_lang object, otherwise it defaults to the first language (unless a default language \
				has been set for the instance).';

/*

CHANGE NOTES
2010-12-29	JS	->getstring: make sure we only try to do replacement in strings or bytes
2010-05-27	JS	getstring now works also with empty -replace value
2010-04-20	JS	getstring debug output corrected for missing string. 
2009-09-16	JS	Syntax adjustments for Lasso 9
2009-06-26	JS	->addstring: clarified deprecation warning
2009-04-08	JS	Added -debug flag to oncreate (when creating a lang object) to make undefined language keys appear as *key*.
2009-03-24	JS	->getstring prevents replacements if there is no language string found, to prevent null->replace error
2008-11-12	JS	Added ->insert instead of ->addstring for better consistency with other Lasso data types. ->addstring will remain functional for backwards compatibility. 
2008-09-11	JS	->_unknowntag: added missing support for -language
2008-01-22	JS	->getstring: corrected the fallback behavior when a current language has been set 
2008-01-07	JS	Removed instance variable browserlanguage due to problems with transient storage. The browserlanguage is cached on page level so it's no loss in reality. 
2007-12-12	JS	Added page level storage of currentlanguage, so all knop_lang instances defaults to the same language once one of them have set a language explictly, but only if the other knop_lang instances don't have a language set explicitly. 
2007-12-12	JS	Added page level caching of browser language (stores the value in $_knop_data map)
2007-12-06	JS	Moved -> help to knop_base
2007-12-06	JS	Added ->description to all member tags. 
2007-12-03	JS	Finished first complete version
2007-06-18	JS	Added tag timer to most member tags
2007-06-13	JS	added inheritance from knop_base
2007-06-13	JS	Renamed to knop_lang (formerly knop_strings)
2007-04-04	JS	Created the data type and started coding

TODO:
Provide methods to handle formatting of dates, numbers, currency etc for different languages/locales. 
Weekday names, month names etc. 
Member tag to return the current language
Member tag to set default output encoding unless it follows encode_set. 

Examples
	var: 'lang_messages'=(knop_lang: -default='en');
	$lang_messages -> (insert: -key='welcome', -value='Welcome to the home page', -language='en');
	$lang_messages -> (insert: -key='welcome', -value='Välkommen till hemsidan', -language='sv');
	$lang_messages -> (insert: -key='loggedin', -value='You are logged in as #1# #2#', -language='en');
	$lang_messages -> (insert: -key='loggedin', -value='Du är inloggad som #1# #2#', -language='sv');

	// proper call, defaults to the browser's preferred language
	$lang_messages -> (getstring: 'welcome');
	// shorthand call
	$lang_messages -> welcome;

	// change language
	$lang_messages -> (setlanguage: 'sv');
	$lang_messages -> welcome;

	// proper call with replacements
	$lang_messages -> (getstring: -key='loggedin': -replace=(array: (field: 'firstname'), (field: 'lastname')));

	// shorthand call with replacements
	$lang_messages -> (loggedin: -replace=(array: (field: 'firstname'), (field: 'lastname')));


*/

	// instance variables
	local: 'strings'=map,		// map: language=(map: key=value, key=value), language=...
		'defaultlanguage'=string,
		'fallback'=false,
		'currentlanguage'=null,	// the currently set language
		'keys'=null,			// cached keys array
		'debug'=false;

	define_tag: 'oncreate', -description='Creates a new instance of knop_lang. \n\
			Parameters: \n\
			-default (optional) Default language. \n\
			-fallback (optional) If specified, falls back to default language if key is missing. \n\
			-debug (optional flag) If specified, missing strings will be output using the key surrpunded by asterisks',
		-optional='default',	
		-optional='fallback',
		-optional='debug';
		local: 'timer'=knop_timer; 
		
		if(local_defined('default') && #default -> size);
			(self -> 'defaultlanguage') = #default;
		/if;
		(self -> 'fallback') = ((local_defined: 'fallback') && #fallback != false);
		(self -> 'debug') = ((local_defined: 'debug') && #debug != false);
		
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	define_tag: 'onconvert', -description='Returns raw dump of the instance for debugging';
		return: (self -> dump);
	/define_tag;

	define_tag: '_unknowntag', -description='Returns the language string for the specified text key \
			= shortcut for getstring. \n\Parameters: \n\
			-language (optional)  see getstring: -language.\n\ 
			-replace (optional) see getstring: -replace. ',
		-optional='language',
		-optional='replace';
		!(local_defined: 'replace') ? local: 'replace'=string;
		!(local_defined: 'language') ? local: 'language'=string;
		if: self -> keys >> tag_name;
			return: @(self -> (getstring: -key=Tag_name, -language=#language, -replace=#replace));
		else;
			(self -> 'debug_trace') -> insert('Error: ' + tag_name + ' not found');
			(self -> 'debug') ? return('*' + tag_name + '*');
		/if;
	/define_tag;
	

	define_tag: 'addlanguage', -description='Adds a map with language strings for an entire language. Replaces all existing language strings for that language. \n\
			Parameters: \n\
			-language (required) The language to add. \n\
			-strings (required) Complete map of key=value for the entire language.',
		-required='language',
		-required='strings', -type='map';
		local: 'timer'=knop_timer; 
		(self -> 'keys') = null;
		(self -> 'strings') -> (insert: #language = #strings);

		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;


	define_tag('addstring', -description='Deprecated synonym for ->insert',
		-required='language',
		-required='key',
		-required='value');
		(self -> 'debug_trace') -> insert('*** DEPRECATION WARNING *** ' + tag_name + ' is deprecated, use ->insert instead');
		self -> insert(-language=#language, -key=#key, -value=#value);
	/define_tag;
	
	define_tag: 'insert',-description='Adds an individual language string. \n\
			Parameters:\n\
			-language (required) The language for the string. \n\
			-key (required) Textkey to store the string under. Replaces any existing key for the same language. \n\
			-value (required) The actual string (can also be compound expression). Can contain replacement tokens #1#, #2# etc. ',
		-required='language',
		-required='key',
		-required='value';
		local: 'timer'=knop_timer; 
		(self -> 'keys') = null;
		if: (self -> 'strings') !>> #language;
			(self -> 'strings') -> (insert: #language = map);
		/if;
		((self -> 'strings') -> (find: #language)) -> (insert: #key = #value);

		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'getstring',	-description='Returns a specific text string in the language that has previously been set for the instance.\
				If no language has been set, the browser\'s preferred language will be used unless another instance on the same page \
				has a language set using ->setlanguage. \n\
				If the string is not available in the chosen language and -fallback was specified, \
				the string for the language that was first specified for that key will be returned. \n\
				Parameters: \n\
				-key (required) textkey to return the string for. \n\
				-language (optional) to return a string for a specified language (temporary override). \n\
				-replace (optional) single value or array of values that will be used as substitutions for placeholders #1#, #2# etc in the returned string, in the order they appear. Replacements can be compund expressions, which will be executed.  Can also be map or pair array, and in that case the left hand element of the map/array will be replaced by the right hand element. ',
		-required='key',
		-optional='language', -copy,
		-optional='replace', -copy;
										
		if: (var: '_knop_data') -> type != 'map';
			// page level caching
			$_knop_data = map;
		/if;

		!(local_defined: 'language') ? local: 'language'=string;

		local: 'timer'=knop_timer; 
		local: 'output'=string;
		if: #language -> size == 0 || !(self -> (validlanguage: #language));
			#language=(self -> 'currentlanguage');
			if: #language -> size == 0;
				if: $_knop_data >> 'currentlanguage' && self -> (validlanguage: ($_knop_data -> (find: 'currentlanguage')));
					// fall back to page level language
					#language = $_knop_data -> find('currentlanguage');
				else;
					// fall back to the browser's preferred language
					#language=(self -> browserlanguage);
				/if;
			/if;
			if: #language -> size == 0 && self -> (validlanguage: (self -> 'defaultlanguage'));
				// still no matching language, fall back to defaultlanguage
				#language = (self -> 'defaultlanguage');
			else: #language -> size == 0;
				// still no matching language, fall back to the first language
				#language = (self -> 'strings') -> keys -> first;
			/if;
			if: (self -> 'strings') !>> #language
				|| ((self -> 'strings') >> #language
					&& (self -> 'strings') -> (find: #language) !>> #key
					&& (self -> 'fallback'));
				// key is not found in current language, switch to default language
				if: self -> (validlanguage: (self -> 'defaultlanguage'));
					// still no matching language, fall back to defaultlanguage
					#language = (self -> 'defaultlanguage');
				else;
					// no default language to fall back to
				/if;
			/if;
		/if;
		if: (self -> 'strings') >> #language;
			if((self -> 'strings') -> find(#language) !>> #key);
				(self -> 'debug_trace') -> insert('Error: ' + #key + ' not found');
				self -> 'tagtime_tagname'=tag_name;
				self -> 'tagtime'=integer(#timer); // cast to integer to trigger onconvert and to "stop timer"
				(self -> 'debug') 
					? return('*' + #key + '*') 
					| return;
			/if;
			#output = (self -> 'strings') -> (find: #language) -> (find: #key);
			//(self -> 'debug_trace') -> (insert: tag_name + ': found string "' + (encode_html: #output) + '" for key "' + #key + '" and language ' + #language);
			
			if: #output -> (isa: 'tag');
				// execute compund expression
				#output = #output -> run;
			/if;
			if((#output -> isa('string') || #output -> isa('bytes')) 
				&& #output -> size && (local_defined: 'replace'));
				// replace placeholders with real values
				if: !(#replace -> (isa: 'array')) && !(#replace -> (isa: 'map'));
					#replace = (array: #replace);
				/if;
				iterate: #replace, local: 'replacement';
					// make sure we have a pair
					if: !(#replacement -> (isa: 'pair'));
						#replacement = (pair: '#' + loop_count + '#' = #replacement);
					/if;
					// if we have a compund expression as replacement, execute the replacement first
					if: (#replacement -> value -> (isa: 'tag'));
						(#replacement -> value) = #replacement -> value -> run;
					/if;
					#output -> (replace: #replacement -> name, #replacement -> value);
				/iterate;
			/if;
		/if;
		
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: #output;
		
	/define_tag;
	
	define_tag: 'setlanguage', -description='Sets the current language for the string object. Also affects other instances on the same page that do not have an explicit language set. ',
		-required='language';
		local: 'timer'=knop_timer; 
		if: (var: '_knop_data') -> type != 'map';
			// page level caching
			$_knop_data = map;
		/if;
		if: self -> (validlanguage: #language);
			(self -> 'debug_trace') -> (insert: tag_name + ': Setting language to ' + #language);
			(self -> 'currentlanguage') = #language;
			// save page level language
			$_knop_data -> insert('currentlanguage' = #language);
		else;
			(self -> 'debug_trace') -> (insert: tag_name + ': Could not set current language to ' + #language + ' since it does not exist in the lang object');
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	define_tag: 'validlanguage', -description='Checks if a specified language exists in the string object, \
		returns true or false.',
		-required='language';
		return: (self -> 'strings') -> keys >> #language;
	/define_tag;
	
	define_tag: 'browserlanguage', -description='Autodetects and returns the most preferred language \
			out of all available languages as specified by the browser\'s accept-language q-value. ';
		
		local: 'timer'=knop_timer; 

		local: 'browserlanguage'=string;

		if: (var: '_knop_data') -> type != 'map';
			// page level caching
			$_knop_data = map;
		/if;

		if: $_knop_data >> 'browserlanguage';
			// use page cache
			#browserlanguage = $_knop_data -> find('browserlanguage');

		else;
			local: 'requestheader'=client_headers -> (split: '\r\n'),
				'acceptlanguage'=string,
				'browserlanguages'=array;
			
			iterate: #requestheader, (local: 'headerfield'); 
				// strip IIS header prefix
				#headerfield -> (removeleading: 'HTTP_');
				
				if: #headerfield -> (beginswith: 'Accept-Language:'); 
					#acceptlanguage=#headerfield;
					loop_abort;
				/if;
			/iterate;
			#acceptlanguage -> (removeleading: 'Accept-Language:');
			#acceptlanguage -> trim;
			(self -> 'debug_trace') -> (insert: tag_name + ': Accept-Language: ' + #acceptlanguage);
			#acceptlanguage = #acceptlanguage -> (split: ',');
			iterate: #acceptlanguage, local: 'language';
				#language = #language -> (split: ';');
				if: #language -> size == 1;
					// no q value specified, use default 1.0
					#language -> (insert: 'q=1.0');
				/if;
				(#language -> first) -> trim; 
				if: #language -> size >= 2 && #language -> first -> size;
					(#language -> second) = (#language -> second) -> (split: '=') -> last;
					(#language -> second) -> trim; 
					#browserlanguages -> (insert: (decimal: (#language -> second)) = (#language -> first) );
				/if;
			/iterate;
			#browserlanguages -> (sort: -ascending=false);
	
			// find the most preferred language
			(self -> 'debug_trace') -> (insert: tag_name + ': looking for matching languages ');
			iterate: #browserlanguages, local: 'language';
				if: (self -> validlanguage: (#language -> second));
					/// found a valid language
					#browserlanguage=#language -> second;
					(self -> 'debug_trace') -> (insert: tag_name + ': found valid language ' + #browserlanguage);
					loop_abort;
				/if;
			/iterate;
			if: #browserlanguage -> size == 0;
				// no matching language found, try again without locale
				(self -> 'debug_trace') -> (insert: tag_name + ': no valid language found, looking again without locale ' + #language);
				iterate: #browserlanguages, local: 'language';
					(#language -> second) = (#language -> second) -> (split: '-') -> first;
					if: (self -> validlanguage: (#language -> second));
						/// found a valid language
						#browserlanguage=#language -> second;
						(self -> 'debug_trace') -> (insert: tag_name + ': found valid language ' + #browserlanguage);
						loop_abort;
					/if;
				/iterate;
			/if;
			$_knop_data -> insert('browserlanguage'=#browserlanguage);
		/if;
		
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: #browserlanguage;

	/define_tag;
	
	define_tag: 'languages', -description='Returns an array of all available languages in the string object \
			(out of the languages in the -language array if specified). \n\
			Parameters: \n\
			-language (optional) string or array of strings.',
		-optional='language', -copy;
		local: 'timer'=knop_timer; 

		local: 'languages'=(self -> 'strings') -> keys;
		if: (local_defined: 'language');
			if: !(#language -> (isa: 'array'));
				#language = array: #language;
			/if;
			#languages -> sort;
			#language -> sort;
			// get the languages that exist in both arrays
			#languages = #languages -> (intersection: #language);
		/if;

		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: #languages;
	/define_tag;
	
	define_tag: 'keys', -description='Returns array of all text keys in the string object. ';
		if: !((self -> 'keys') -> (isa: 'array'));
			local: 'keysarray'=array, 'keysmap'=map, 'keysarray_new'=array;
			// no cached result yet - create list of all keys
			iterate: (self -> 'strings'), local: 'strings_language';
				#keysarray_new = #strings_language -> value -> keys;
				#keysarray_new -> sort;
				#keysarray -> sort;
				// add the keys that are not already in #keysarray by using union
				#keysarray = #keysarray -> (union: #keysarray_new);
			/iterate;
			(self -> 'keys') = #keysarray;
		/if;
		return: (self -> 'keys');
	/define_tag;
	

/define_type;

?>
[
//------------------------------------------------------------------
//    End knop_lang
//------------------------------------------------------------------

//##################################################################

][
//------------------------------------------------------------------
//    Begin knop_nav
//------------------------------------------------------------------

]<?LassoScript

define_type: 'nav',
	'knop_base',
	-namespace='knop_';
//	-prototype;

	local: 'version'='2010-11-17',
 		'description'='Custom type to handle site navigation menu';

/*

CHANGE NOTES
2010-11-17	JC	Fixed bug so that session links no longer gets added to urls by the nav -> url tag.
2010-11-17	JC	Fixed a bug that would not convert local params to an array under certain situations
2009-09-18	JS	Syntax adjustments for Lasso 9
2009-09-04	JS	->linkparams: Multiple paramaters with the same name (typically checkboxes) are now passed properly
2009-05-06	JS	->directorytree considers _include folders as part of the Knop directory structure. nav->include('_include/myfile.inc') will first look for _include/myfile.inc and if not found it will look for _knop/_include/myfile.inc 
2009-05-05	JS	->include looks for a specified file also inside a _knop folder, if the file does not exist at the specified location
2009-02-09	JS	->filename: Casting path and actionpath to string
2008-12-19	JS	->filename (and consequently ->include and ->library) can now use a specific -path instead of the current location's path
2008-12-09	JS	->linkparams: fixed undefined local in trace call (only showed when trace was enabled for the nav object)
2008-11-25	JS	->getlocation will now avoid disabled and hidden pages when looking for the first page if no default page is specified
2008-11-03	JS	->getlocation will not break if no navigation items have been defined
2008-10-30	JS	_mod folders will now work with knop paths with just a single level, so an include file path can end up as _mod_customer/lib_customer.inc or just _mod_customer/lib.inc (the latter variant does not apply to extension-based filenaming, so _mod_customer/.lib will never be a working file path)
2008-07-10	JS	Added -> label to return the name of the current page
2008-05-20	JS	->renderhtml: #current# is not automatically added to the template if the more elaborate template format is used. This makes it easier to hide the currentmarker without changing the nav config (partial revert of fix 2008-01-04). 
2008-05-08	JS	->insert: A duplicate key does no longer cause a fatal error but instead fails silently and logs to the debug log
2008-05-07	JS	->url: will now call ->linkparams so all links that are constructed from ->url will properly send along the parameters specified in the nav item. This also affects pagination and sort links in grid. 
2008-05-07	JS	Added ->linkparams, Returns an array for all parameters that should be sent along with nav links (this was previously embedded in renderhtml)
2008-02-25	JS	->url: new parameter -getargs to add the getargs to the path link if the path equals the current path
2008-02-06	JC	->insert: the -hide flag can now be a boolean (also the -disabled flag)
2008-02-03	JS	->include: can now include any specified filename
2008-02-01	JS	->oncreate: added optional -fileroot to be able to use a root for files that is different from the logical site root used for navigation 
2008-01-23	JS	->url: Added -autoparams that is required to enable the automatic passing of action_params that begin with "-" (this reverts the default behavior to match the the old)
2008-01-22	JS	->url: GET params that begin with "-" are sent as parameters on links. -path, -sort, -desc, -q are explicitly excluded from nav links in renderhtml. 
2008-01-22	JS	When using param based navigation, navigation links now use much cleaner /?path/to/page/ style links instead of /?-path=path/to/page/
2008-01-22	JS	->getlocation: when navmethod is param, the path can now be sent as unnamed parameter insetad of -path parameter (such as /?path/to/page/). 
2008-01-04	JS	->renderbreadcrumb: added flag -plain to output breadcrumb without html
2008-01-04	JS	->renderbreadcrumb: added flag -skipcurrent to not include the current location in the output
2008-01-04	JS	->insert: -template can now be specified also for individual nav items. Use the form #link##label##current##/link##children#. 
2008-01-04	JS	->renderhtml: changed #title# to #label# in template for clarity, for example #link##label##current##/link##children# (#title# will remain supported)
2008-01-04	JS	Added #current# as placeholder for template, to specify where the current marker should occurr. If not specified in the template, the current marker appears immediately after the label.
2007-12-12	JS	->include now logs processing time for the include to debug trace
2007-12-11	JS	Added documentation as -description to most member tags, to be used by the new ->help tag
2007-12-11	JS	Moved ->help to knop_base
2007-12-04	JS	nav item css class is now applied also to disabled nav items (rendered as <span>)
2007-11-08	JS	Changed trace so it tracks some things even when it's not enabled (like include etc)
2007-11-05	JS	Added var name to trace output
2007-10-28	JS	->directorytree: should now work also when knop folders are symlinks
2007-09-06	JS	top level nav elements that are -disabled now behave properly when accessed
2007-09-05	JS	-currentmarker can now be set separately on sublevels, not only on topmost level
2007-08-29	JS	Added _knop as optional base folder to put all knop files in
2007-08-29	JS	->include: Removed file_exists check since the filename has already been verified in ->filename.
2007-08-29	JS	Added ->directorytree which returns a map with all knop filenames, to use when searching for includes
2007-08-28	JS	->oncreate: -filenaming to specify how include files are named prefix/suffix/extension
2007-08-28	JS	->filename: Implemented support for flexible folder structures
2007-08-28	JS	Instance variable #actionconfigfile_didrun was not properly declared
2007-06-18	JS	Added tag timer to most member tags
2007-06-14	JS	->insert: -url='/' can now be used to specify the "home" location. 
2007-06-13	JS	added ->children to get a reference to the children nav object for a specified path, so new children can be inserted. Must call ->reindex afterwards. 
2007-06-13	JS	added ->reindex to rebuild the index maps from scratch. Must be done after adding children items. 
2007-06-13	JS	added ->addchildren to replace a current children nav object for a specified path. Will handle the reindexing transparently. 
2007-06-13	JS	added ->keymap, ->pathmap and ->urlmap to access the index maps so they call reindex if they have been invalidated (for example by ->addchildren)
2007-06-13	JS	added inheritance from knop_base
2007-06-11	JC	added handling of xhtml output
2007-06-08	JS	->insert: params with empty values are now ignored
2007-05-04	JS	->insert: added check that default item exists in children before storing the default
2007-04-19	JS added ->data to retrieve data stored for the current path (or specified path). Optional -type ensures the returned data has the correct type. 
2007-04-19	JS ->insert: added -data to store arbitrary data object for each path. The object is stored as reference so a variable can be changed after it has been added to the nav object
2007-04-19	JS	added ->patharray to return the current path as array
2007-04-17	JS	->renderhtml: template now supports #link##title##/link##children# in addition to #item# to provide more flexibility
2007-04-17	JS	->oncreate: added support for -template, -class, currentclass and -currentmarker
2007-04-13	JS	Implemented -class per navitem (only worked globally with setformat before)
2007-04-03	JS	->renderhtml: added -renderpath, -expand and -flat to be able to render parts of the nav menu for more flexible layout
2007-04-03	JS	->renderbreadcrumb: added -home to show the default navigation item first in the breadcrumb 
2007-04-03	JS	Changed namespace from mt_ to knop_
2007-03-01	JS	Changed navmethod path so it uses response_filepath instead of $url_path
2007-02-25	JS	Added ->actionpath
2007-02-24	JS	->renderhtml: improved handling of classes in nav links
2007-02-22	JS	->url: added -except
2007-02-09	JS	->url: corrected the behavior for plain parameters passed to the tag
2007-02-05	JS	->insert: Added -param to be able to specify params that should be propagated in nav for certain nav elements (like -keyvalue to be able to move between different subtabs for a selected record
2007-02-05	JS	->renderhtml now use ->url to get the right links
2007-02-01	JS	Made usage of trace optional to improve performance
2007-02-01	JS	Improvements to debug_trace to log also recursive events
2007-02-01	JS	Added ->haschildren, which is now used by ->renderhtml to properly show or hide child level
2007-01-31	JS	->renderhtml Added renderhtml_levels to keep track of how many levels deep navigation has been rendered, to be able to add proper spacing between navigation and content
2007-01-31	JS 	->url: -urlargs  Improved handling of urlargs and ?/& delimiters, should work better with navmethod path
2007-01-30	JS	Removed automatic link title attribute since it can be confusing to show children page titles there
2007-01-30	JS	Corrected parameter path to -path (bug)
2007-01-23	JS	->include: 'config' checks if the same config has already run as actionconfig and won't run again in that case
2007-01-23	JS	->include checks if the file exists first, so no need for empty placeholder files
2007-01-23	JS	Added ->include and ->library
2007-01-23	JS	Added ->setlocation
2007-01-17	JS	Added insert: -hide to allow a location without showing it in navigation

TODO
Add support for compound expressions for template. The expression could return a map that would override corresponding param values. 
->insert: Add -raw to be able to inject code into the link tag, similar to form->addfield(-raw). 
Exclude file name for example index.lasso from getargs
Optimize nav->url! Very slow with complex nav object. 
Needs to exclude also -keyfield and -lockfield. Maybe better to add an option to ->url to not auto-add any "-" params at all. 
-params are not sent along in breadcrumb links
Need simple way to exclude certain "-" params from ->url, also in config per nav item
Add support for adding nav structure from a database
Move templates to a member tag to be make it easier to subclass
Make it possible to use external URL for -url (make sure there is no / before http)

*/

	local: 'navitems'=array,	// array of maps
		'keymap'=map,			// to speed up the locating of critical navigation elements (TBD)
		'pathmap'=map,			// contains full key paths also for children navitems
		'urlmap'=map,			// to translate a url to key path, contains url also for children navitems
		'default'=string,		// default path, i.e. home page
		'parentkey'=null,		// the key of the parent to this navitem (null for top item)
		'template'=string,
		'class'=string,
		'currentclass'=string,
		'currentmarker'=null,

		'actionpath'=string,		// captured from -action parameter in submission
		'path'=string,			// captured from path param or urlhandler and translated from url
		'patharray'=array,		// path broken down into elements
		'pathargs'=string,		// extra path parts that can contain record identification etc
		'urlparams'array,		// holds everything needed to generate nav links
		'navmethod'=string,		// path or param depending on how the nav is propagated. To be able to force path, since url handler doesn't kick in for the start page
		'filenaming'=string,
		'directorytreemap'=map,	// contains a list of all existing filenames in the knop directory tree
		'root'='/',				// site root
		'fileroot'='/',			// root for physical files
		'renderhtml_levels'=0,	// number of levels rendered by renderhtml
		// 'tagtime'=integer, 			// moved to knop_base
		// 'tagtime_tagname'=string, 	// moved to knop_base
		'debug_trace'=array,
		'dotrace'=false,
		'actionconfigfile_didrun'=string,	// path to action config file that has been run for the current page load
											// used to not load the same config again
		'error_lang'=(knop_lang: -default='en', -fallback);

	define_tag: 'oncreate', -description='Parameters:\n\
			-default (optional) Key of default navigation item\n\
			-root (optional) The root path for the site section that this nav object is used for\n\
			-fileroot (optional) The root for include files, to be able to use a different root for physical files than the logical root of the site. Defaults to the value of -root. \n\
			-navmethod (optional) path or param. Path for "URL designed" URLs, otherwise a -path parameter will be used for the navigation. \n\
			-filenaming (optional) prefix (default), suffix or extension, specifies how include files are named\n\
			-trace (optional flag) If specified debug_trace will be used. Defaults to disabled for performance reasons. \n\
			-template (optional) html template used to render the navigation menu\n\
			-class (optional) default class for all navigation links\n\
			-currentclass (optional) class added for the currently active link\n\
			-currentmarker (optional) character(s) show to the right link of current nav (typically &raquo;)',
		-optional='template',
		-optional='class',
		-optional='currentclass',
		-optional='currentmarker',
		-optional='default',
		-optional='root',
		-optional='fileroot',
		-optional='navmethod',
		-optional='filenaming',
		-optional='trace';
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace');
		// TODO: check if we are in an inline, in that case use -key, -label etc as field names and loop through records to fill nav		
		
		local_defined('default') ? (self -> 'default') = #default;
		local_defined('root') ? (self -> 'root') = #root;
		local_defined('navmethod') ? (self -> 'navmethod') = #navmethod; 
		local_defined('template') ? (self -> 'template') = #template;
		local_defined('class') ? (self -> 'class') = #class;
		local_defined('currentclass') ? (self -> 'currentclass') = #currentclass;
		local_defined('currentmarker') ? (self -> 'currentmarker') = #currentmarker;
		local_defined('filenaming') ? (self -> 'filenaming') = #filenaming;

		(self -> 'dotrace') = (local_defined: 'trace') && #trace != false;
		// normalize slashes
		(self -> 'root') -> removeleading('/') & removetrailing('/');
		(self -> 'root') = '/' + (self -> 'root') + '/';
		(self -> 'root') -> replace('//', '/');

		if: (local_defined: 'fileroot');
			(self -> 'fileroot') = #fileroot;
			// normalize slashes
			(self -> 'fileroot') -> removeleading('/') & removetrailing('/');
			(self -> 'fileroot') = '/' + (self -> 'fileroot') + '/';
			(self -> 'fileroot') -> replace('//', '/');
		else;
			(self -> 'fileroot') = (self -> 'root');
		/if;
		
		// validate and set default value
		(map: 'prefix', 'suffix', 'extension') !>> (self -> 'filenaming') ? (self -> 'filenaming') = 'prefix'; 
		
		// can not perform getlocation here since the nav structure must be defined first
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	define_tag: 'onconvert', -description='Outputs the navigation object in a very basic form, just to see what it contains';
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		local: 'output' = (self -> 'parentkey') + ': ';
		iterate: (self -> 'navitems'), (local: 'navitem');
			#output += #navitem + '\n';
		/iterate;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @#output;
	/define_tag;

	define_tag: 'insert', -description='Adds a navigation item to nav structure\n\
			Parameters:\n\ 
			-key (required) The key will be part of the path\n\ 
			-label (optional) The menu text\n\ 
			-url (optional) Nav url to use instead of the default url that is derived from the keys path, used as shortcuts for cleaner URLs\n\ 
			-title (optional) Link href title text\n\ 
			-template (optional) Template to use only for this specific nav item, in the form #link##label##current##/link##children#\n\
			-children (optional nav) Sub navitems\n\ 
			-default (optional) Key of default child subnav item\n\ 
			-param (optional) Name of param or params (string/array) that should be sent along with nav links\n\ 
			-class (optional) CSS class name to use for this nav item (defaults to none)\n\ 
			-filename (optional) Name stem for lib, action and content files (defaults to key path with _ instead of /), to be able to use the same processing files for multiple locations\n\ 
			-disabled (optional flag) Makes the menu item dimmed and non-clickable\n\ 
			-hide (optional flag) Makes the location valid without showing a menu item for it\n\ 
			-after (optional) After which navitem to insert this navitem (key or numeric index)\n\ 
			-data (optional) Arbitrary data object (variable, map, array, compound expression) that can be returned for the location by calling ->data',
		-required='key',
		-optional='label',
		-optional='default',
		-optional='url', -copy,
		-optional='title',
		-optional='template',
		-optional='children', -type='nav',
		-optional='param',
		-optional='class',
		-optional='filename',
		-optional='disabled',
		-optional='hide',
		-optional='after',
		-optional='data';
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		
		fail_if: !(local_defined: 'hide') && (local: 'label') == '', -1, 'Insert requires a label';
		#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': -key = ' + #key);
		// fail_if: (self -> keymap) >> #key, -1, #key + ' is not unique';
		if: (self -> keymap) >> #key;
			(self -> 'debug_trace') -> (insert: tag_name + ': *** Error key ' + #key + ' is not unique - skipping');
			return;
		/if;
		local: 'urlmapchildren'=map,
			'pathmapchildren'=map;
		#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': #key=' + #key);
		if: (local_defined: 'children');
			(#children -> 'parentkey') = #key;
			// grab urlmap and pathmap from children
			#urlmapchildren = (#children -> urlmap);
			#pathmapchildren = (#children -> pathmap);
			if: !(local_defined: 'title');
				// set default title to labels of children
				/*
				local: 'title' = array;
				iterate: (#children -> 'navitems'), (local: 'item');
					#title -> (insert: (#item -> (find: 'label')));
				/iterate;
				#title = #title ->  (join: ', ');
				*/
			/if;
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': adding children');
			(self -> 'debug_trace') -> (merge: (#children -> 'debug_trace'));
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': done adding children');
		/if;
		#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': #key=' + #key);
		local: 'navitem'=map;
		local_defined('key') && #key != '' ? #navitem -> insert('key' = #key); 
		local_defined('label') && #label != '' ? #navitem -> insert('label' = #label); 
		local_defined('default') && #default != '' ? #navitem -> insert('default' = #default); 
		local_defined('url') && #url != '' ? #navitem -> insert('url' = #url); 
		local_defined('title') && #title != '' ? #navitem -> insert('title' = #title); 
		local_defined('template') && #template != '' ? #navitem -> insert('template' = #template); 
		local_defined('children') && #children != '' ? #navitem -> insert('children' = #children); 
		local_defined('class') && #class != '' ? #navitem -> insert('class' = #class); 
		local_defined('filename') && #filename != '' ? #navitem -> insert('filename' = #filename);

		if(local_defined('default') && #default != '' && #pathmapchildren >> #default);
			// only add default that exists in children
			#navitem -> insert('default' = #default);
		/if;
		// store these params by reference
		#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': #key=' + #key);
		local_defined('data') && #data != '' ? #navitem -> insert('data' = @#data);

		if: (local_defined: 'param') && #param -> type =='array';
			#navitem -> (insert: 'params'=#param);
		else: (local_defined: 'param') && #param != '';
			#navitem -> (insert: 'params'=(array: #param));
		/if;
		#navitem -> (insert: 'disabled'=(local_defined: 'disabled') && #disabled != false);
		#navitem -> (insert: 'hide'=(local_defined: 'hide') && #hide != false);
		#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': #key=' + #key);
		local: 'index'=(self -> 'navitems') -> size + 1;
		if: (local_defined: 'after') && (string: #after) -> size;
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': -after=' + #after);
			if: #after -> type == 'integer';
				#index = #after;
			else: (self -> keymap) >> #after;
				#index = (self -> keymap) -> (find: #after) + 1;
			/if;
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': index=' + #index);
		/if;
		if: #urlmapchildren -> size;
			// we have urlmap from children - insert it into our urlmap with modified keyvalues
			iterate: #urlmapchildren, (local: 'urlitem');
				fail_if: (self -> urlmap) >> #urlitem -> name, -1, 'url ' + (#urlitem -> name) + ' is not unique';
				(self -> 'urlmap') -> (insert: #urlitem -> name = #key + '/' + (#urlitem -> value));
			/iterate;
		/if;
		if: #pathmapchildren -> size;
			// we have pathmap from children - insert it into our pathmap with modified keys
			iterate: #pathmapchildren, (local: 'pathitem');
				(self -> 'pathmap') -> (insert: #key + '/' + #pathitem -> name);
			/iterate;
		/if;
		#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': #key=' + #key);
		if: (local_defined: 'url') && #url != '';
			#url -> (removeleading: '/') & (removetrailing: '/');
			fail_if: (self -> urlmap) >> #url, -1, 'url ' + #url + ' is not unique';
			(self -> 'urlmap') -> (insert: #url = #key);
		/if;
		(self -> 'pathmap') -> (insert: #key);
		(self -> 'navitems') -> (insert: #navitem, #index);
		// update keymap
		(self -> 'keymap') = map;
		iterate: (self -> 'navitems'), (local: 'item');
			(self -> 'keymap') -> (insert: (#item -> (find: 'key'))  = loop_count);
		/iterate;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"	
	/define_tag;
	
	
	/*
	define_tag: 'remove',	-description='Remove nav object and its children for specified key path. Must update index maps afterwards. ',
		-required='path', -type='string', -copy;
		fail: -1, 'Not implemented'; // This tag is not yet functional
		
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		#path -> removeleading('/') & removetrailing('/');
		local: 'path_remove'=#path -> split('/') -> last;
		#path -> removetrailing(#path_remove) & removetrailing('/');
		// we need to find a reference to the navitems array of maps that contains the nav item to remove
		if: #path -> size;
			local: 'navitem'=@(self -> getnav(#path));
		else;
			local: 'navitem'=@self -> getnav;
		/if;
		// we also need to find a reference to the keymap where we can find the index of the array item to remove
		return: #navitem;
		(#navitem -> getnav) -> remove(#navitem -> keymap -> find(#path_remove));
		
		// invalidate index maps
		(#navitem -> 'keymap') = null;
		(#navitem -> 'pathmap') = null;
		(#navitem -> 'urlmap') = null;

		(self -> 'keymap') = null;
		(self -> 'pathmap') = null;
		(self -> 'urlmap') = null;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	*/
	
	define_tag: 'addchildren',	-description='Add nav object as children to specified key path, replacing the current children if any. \
		Must update index maps afterwards. ',
		-required='path',
		-required='children', -type='nav';
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		local: 'navitem'=@(self -> (getnav: #path));
		#navitem -> (insert: 'children' = #children);

		// invalidate index maps
		(#navitem -> 'keymap') = null;
		(#navitem -> 'pathmap') = null;
		(#navitem -> 'urlmap') = null;

		(self -> 'keymap') = null;
		(self -> 'pathmap') = null;
		(self -> 'urlmap') = null;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	define_tag: 'children', -description='Return reference to the children of the current navigation object map, or for the specified path',
		-optional='path', -copy;  
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		!(local_defined: 'path') ? local: 'path'=(self -> 'patharray');
		if: #path -> type != 'array';
			#path = string: #path;
			#path -> (removeleading: '/') & (removetrailing: '/');
			#path = #path -> (split: '/');
		/if;
		(self -> pathmap) !>> (#path -> (join: '/')) ? return: knop_nav;

		local: 'nav'=@(self -> (getnav: #path));
		if: #nav !>> 'children';
			#nav -> (insert: 'children'=knop_nav);
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @(#nav -> (find: 'children'));
	/define_tag;
	
	define_tag: 'reindex', -description='To recreate keymap, pathmap and urlmap';
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		(self -> 'keymap') = map;
		(self -> 'pathmap') = map;
		(self -> 'urlmap') = map;
		local: 'key'=null, 'pathitem'=null, 'urlitem'=null, 'url'=string;
		iterate: (self -> 'navitems'), (local: 'navitem');
			#key = #navitem -> (find: 'key');
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': key=' + #key);
			(self -> 'keymap') -> (insert: #key = loop_count);
			(self -> 'pathmap') -> (insert: #key);
			if: #navitem -> (find: 'url') != '';
				#url=#navitem -> (find: 'url');
				#url -> (removeleading: '/') & (removetrailing: '/');
				fail_if: (self -> urlmap) >> #url, -1, 'url ' + #url + ' is not unique';
				(self -> 'urlmap') -> (insert: #url = #key);
			/if;
			if: #navitem >> 'children';
				#navitem -> (find: 'children') -> reindex; // recursive
				iterate: (#navitem -> (find: 'children') -> pathmap), #pathitem;
					(self -> 'pathmap') -> (insert: #key + '/' + #pathitem -> name);
				/iterate;
				iterate: (#navitem -> (find: 'children') -> urlmap), #urlitem;
					fail_if: (self -> urlmap) >> #urlitem -> name, -1, 'url ' + (#urlitem -> name) + ' is not unique';
					(self -> 'urlmap') -> (insert: #urlitem -> name = #key + '/' + (#urlitem -> value));
				/iterate;
			/if;
		/iterate;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	define_tag: 'keymap';
		(self -> 'keymap') -> type != 'map' ? self -> reindex;
		return: (self -> 'keymap');
	/define_tag;
	define_tag: 'pathmap';
		(self -> 'pathmap') -> type != 'map' ? self -> reindex;
		return: (self -> 'pathmap');
	/define_tag;
	define_tag: 'urlmap';
		(self -> 'urlmap') -> type != 'map' ? self -> reindex;
		return: (self -> 'urlmap');
	/define_tag;
	
	define_tag: 'getlocation', -description='Grabs path and actionpath from params or urlhandler, translates from url to path if needed. This must be called before using the nav object. \n\
			Parameters:\n\
			-setpath (optional) forces a new path',
		-optional='setpath';
		handle;knop_debug('Done with ' + self->type + ' -> ' + tag_name, -time, -type=self->type);/handle;
		local: 'path'=string, 
			'patharray'=array, 
			'originalpath'=string, 
			'pathargs'=string,
			'actionpath'=string,
			'validurl'=false;
			// TODO: Produce 404 error for invalid urls
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
			
		(self -> 'path') = string;
		(self -> 'patharray') = array;
		(self -> 'pathargs') = string;
		(self -> 'actionpath') = string;
		// get action path
		local: 'clientparams'=client_getparams;
		#clientparams -> (merge: client_postparams);
		#actionpath = (#clientparams >> '-action' ? (#clientparams -> (find: '-action') -> first -> value) | string);
		#actionpath -> (removeleading: '/') & (removetrailing: '/');
		// validate action path
		#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': testing action path ' + #actionpath);
		if: #actionpath -> size && (self -> pathmap) >> #actionpath;
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': found match for action path ' + #actionpath);
			(self -> 'actionpath') = #actionpath;
		/if;

		// get url or path
		if: (local: 'setpath') != '';
			#originalpath = string: #setpath;
		else: (self -> 'navmethod') != 'param'; 
			(self -> 'navmethod') = 'path';
			#originalpath = response_filepath;
			#originalpath -> (removeleading: (self -> 'root'));
		else: (self -> 'navmethod') != 'path';
			(self -> 'navmethod') = 'param';
			if: #clientparams >> '-path';
				// path is sent as -path GET or POST parameter
				#originalpath = (#clientparams >> '-path' ? (#clientparams -> (find: '-path') -> first -> value) | string);
			else: client_getparams -> size && client_getparams -> first -> isa('string');
				// path is sent as first unnamed GET parameter 
				#originalpath = client_getparams -> first;
			/if;
		/if;
		#originalpath -> (removeleading: '/') & (removetrailing: '/');
		#path = #originalpath;
		#patharray = (string: #originalpath) -> (split: '/');
		// look for longest match in urlmap
		local: 'pathfinder'=#patharray;
		loop: #pathfinder -> size;
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': looking at url ' + #pathfinder -> (join: '/'));
			if: (self -> urlmap) >> #pathfinder -> (join: '/');
				// use translated key path
				#path=(self -> urlmap) -> (find: #pathfinder -> (join: '/'));
				#validurl=true;
				#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': found url match for ' (#pathfinder -> (join: '/')) + ' translating to ' + #path);
				loop_abort;
			else;
				// remove last path part and try again
				#pathfinder -> remove;
			/if;
		/loop;
		if: !#validurl;
			// no url found, dig into the nav structure to see if path is valid
			local: 'pathfinder'=#patharray;
			loop: #pathfinder -> size;
				#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': looking at path ' + #pathfinder -> (join: '/'));
				if: (self -> pathmap) >> #pathfinder -> (join: '/');
					// use key path
					#path=#pathfinder -> (join: '/');
					#validurl=true;
					#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': found path match for ' (#pathfinder -> (join: '/')));
					loop_abort;
				else;
					// remove last path part and try again
					#pathfinder -> remove;
				/if;
			/loop;
		/if;
		// look for disabled path
		if: #validurl;
			#path = (string: #path) -> (split: '/');
			while: #path -> size > 1 && (self -> (getnav: #path)) -> (find: 'disabled');
				#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': path '+ #path + ' is disabled');
				#path -> remove;
			/while;
			if: (self -> (getnav: #path)) -> (find: 'disabled');
				#validurl = false;
			/if;
			#path = #path -> (join: '/');
		/if;
		
		if: !#validurl;
			// we haven't found a valid location, we must resort to a default page
			if: self -> 'default' != '' && self -> pathmap >>  self -> 'default';
				#path = self -> 'default';
				#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': use defalt location ' #path);
			else;
				// use first page as default, if it exists
				iterate(self -> 'navitems', local('navitem'));
					if(#navitem >> 'key' 
						&& !(#navitem -> find('disabled')) 
						&& !(#navitem -> find('hide')));
						#path = #navitem -> find('key');
						#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': use first page as default location ' #path);
						loop_abort;
					/if;
				/iterate;
			/if;
			if: (self -> pathmap) >> #path;
				#validurl = true;
				(self -> 'debug_trace') -> (insert: tag_name + ': no navigation seems to be defined ' #path);
			/if;
		/if;
		
		if: #validurl;
			// recursively look for default sub page
			local: 'hasdefault'=true;
			while: #hasdefault;
				local: 'defaultkey' = (self -> (getnav: #path)) -> (find: 'default');
				#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': looking for default key ' + #defaultkey);
				if: !(((self -> (getnav: #path))) -> (find: 'disabled')) 
					&& !(((self -> (getnav: #path + '/' + #defaultkey))) -> (find: 'disabled'))
					&& #defaultkey != '' && (self -> (getnav: #path)) -> (find: 'children') -> keymap >> #defaultkey;
					#path += '/' + #defaultkey;
					#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': new path is ' + #path);
				else;
					#hasdefault=false;
				/if;
			/while;
			
			// look for path arguments = the leftover when we found a matching path
			#pathargs = #originalpath;
			#pathargs -> (removeleading: #pathfinder -> (join: '/')) & (removeleading: '/');
			
			// store values
			(self -> 'path') = #path;
			(self -> 'patharray') = (string: #path) -> (split: '/');
			(self -> 'debug_trace') -> (insert: tag_name + ': path is ' + #path);
			if: #pathargs != '';
				(self -> 'pathargs') = #pathargs;
				(self -> 'debug_trace') -> (insert: tag_name + ': pathargs is ' + #pathargs);
			/if;
		else;
			(self -> 'debug_trace') -> (insert: tag_name + ': no matching path found');
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	define_tag: 'setlocation', -description='Sets the current location to a specific nav path or url',
		-required='path';
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		self -> (getlocation: -setpath=#path);
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	
	define_tag: 'setformat', -description='Sets html template for the nav object, use #items# #item# #/items# or more elaborate #items# #link##label##current##/link##children# #/items# as placeholders.\n\
			Parameters:\n\
			-template (optional string) Html template, defaults to <ul>#items#<li>#item#</li>#/items#</ul>\n\
			-class (optional string) Css class name that will be used for every navigation link\n\
			-currentclass (optional string) Css class name that will be added to the currently active navigation link (defaults to crnt)\n\
			-currentmarker (optional string) String that will be appended to menu text of currently active navigation link',
		-optional='template', -type='string',
		-optional='class', -type='string',
		-optional='currentclass', -type='string',
		-optional='currentmarker', -type='string';
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		
		(local_defined: 'template') ? (self -> 'template') = #template;
		(local_defined: 'class') ? (self -> 'class') = #class;
		(local_defined: 'currentclass') ? (self -> 'currentclass') = #currentclass;
		(local_defined: 'currentmarker') ? (self -> 'currentmarker') = #currentmarker;
		
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'haschildren',	-description='Returns true if nav object has children that are not all -hide.',
		-required='navitem', -type='map';
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		local: 'haschildren'=#navitem >> 'children';
		if: #haschildren;	// verify that there is at least one child that does not have -hide
			#haschildren=false; // assume there is no child to show
			iterate: #navitem -> (find: 'children') -> 'navitems', (local: 'childitem');
				#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': checking ' + (#childitem -> (find: 'key')) + ', hide=' + (#childitem -> (find: 'hide')));
				if: !(#childitem -> (find: 'hide')); // found one
					#haschildren=true;
					loop_abort;
				/if;
			/iterate;
		/if;
		#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': ' + #haschildren);
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: #haschildren;
	/define_tag;

	define_tag: 'renderhtml', -description='Called recursively to render hierarchial nav structure.\n\
			Parameters:\n\
			-renderpath (optional) Only render the children of the specified path (and below)\n\
			-flat (optional flag) Only render one level\n\
			-expand (optional flag) Render the entire expanded nav tree and not just the currently active branch\n\
			-basepath (optional) Internal, needed for recursive calls to get proper paths\n\
			-ancestor (optional nav) Internal, reference to the topmost object, needed for recursive calls\n\
			-xhtml (optional) XHTML valid output',
		-optional='renderpath', -copy,
		-optional='flat',
		-optional='expand',
		-optional='basepath', -copy,
		-optional='ancestor', -type='nav';
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		if: !(local_defined: 'ancestor');
			handle;knop_debug('Done with ' + self->type + ' -> ' + tag_name, -time, -type=self->type);/handle;
			// reset timer when called at top level
			// (self -> 'timer')=0; // undefined instance variable
		/if;
		
		// create reference to the topmost level nav object
		local: 'topself'=(local_defined: 'ancestor') ? @#ancestor | @self ;
		!(local_defined: 'ancestor') ? (#topself -> 'renderhtml_levels') = 1 | (#topself -> 'renderhtml_levels') += 1;
		
		local: '_flat' = (local_defined: 'flat') && #flat != false;
		local: '_expand' = (local_defined: 'expand') && #expand != false;
		
		local: 'template'=(self -> 'template' != '' ? self -> 'template' | #topself -> 'template');
		local: 'clientparams'=client_getparams;
		#clientparams -> (merge: client_postparams);
		
		!(local_defined: 'basepath') ? local: 'basepath'=string;
		if: #template == '';
			#template = '<ul>#items#<li>#item#</li>#/items#</ul>';
		/if;
		local: 'output'=string,
			'itemoutput'=string,
			'itemchildren'=string,
			'itemrow'=string,
			'link'=string,
			'linkparams'=array,
			'thispath'=string,
			'classarray'=array,
			'currentmarker'=string,
			'itemtemplate'=(string_findregexp: #template, -find='(?si)#items#(.*?)#/items#'),
			'itemlinkstart'=string,
			'itemlinkend'=string,
			'itemlabel'=string;
		#itemtemplate = (#itemtemplate -> size >= 2 ? #itemtemplate -> (get: 2) | string);
		
		if: (local_defined: 'renderpath') && #renderpath != '' && #renderpath != '/';
			// render nav for specified path
			#renderpath -> (removeleading: '/') & (removetrailing: '/');
			if: #topself -> (getnav: #renderpath) -> (find: 'children') -> type == 'nav';
				return: @(#topself -> (getnav: #renderpath) -> (find: 'children') -> (renderhtml: -basepath=#renderpath, 
					-ancestor=#topself, -flat=#_flat, -expand=#_expand));
			else;
				return: string;
			/if;
		/if;
		iterate: (self -> 'navitems'), (local: 'navitem');
			if: #navitem -> (find: 'hide');
				// do not show in navigation
			else;
				#itemlinkstart=string;
				#itemlabel=string;
				#itemlinkend=string;
				#itemchildren = string;
				#currentmarker=string;

				#thispath=#basepath + '/' + (#navitem -> (find: 'key'));
				#thispath -> (removeleading: '/');
				#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': thispath is ' + #thispath + ', currentpath is ' + (#topself -> 'path'));
				if: (#navitem -> (find: 'template')) != '';
					#itemrow = (#navitem -> (find: 'template'));
				else;
					#itemrow = #itemtemplate;
				/if;
				#classarray = array;
				(#topself -> 'class') != '' ? #classarray -> (insert: (#topself -> 'class'));
				(#navitem -> (find: 'class')) != '' ? #classarray -> (insert: (#navitem -> (find: 'class')));
				if: #navitem -> (find: 'disabled');
					#itemlinkstart='<span';
					if: #classarray -> size;
						#itemlinkstart += ' class="' + #classarray -> (join: ' ') + '"';
					/if;
					#itemlinkstart += '>';
					#itemlabel=(#navitem -> (find: 'label'));
					#itemlinkend='</span>';
				else;
					/* this code is moved into ->url
					if: (#navitem -> (find: 'params')) -> type == 'array';
						#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': checking params for ' + #thispath + ' with ' + (#navitem -> (find: 'params')));
						// TODO: Move this to a new member tag ->linkparams: path|navitem
						#linkparams=array;
						iterate: #navitem -> (find: 'params'), (local: 'param');
							if: #clientparams >> #param && #clientparams -> (find: #param) -> first -> type == 'pair';
								#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': got pair param');
								#linkparams -> (insert: #clientparams -> (find: #param) -> first -> name = #clientparams -> (find: #param) -> first -> value);
							else: #clientparams >> #param;
								#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': got plain param');
								#linkparams -> (insert: #clientparams -> (find: #param) -> first);
							/if;
						/iterate;
						#link = (self -> (url: -path=#thispath, -params=self -> (linkparams: -navitem=#navitem), -topself=#topself));
					else;*/
						//#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': no params for ' + #thispath);
						#link = (self -> (url: -path=#thispath, -topself=#topself));
					///if;
					
										
					#itemlinkstart = '<a href="' + #link + '"';
					if: (#navitem -> (find: 'title')) != '';
						#itemlinkstart += ' title="' + (encode_html: (#navitem -> (find: 'title'))) + '"';
					/if;
					if: (((#topself -> 'path') + '/') -> (beginswith: #thispath + '/'));
						#classarray -> (insert: ((#topself -> 'currentclass') != '' ? (#topself -> 'currentclass') | 'crnt') );
					/if;
					if: #classarray -> size;
						#itemlinkstart += ' class="' + #classarray -> (join: ' ') + '"';
					/if;
					#itemlinkstart += '>';
					#itemlabel=(#navitem -> (find: 'label'));
					if: #thispath == (#topself -> 'path');
						if: (self -> 'currentmarker') -> type != 'null';
							#currentmarker = (self -> 'currentmarker');
						else: (#topself -> 'currentmarker') -> type != 'null';
							#currentmarker = (#topself -> 'currentmarker');
						/if;
					/if;
					#itemlinkend = '</a>';
					if: ( (((#topself -> 'path') + '/') -> (beginswith: #thispath + '/')) || #_expand)
						&& self -> (haschildren: #navitem)
						&& !#_flat;
						// recursively render child items
						#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': begin rendering children for ' + #basepath + '/' + (#navitem -> (find: 'key')) );
						// clear debug_trace for child
						(#navitem -> (find: 'children') -> 'debug_trace') = array;
						#itemchildren += (#navitem -> (find: 'children') -> (renderhtml: -basepath=#basepath + '/' + (#navitem -> (find: 'key')), 
							-ancestor=#topself,
							-flat=#_flat,
							-expand=#_expand));
						#dotrace ? (self -> 'debug_trace') -> (merge: (#navitem -> (find: 'children') -> 'debug_trace'));
						#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': done rendering children');
					/if;
				/if;
				if: #itemrow >> '#link#';
					#itemrow -> (replace: '#link#', #itemlinkstart) 
						& (replace: '#title#', #itemlabel)	// deprecated, use #label# instead
						& (replace: '#label#', #itemlabel)	// preferred
						& (replace: '#/link#', #itemlinkend)
						& (replace: '#children#', #itemchildren);
				else;
					if: #itemrow !>> '#current#';
						#itemlabel += '#current#';
					/if;
					#itemrow -> (replace: '#item#', #itemlinkstart + #itemlabel + #itemlinkend 
						+ #itemchildren);
				/if;
				#itemrow -> (replace: '#current#', #currentmarker);
				#itemoutput += #itemrow;
			/if;
		/iterate;
		#output = (string_replaceregexp: #template, -find='(?si)#items#(.*?)#/items#', -replace=#itemoutput);
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': render done in ' (self -> 'tagtime') + ' ms');
		return: @#output;

	/define_tag;

	define_tag: 'renderbreadcrumb', -description='Shows the current navigation as breadcrumb trail. \n\
			Parameters:\n\
			-delimiter (optional) Specifies the delimiter to use between nav levels, defaults to " > " if not specified\n\
			-home (optional flag) Show the default navigation item (i.e. "home") first in the breadcrumb (unless already there).',
		-optional='delimiter',
		-optional='home',
		-optional='skipcurrent',
		-optional='plain';
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		local: 'output'=array, 'path'=array;
		!(local_defined: 'delimiter') ? local: 'delimiter'=' &gt; ';
		if: (local_defined: 'home') && #home != false;
			// show the default navigation item first in breadcrumb
			
			// find default path
			if: self -> 'default' != '' && self -> pathmap >>  self -> 'default';
				local: 'homepath'= (self -> 'default');
			else;
				// use first top level nav item as default
				local: 'homepath'= self -> 'navitems' -> first -> (find: 'key');
			/if;
			
			if: !((self -> 'path') -> (beginswith: #homepath));
				if: (local_defined: 'plain') && #plain != false;
					#output -> (insert: (self -> (getnav: #homepath)) -> (find: 'label'));
				else;
					#output -> (insert: '<a href="' + (self -> (url: -path=#homepath)) + '">' + (self -> (getnav: #homepath)) -> (find: 'label') + '</a>');				
				/if;
			/if;
		/if;
		iterate: (self -> 'patharray'), (local: 'pathitem');
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': pathitem ' #pathitem);
			#path -> (insert: #pathitem);
			if: (self -> (getnav: #path)) -> (find: 'hide');
				// do not show in navigation
				loop_abort;
			else;
				if: (local_defined: 'plain') && #plain != false;
					#output -> (insert: (self -> (getnav: #path)) -> (find: 'label'));
				else;
					#output -> (insert: '<a href="' + (self -> (url: -path=#path)) + '">' + (self -> (getnav: #path)) -> (find: 'label') + '</a>');
				/if;
			/if;
		/iterate;
		if: (local_defined: 'skipcurrent') && #skipcurrent != false;
			#output -> removelast;
		/if;
		#output = #output -> (join: #delimiter);
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @#output;
	/define_tag;

	define_tag: 'getnav', -description='Return reference to the current navigation object map, or for the specified path.',
		-optional='path', -copy;  
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		!(local_defined: 'path') ? local: 'path'=(self -> 'patharray');
		if: #path -> type != 'array';
			#path = string: #path;
			#path -> (removeleading: '/') & (removetrailing: '/');
			#path = #path -> (split: '/');
		/if;
		(self -> pathmap) !>> (#path -> (join: '/')) ? return: map;

		local: 'nav'=@self,
			'navmap'=map;
		local: 'pathitem' = #path -> (get: 1);
		if: #nav -> keymap >> #pathitem; 
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': found ' + #pathitem + ' in nav');
			#navmap = @((#nav -> 'navitems') -> (get: ((#nav -> keymap) -> (find: #pathitem))));
			if: #navmap -> type == 'map' && !(#navmap -> (find: 'disabled')) && #navmap -> (find: 'children') -> type == 'nav' && #path -> size > 1;
				// look into children level recursively - remove the path level we are at now
				#path -> (remove: 1);
				return: @(#navmap -> (find: 'children')) -> (getnav: #path); // recursion
			else;
				// we are at the bottom, bail out
				self -> 'tagtime_tagname'=tag_name;
				self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
				return: @#navmap;
			/if;
		/if;
	/define_tag;


	
	define_tag: 'getargs', -description='Path arguments = the leftover when we found a matching path, to be used for keyvalue for example.\n\
			Parameters:\n\
			-index (optional integer) Specifies which leftover path item to return, defaults to all path items as a string',
		-optional='index', -type='integer';
		local: 'dotrace'=(self -> 'dotrace');
		
		local: 'args'=(self -> 'pathargs');
		#args == '' ? return;
		if: local_defined: 'index';
			#args = #args -> (split: '/');
			if: #args -> size >= #index;
				return: #args -> (get: #index);
			else;
				return;
			/if;
		else;
			return: #args;
		/if;
		
	/define_tag;
	
	define_tag: 'label', -description='Returns the name of the current (or specified) nav location\n\
			Parameters:\n\
			-path (optional)',
		-optional='path', -copy;
		local: 'dotrace'=(self -> 'dotrace');
		!(local_defined: 'path') ? local: 'path'=@(self -> 'path');
		return: self -> (getnav: #path) -> (find: 'label');
	/define_tag;

	define_tag: 'path', -description='Returns url or key path for the current or specified location.',
		-optional='path', -copy;	
		local: 'dotrace'=(self -> 'dotrace');
		!(local_defined: 'path') ? local: 'path'=@(self -> 'path');
		if: self -> (getnav: #path) -> (find: 'url') != '';
			local: 'url'=self -> (getnav: #path) -> (find: 'url');
			#url -> (removeleading: '/') & (removetrailing: '/');
			return: #url;
		else: #path -> type == 'array';
			return: @(#path -> (join: '/'));
		else;
			return: @#path;
		/if;
	/define_tag;

	define_tag: 'patharray', -description='Returns current path as array.';
			return: (self -> 'patharray');
	/define_tag;

	define_tag: 'actionpath', -description='Returns action path if any.';
			return: (self -> 'actionpath');
	/define_tag;

	define_tag: 'linkparams', -description='Returns an array for all parameters that should be sent along with nav links',
		-required='navitem', -type='map';
		if: (#navitem -> (find: 'params')) -> type == 'array';
			local: 'linkparams'=array,
				'dotrace'=(self -> 'dotrace'),
				'clientparams'=client_getparams;
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': checking params ' + (#navitem -> (find: 'params')));
			#clientparams -> (merge: client_postparams);
			iterate: #navitem -> (find: 'params'), (local: 'param');
					iterate(#clientparams -> find(#param), local('paraminstance'));
					if: #paraminstance -> type == 'pair';
						#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': got pair param');
						#linkparams -> (insert: (#paraminstance -> name) = (#paraminstance -> value));
					else;
						#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': got plain param');
						#linkparams -> (insert: #paraminstance);
					/if;
				/iterate;
			/iterate;
			return: @#linkparams;
		/if;
	/define_tag;

	define_tag: 'data', -description='Returns data object that can be stored for the current nav location (or specified nav location).\n\
			Parameters:\n\
			-path (optional)\n\
			-type (optional string) Force a certain return type. If the stored object doesn´t match the specified type, an empty instance of the type is returned. That way the data can be filtered by type without having to use conditionals to check the type before. ',
		-optional='path', -copy,
		-optional='type';
		local: 'dotrace'=(self -> 'dotrace');
		!(local_defined: 'path') ? local: 'path'=@(self -> 'path');
		if: (local_defined: 'type');
			if: self -> (getnav: #path) -> (find: 'data') -> type == #type;
				return: self -> (getnav: #path) -> (find: 'data');
			else;
				// return empty instance of the specified type
				return: (\#type)->astype;
			/if;
		else;
			return: self -> (getnav: #path) -> (find: 'data');
		/if;
	/define_tag;

	
	define_tag: 'url', -description='Returns full url for current path or specified path. Path parameters can be provided and overridden by \
			passing them to this tag. \n\
			Parameters:\n\
			-path (optional) \n\
			-params (optional) Pair array to be used in url instead of plain parameters sent to this tag\n\
			-urlargs (optional) Raw string with url parameters to append at end of url and -params\n\
			-getargs (optional flag) Add the getargs (leftover path parts) to the url\n\
			-except (optional) Array of parameter names to exclude (or single parameter name as string)\n\
			-topself (optional nav) Internal, needed to call url from renderhtml when rendering sublevels\n\
			-autoparams (optional flag) Enables the automatic passing of action_params that begin with "-"',
		-optional='path', -copy,
		-optional='params', -copy,
		-optional='urlargs',
		-optional='except', -copy,
		-optional='topself', -type='nav',
		-optional='autoparams';

		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		local: 'url'=string,
			'urlparams'=array;
		// only getparams to not send along -action etc
		local: 'clientparams'=client_getparams, 'param'=null;

		!(local_defined: 'except') ? local: 'except'=array;
		#except -> type != 'array' ? #except = array: #except;
		#except -> insert('-session');
		!(local_defined: 'topself') ? local: 'topself'=@self;
		!(local_defined: 'params') ? local: 'params'=(params -> isa('array') ? params | array);
		!#params -> isa('array') ? #params = array(#params); // added by Jolle 101117
		if: local_defined: 'path';
			if: #params >> '-path';
				// -path was passed as explicit param
				#params -> (removeall: '-path');
			else: #params >> #path;
				// -path was passed as implicit param - shows up in params as plain value (no pair) so remove the value from params
				#params -> (removeall: #path);
			/if;
		else;
			local: 'path'=(#topself -> 'path');
		/if;
		local: 'navitem'=#topself -> getnav: #path;
		if: (#navitem -> (find: 'params')) -> type == 'array';
			// add parameters defined as -param for nav item
			#params -> (merge: (#topself -> (linkparams: -navitem=#navitem)));
		/if;
		if: #params -> isa('array');
			// clean up other parameters passed to he tag
			#params -> (removeall: '-urlargs')
				& (removeall: '-topself')
				& (removeall: '-params')
				& (removeall: '-except')
				& (removeall: '-autoparams')
				& (removeall: '-getargs');
		 /if;

		iterate: #except, (local: 'param');
			#params -> (removeall: #param);
		/iterate;

		#url = (#topself -> (path: #path)) + ((#topself -> (path: #path)) != '' ? '/');
		if: (#topself -> getargs) -> size && (local_defined: 'getargs') && #getargs != false;
			// for links to the current path, add the path args 
			#url += (#topself -> getargs) + '/';
		/if;
		if: #params >> '-keyvalue';
			#url += (#params -> (find: '-keyvalue') -> first -> value) + '/';
			#params -> (removeall: '-keyvalue');
		/if;
		
		iterate: (#params -> type == 'array' ? #params | array: #params), (local: 'param');
			if: #param -> type == 'pair';
				#urlparams -> (insert: (encode_stricturl: #param -> name) + '=' + (encode_stricturl: #param -> value));
			else: #param != '';
				#urlparams -> (insert: (encode_stricturl: #param));
			/if;
		/iterate;
		if: (local_defined: 'autoparams') && #autoparams != false;
			// add getparams that begin with -
			iterate: #clientparams, #param;
				if: #param -> type == 'pair';
					if: #param -> name -> (beginswith: '-') && #except !>> #param -> name;
						#urlparams -> (insert: (encode_stricturl: #param -> name) + '=' + (encode_stricturl: #param -> value));
					/if;
				else; // just a string param (no pair)
					if: #param -> (beginswith: '-') && #except !>> #param;
						#urlparams -> (insert: encode_stricturl: #param);
					/if;
				/if;
			/iterate;
		/if;
		
		if: (#topself -> 'navmethod') == 'param';
			#url = './?' + #url + (#urlparams -> size || (local: 'urlargs') != '' ? '&amp;');
		else;  // path
			#url = (#topself -> 'root') + #url +  (#urlparams -> size || (local: 'urlargs') != '' ? '?');
		/if;

		#urlparams = string: (#urlparams -> (join: '&amp;'));
		// restore / in paths for looks
		#urlparams -> replace('%2f', '/');
		#url += #urlparams;
		#urlparams -> size && (local: 'urlargs') -> size ? #url += '&amp;';
		(local: 'urlargs') -> size ? #url += #urlargs;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: @#url;
	/define_tag;
	
	define_tag: 'filename', -description='Returns the full path to the specified type of precissing file for the current navigation. \n\
			Parameters:\n\
			-type (required) lib, act, cnt, cfg, actcfg',
		-required='type',
		-optional='path', -copy;
		/*
		
		-filenaming can be one of prefix, suffix or extension. 
		Prefix is "the old way". lib_customer.inc.  This is the default if -filenaming is not specified. 
		Suffix is a hybrid, for example customer_lib.inc. 
		Extension is for example customer.lib
		
		The rest is automatic. 
		
		
		Possible places to look for a library file that belongs to the path "customer/edit" (in order of precedence):
		A) -filenaming='prefix' (default)
		1. _mod_customer/lib_customer_edit.inc 		// modular prefixed with module name
		2. _mod_customer/lib_edit.inc				// modular
		3. _mod_customer/_library/lib_customer_edit.inc	// modular separated, prefixed with module name
		4. _mod_customer/_library/lib_edit.inc		// modular separated
		5. _library/lib_customer_edit.inc			// collective ("all modules together") separated. This is the old way. 
		
		B) -filenaming='suffix'
		1. _mod_customer/customer_edit_lib.inc
		2. _mod_customer/edit_lib.inc
		3. _mod_customer/_library/customer_edit_lib.inc
		4. _mod_customer/_library/edit_lib.inc
		5. _library/customer_edit_lib.inc
		
		C) -filenaming='extension'
		1. _mod_customer/customer_edit.lib
		2. _mod_customer/edit.lib
		3. _mod_customer/_library/customer_edit.lib
		4. _mod_customer/_library/edit.lib
		5. _library/customer_edit.lib
		
		The principle is to start looking at the most specific location and then look at more and more generic locations, to be able to do the local override. 

		*/
		
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		local: 'filenamearray'=array, 
			'filenamearray_temp'=array,
			'filename'=string,
			'prefix'=string,
			'type_short'=string,
			'suffix'=string,
			'extension'=string,
			'typefoldermap'=(map: 
				'cfg'='_config/', 
				'actcfg'='_config/', 
				'act'='_action/',
				'lib'='_library/',
				'cnt'='_content/'),
			'typefolder'=string,
			'basefolder'=string,
			'directorytree' = (self -> directorytree);
		if: #type == 'act' || #type == 'actcfg';
			local('path') -> size == 0 ? local('actionpath'=string(self -> 'actionpath')) | local('actionpath'=string(#path));
			#actionpath -> removeleading('/') & removetrailing('/');
			#actionpath == '' ? return;
			#filenamearray = (self -> getnav(#actionpath) -> find('filename'));
			if: #filenamearray  == '';
				#filenamearray=#actionpath;
			/if;
			#filenamearray = #filenamearray -> split('/');
		else;
			local('path') -> size == 0 ? local('path'=string(self -> 'path'));
			#path -> removeleading('/') & removetrailing('/');
			self -> getnav(#path) -> size == 0 ? return;
			#filenamearray=(self -> getnav(#path) -> find('filename'));
			if: #filenamearray == '';
				#filenamearray=#path;
			/if;
			#filenamearray = #filenamearray -> split('/');
		/if;
		#type =='actcfg' ? #prefix = 'cfg' | #prefix = #type;
		#type_short = #prefix;
		#typefolder=#typefoldermap -> (find: #type);

		select: (self -> 'filenaming');
		case: 'suffix';
			#suffix='_' + #prefix;
			#extension='.inc';
			#prefix = '';
		case: 'extension';
			#extension='.' + #prefix;
			#suffix='';
			#prefix = '';
		case; // prefix as default
			#prefix += '_';
			#extension='.inc';
			#suffix='';
		/select;

		local: 'findtimer'=_date_msec;
		loop: 2;
			#basefolder=(array: '', '_knop/') -> (get: loop_count);
			loop: 5;
				#filename = string;
				select: loop_count;
				case: 1;
					// customer/lib_customer_edit.inc
					if: #filenamearray -> size >= 1;
						// at least 1 level, look in module folder
						#filenamearray_temp = #filenamearray;
						#filename = #basefolder + '_mod_' + #filenamearray_temp -> first;
						#filename += '/' + #prefix + (#filenamearray_temp -> (join: '_')) + #suffix + #extension;
					/if;
				case: 2; 
					// customer/lib_edit.inc
					if: #filenamearray -> size >= ((self -> 'filenaming') == 'extension' ? 2 | 1);
						// at least 1 level (2 levels for suffix naming), look in module folder
						#filenamearray_temp = #filenamearray;
						#filename = #basefolder + '_mod_' + #filenamearray_temp -> first;
						#filenamearray_temp -> removefirst;
						#filename += '/' + #prefix + (#filenamearray_temp -> (join: '_')) + #suffix + #extension;
						if(#filenamearray -> size == 1);
							// clean up underscore so filename ends up as lib.inc instead of lib_.inc etc
							#filename -> replace('/' + #type_short + '_' + #extension, '/' + #type_short + #extension);
							#filename -> replace('/_' + #type_short + #extension, '/' + #type_short + #extension);
						/if;
					/if;
				case: 3;
					// customer/_library/lib_customer_edit.inc
					if: #filenamearray -> size >= 2;
						// at least 2 levels, look in module folder
						#filenamearray_temp = #filenamearray;
						#filename = #basefolder + '_mod_' + #filenamearray_temp -> first;
						#filename += '/' + #typefolder + #prefix + (#filenamearray_temp -> (join: '_')) + #suffix + #extension;
					/if;
				case: 4;
					// customer/_library/lib_edit.inc
					if: #filenamearray -> size >= 2;
						// at least 2 levels, look in module folder
						#filenamearray_temp = #filenamearray;
						#filename = #basefolder + '_mod_' + #filenamearray_temp -> first;
						#filenamearray_temp -> removefirst;
						#filename += '/' + #typefolder + #prefix + (#filenamearray_temp -> (join: '_')) + #suffix + #extension;
					/if;
				case;
					// _library/lib_customer_edit.inc
					#filename = #basefolder + #typefolder + #prefix + (#filenamearray -> (join: '_')) 
						+ #suffix + #extension;
				/select;
				if: #filename != '';
					#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': trying ' + (self -> 'fileroot') + #filename );
					if: #directorytree >> #filename;
						//file_exists: (self -> 'fileroot') + #filename;
						// clean up and exit
						#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': ** Found ' + (self -> 'fileroot') + #filename + ' in ' + (_date_msec - #findtimer) ' ms');
						self -> 'tagtime_tagname'=tag_name;
						self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
						return: (self -> 'fileroot') + #filename;
					/if;
				/if;
			/loop;
		/loop;
		// clean exit if nothing was found
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return;

	/define_tag;
	
	define_tag: 'actionconfigfile', -description='Shortcut to filename: actcfg'; return: self -> (filename: 'actcfg'); 	/define_tag;
	define_tag: 'actionfile', -description='Shortcut to filename: act'; 		return: self -> (filename: 'act'); 		/define_tag;
	define_tag: 'configfile', -description='Shortcut to filename: cfg'; 		return: self -> (filename: 'cfg'); 		/define_tag;
	define_tag: 'libraryfile', -description='Shortcut to filename: lib'; 		return: self -> (filename: 'lib'); 		/define_tag;
	define_tag: 'contentfile', -description='Shortcut to filename: cnt'; 		return: self -> (filename: 'cnt'); 		/define_tag;

	define_tag: 'include', -description='Includes any of the files for the current path, fails silently if file does not exist. \n\
			Parameters:\n\
			-file (required) lib, act, cnt, cfg, actcfg or library, action, config, actionconfig, content, or any arbitrary filename',
		-required='file',
		-optional='path';
		knop_debug(self->type + ' -> ' + tag_name + ' ' + params -> first, -type=self->type, -open);
		handle;knop_debug('Done with ' + self->type + ' -> ' + tag_name + ' ' + params -> first, -close, -time, -witherrors);/handle;
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		// includes any of the files for the current path, fails silently if file does not exist
		local: 'translation'=(map:
				'actionconfig'= 'actcfg',
				'action'= 'act',
				'config'= 'cfg',
				'library'= 'lib',
				'content'= 'cnt'),
			'types'=(map: 'actcfg', 'act', 'cfg', 'lib', 'cnt'),
			'result'=string;
		local: 'type'= (#translation >> #file ? #translation -> (find: #file) | #types >> #file ? #file | 'other');
		// find out full filename
		!local_defined('path') ? local('path'=null);
		local: 'filename'=null;
		if: #types >> #type;
			// knop include
			#filename = self -> filename(#type, -path=#path);
		else: (self -> directorytree) >> #file;
			// arbitrary include within the Knop folder structure
			#filename = (self -> 'fileroot') + #file;
		else: (self -> directorytree) >> '_knop/' + #file;
			// arbitrary include one level down in _knop folder
			#filename = (self -> 'fileroot') + '_knop/' + #file;
		/if;
		if: #type == 'cfg' && #filename -> size && (self -> 'actionconfigfile_didrun') == #filename;
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': ' + #filename ' has already run as actionconfig');
			//knop_debug(self->type + ' -> ' + tag_name + ': ' + #filename ' has already run as actionconfig');
			return;
		else: #type == 'actcfg';
			// remember that we have run this config file as actionconfig so we don't run the same file again as page config
			(self -> 'actionconfigfile_didrun') = #filename;
		/if;
		if: #filename != '';
			local: 't'=_date_msec;
			#result=@(include: #filename);
			(self -> 'debug_trace') -> (insert: 'Include ' + #file + ': ' + #filename + ' processed in ' + (_date_msec - #t) ' ms');
			//knop_debug(self->type + ' -> ' + tag_name + ' ' + #file + ': ' + #filename + ' processed in ' + (_date_msec - #t) ' ms', -type=self->type);
			self -> 'tagtime_tagname'=tag_name;
			self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
			return: @#result;
		else;
			#dotrace ? (self -> 'debug_trace') -> (insert: 'Include ' + #file + ': no matching filename found');
			knop_debug(self->type + ' -> ' + tag_name + ' ' + #file + ': no matching filename found');
		/if;
	/define_tag;
	
	define_tag: 'library', -description='includes file just as ->include, but returns no output', 
		-required='file',
		-optional='path';
		// includes file just as ->include, but returns no output
		!local_defined('path') ? local('path'=null);
		self -> include(#file, -path=#path);
		// returns nothing
	/define_tag;

	define_tag: 'directorytree', -description='Returns a map of all existing knop file paths', 
		-optional='basepath'; // only used for recursive calls
		local: 'timer'=knop_timer, 'dotrace'=(self -> 'dotrace'); 
		local: 'dirlist'=map;
		if: (self -> 'directorytreemap') -> size;
			// use the stored directory tree that has already been created
			#dirlist = (self -> 'directorytreemap');
			#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': Returning stored directorytree');

		else;
			// first time calling this tag - create the directory tree
			if: (local_defined: 'basepath');
				local: 'path' = #basepath;
			else;
				local: 'path'=(self -> 'fileroot');
			/if;
			!(#path -> (endswith: '/')) ? #path += '/';
			local: 'diritem'=string,
				'dirlist_sub'=map,
				'diritem_sub'=pair;
			iterate: file_listdirectory: #path, #diritem;
				if: !(#diritem -> (beginswith: '.'));
					#dirlist_sub = map;
					#diritem -> (removetrailing: '/');
					if: //#diritem -> (endswith: '/') && 
						((map: '_knop', '_include', '_config', '_action', '_library', '_content') >> #diritem
							|| #diritem -> (beginswith: '_mod_'));
						// recursive call for sub folder within the Knop directory structure
						#dirlist_sub = self -> (directorytree: #path + #diritem);
						iterate: #dirlist_sub, #diritem_sub;
							#dirlist -> (insert: #diritem + '/' +  #diritem_sub -> name);
						/iterate;
					/if;
					// Add item to map, with trailing / if item has sub items (folder contents)
					#dirlist -> (insert: #diritem + (#dirlist_sub -> size ? '/'));
				/if;
			/iterate;
			if: !(local_defined: 'basepath');
				// this was the topmost call in the recursive chain, so store the result
				(self -> 'directorytreemap') = #dirlist;
				#dotrace ? (self -> 'debug_trace') -> (insert: tag_name + ': Creating directorytree');
			/if;
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: #dirlist;
	/define_tag;


	define_tag: 'trace', 
		-optional='html',
		-optional='xhtml';
		local: 'dotrace'=(self -> 'dotrace');

		local: 'endslash' = ((self -> (xhtml: params)) ? ' /' | '');

		local: 'eol'=(local_defined: 'html') || #endslash -> size ? '<br' + #endslash + '>\n' | '\n';

		return: #eol + 'Debug trace for nav $' + (self -> varname)
			+ (!#dotrace ? ' (detailed trace not enabled)' )
			+ #eol + (self -> 'debug_trace') -> (join: #eol) 
			+ #eol;

	/define_tag;

/define_type;


?>
[
//------------------------------------------------------------------
//    End knop_nav
//------------------------------------------------------------------

//##################################################################

][
//------------------------------------------------------------------
//    Begin knop_user
//------------------------------------------------------------------

]<?LassoScript

define_type: 'user',
	'knop_base',
	-namespace='knop_';
//	-prototype;

	local: 'version'='2009-09-18',
		'descripion'='Custom type to handle user identification and authentication';

/*

CHANGE NOTES
2012-01-16	SP	Added ->removedata to remove field from the data map.  Thanks to Ric Lewis.
2009-09-18	JS	Syntax adjustments for Lasso 9
2009-06-23	JS	->encrypt now uses default encrypt_cipher from the custom type instead of a hard coded default
2009-02-26	JS	->login: further correction on the search for login with FileMaker, to reduce the risk for false duplicates
2009-02-26	JS	->login: Added optional -searchparams to be able to add more conditions to the login search, for example to exclude users that are not enabled. 
2008-12-02	JS	->encrypt: Changed to -hex cipher instead of encode_base64
2008-11-05	JS	->getdata: corrected a check that prevented the tag from returning anything
2008-11-05	JC	->login: A failed login attempt now results in a logout instead of keeping any old authentication
2008-11-05	JC	->getpermission will always return falseif a user is not logged in
2008-11-05	JC	->logout: The permissions map is now cleared when logging out
2008-11-04	JC	->encrypt: changed incorrect encrypt_cipher to encrypt_digest
2008-09-10	JS	Added ondeserialize to make client_fingerprint_expression survive session
2008-07-17	JS	Implemented ->setpermission and ->getpermission
2008-07-17	JS	Added client_fingerprint_expression as compound expression so it can be configurable by changing the instant variable
2008-05-20	JS	->login: Added delay between more than 5 failed login attempts 
2008-05-08	JS	->login: improved the search for FileMaker datasources to make it work for email address as username
2008-02-08	JS	Added ->keys
2008-02-03	JS	-> login: Corrected storage of id_user
2007-11-27	JS	Coded an incomplete version
2007-06-13	JS	Created the data type

// TODO: 
Make it possible for knop_user to work independently of a knop_database object by creating a custom user lookup - see http://listsearch.com/Lasso/Thread/index.lasso?20528
userdb reference is brooken, probably when stored in session. Can this be fixed?
Make client_fingerprint configurable by specifying a compound expression at oncreate
Add support for role based permisions

*/


/*
Purpose:
- Maintain user identity and authentication
- Handle database record locking more intelligently, also to be able to release all unused locks for a user
- Authenticating user login
- Restricting access to data
- displaying specific navigation options depending on type of user

lets add some date handling in there too like time of last login
and probably the IP that the user logged in from.


Some options to handle what happens when a user logs in again whilst already logged in.
ie one could:
disallow second login (with a message explaining why)
automatically log the first session out (with a message indicating what happened)
send a message to first session: "Your username is attempting to log in again, do you wish to close this session, or deny the second login attempt?"
allow multiple logins (from the same IP address)
allow multiple logins from any IP address

All of these could be useful options, depending of the type of app.

And different types of user (ie normal, admin) could have different types of treatment.

Handling for failed login attempts:
Option to set how many tries can be attempted; 
Option to lock users out permanently after x failed attempts? 
Logging (to database) of failed logins / successful logins

Password recovery system (ie emailing a time sensitive link to re-set password)
By "password recovery" I'm not thinking "email my password" (hashed passwords can't be emailed...) but rather to email a short lived link that gives the user an opportunity to change his password. How is this different from "password reset"?
Yes, that is an accurate description of what I had in mind, except for the bit about emailing a short-lived link.  Instead I imagined having the user reset their password 100% on the web site through the use of "Security Questions", much like banks employ.

I like the idea of more info attached to the user. Like login attempts, locking a user temporarily after too many failed attempts etc.


The setup is more or less that I have users and groups.

I'm thinking that Knop shouldn't do any session handling by itself, but the knop_user variable would be stored in the app's session as any other variable. Knop should stay as lean as possible...

Other things to handle:
Prevent session sidejacking by storing and comparing the user's ip and other identifying properties.
Provide safe password handling with strong one-way salted encryption.

consider having a separate table for auditing all user actions, including logging in, logging out, the basic CRUD actions, searches

The object have to handle situations where no user is logged in. A guest can still have rights to some actions. Modules that can be viewed. Forms that could be sent in etc.
That the added functions don't slow down the processing. We already have a lot of time consuming overhead in Knop.



Features:
1. Authentication and credentials
- Handle the authentication process
- Keep track of if a user is properly logged in
- Optionally keep track of multiple logins to same account
- Prevent sidejacking
- Optionally handle encrypted/hashed passwords (with salt)
- Prevent brute force attacks (delay between attempts etc)
- Handle general information about the user
- Provide accessors for user data

2. Permissions and access control
- Keep track of what actions a user is allowed to perform (the "verbs")
- Tie into knop_nav to be able to filter out locations based on permissions

3. Record locks
- Handle clearing of record locks from knop_database

4. Audit trail/logging
- Optionally log login/logout actions
- Provide hooks to be able to log other user actions

Future additions:
- Keep track of what objects and resources a user is allowed to act on (the "nouns")
- Provide filtering to use in database queries
- What groups a user belongs to
- Mechanism to update user information, password etc
- Handle password recovery


Permissions can be read, create, update, delete, or application specific (for example publish)

*/

	local: 'id_user'=null,
		'validlogin'=false,
		'groups'=array,
		'data'=map,						// map with arbitrary user information (name, address etc)
		'permissions'=map,
		'loginattempt_date'=(date: 0),	// to keep track of delays multiple login attempts
		'loginattempt_count'=integer,	// number of failed login attempts

		'userdb'=null,					// database object for user authentication
		'useridfield'='id',	
		'userfield'='username',	
		'passwordfield'='password',
		'saltfield'=null,
		'encrypt'=false,
		'encrypt_cipher'='RIPEMD160',	// digest encryption method

		'logdb'=null,					// database object for logging
		'logeventfield'='event',		// the event to be logged
		'loguserfield'='id_user',		// the user who is performing the logged action
		'logobjectfield'='id_object',	// what object is affected by the logged action
		'logdatafield'='data',			// details about the logged action

		'singleuser'=false,
		'uniqueid'=null,				// To track multiple logins on the same account (this is to be stored and compared server side)
		'client_fingerprint'=null,		// combination of ip, useragent etc to be able to track sidejacking
		'client_fingerprint_expression'={return(encrypt_md5(string(client_ip) + client_type))},
		'dblocks'=array,				// a list of all database objects that have been locked by this user 
		'error_lang'=(knop_lang: -default='en', -fallback),
		;

	define_tag: 'oncreate', -description='Parameters:\n\
			-encrypt (optional flag or string) Use encrypted passwords. If a value is specified then that cipher will be used instead of the default RIPEMD160. If -saltfield is specified then the value of that field will be used as salt.\n\
			-singleuser (optional flag) Multiple logins to the same account are prevented (not implemented)',
		-required='userdb', -type='database',
		-optional='encrypt',
		-optional='useridfield', -type='string',
		-optional='userfield', -type='string',
		-optional='passwordfield', -type='string',
		-optional='saltfield', -type='string',
		-optional='logdb', -type='database',
		-optional='loguserfield', -type='string',
		-optional='logeventfield', -type='string',
		-optional='logdatafield', -type='string',
		-optional='singleuser';
		
		local: 'timer'=knop_timer; 

		local_defined('userfield') ? (self -> 'userfield') = #userfield;
		local_defined('useridfield') ? (self -> 'useridfield') = #useridfield;
		local_defined('passwordfield') ? (self -> 'passwordfield') = #passwordfield;
		local_defined('saltfield') ? (self -> 'saltfield') = #saltfield;
		local_defined('loguserfield') ? (self -> 'loguserfield') = #loguserfield;
		local_defined('logeventfield') ? (self -> 'logeventfield') = #logeventfield;
		local_defined('logdatafield') ? (self -> 'logdatafield') = #logdatafield;

		// the following params are stored as reference, so the values of the params can be altered after adding a field simply by changing the referenced variable. 
		local_defined('userdb') ? (self -> 'userdb') = @#userdb;
		local_defined('logdb') ? (self -> 'logdb') = @#logdb;

		if: (local_defined: 'encrypt') && #encrypt != false;
			(self -> 'encrypt') = true;
			if: #encrypt -> size && (Cipher_List: -digest) >> #encrypt; // a valid digest cipher was specified
				(self -> 'encrypt_cipher') = #encrypt;
			/if;
		else;
			(self -> 'encrypt') = false;
		/if;
		(self -> 'singleuser') = (local_defined: 'singleuser') && #singleuser != false;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;


	define_tag: 'ondeserialize', -description='Recreates transient variables after coming back from a session';
		// MARK: Why is client_fingerprint_expression considered a transient variable?
		self -> properties -> first -> insert('client_fingerprint_expression'={return(encrypt_md5(string(client_ip) + client_type))});
	/define_tag;

/*
	define_tag: 'onassign', -description='Internal, needed to restore references when ctype is defined as prototype',
		-required='value';
		// recreate references here
		iterate: (array: 
			'userdb',
			'logdb'), (local: 'param');
			(self -> #param) = @(#value -> #param);
		/iterate;
	/define_tag;
// */	
	
	define_tag: '_unknowntag', -description='Shortcut to getdata';
		if: (self -> 'data') >> tag_name;
			return: (self -> 'data') -> (find: tag_name);
		else;
			//fail: -9948, self -> type + '->' + tag_name + ' not known.';
			(self -> '_debug_trace') -> insert(self -> type + '->' + tag_name + ' not known.');
		/if;
	/define_tag;

	define_tag: 'auth', -description='Checks if user is authenticated, returns true/false';
		local: 'timer'=knop_timer; 

		local: 'validlogin'=false, 'client_fingerprint_now'=string;
		// check validlogin
		#validlogin = (self -> 'validlogin');
		if: #validlogin;
			// check client_fingerprint to prevent sidejacking
			#client_fingerprint_now = (self -> 'client_fingerprint_expression') -> invoke;
			if: #client_fingerprint_now != (self -> 'client_fingerprint');
				#validlogin = false;
				(self -> '_debug_trace') -> insert(tag_name + ': Client fingerprint has changed - this looks like session sidejacking. Logging out.');
				(self -> 'error_code') = 7503;
				self -> logout;
				// TODO: log this
			/if;
			// TODO: if singleuser, check uniqueid
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
		return: #validlogin;
	/define_tag;


	define_tag: 'login', -description='Log in user. On successful login, all fields on the user record will be available by -> getdata.\n\
			Parameters:\n\
			-username (required) Optional if -force is specified\n\
			-password (required) Optional if -force is specified\n\
			-searchparams (optional) Extra search params array to use in combination with username and password\n\
			-force (optional) Supply a user id for a manually authenticated user if custom authentication logics is needed',
		-optional='username',
		-optional='password',
		-optional='searchparams', -type='array', -copy,
		-optional='force';
		local: 'timer'=knop_timer; 

		if(!local_defined('force') && (!local_defined('username') || !local_defined('password')));
			fail(-9956, self -> type + '->' + tag_name + ' requires -username and -password, or -force');
		/if;
		
		local: 'db'=@(self -> 'userdb'),
			'validlogin'=false;
		

		if(local_defined('force') && string(#force) -> size && #force != false);
			(self -> '_debug_trace') -> insert(tag_name + ': ' + 'Manually authenticating user id ' + #force);
			#validlogin = true;
			(self -> 'id_user') = #force;

		else;
			!local_defined('searchparams') ? local('searchparams'=array);
			if((local('username') -> size && local('password') -> size));
				if((self -> 'loginattempt_count') >= 5);
					// login delay since last attempt was made 
					(self -> '_debug_trace') -> insert(tag_name + ': Too many login attempts, wait until ' + (2 * (self -> 'loginattempt_count')) + ' seconds has passed since last attempt.');
					while(((date - (self -> 'loginattempt_date')) -> second) <  (2 * (self -> 'loginattempt_count')) // at least 5 seconds, longer the more attempts
						&& loop_count < 100); // rescue sling
						sleep(200);
					/while;
				/if;
				// authenticate user against database (username must be unique)
				(self -> '_debug_trace') -> (insert: tag_name + ': ' + 'Authenticating user');
				if(#db -> 'isfilemaker');
					#searchparams -> merge(array(-op='eq', (self -> 'userfield') = '="' + #username + '"'));
				else;
					#searchparams -> merge(array(-op='eq', (self -> 'userfield') = #username));
				/if;
				#db -> select(#searchparams);
				(self -> '_debug_trace') -> (insert: tag_name + ': ' + 'Searching user db, ' (#db -> found_count) + ' found ' + (#db -> error_msg) + ' ' + (#db -> action_statement));
				if: #db -> found_count == 1
					&& #db -> (field: (self -> 'userfield')) == #username; // double check the username
					// one match, continue by checking the password with case sensitive comparsion
					if: (self -> 'encrypt') && (self -> 'saltfield') -> size;
						// use encryption with salt
						(self -> '_debug_trace') -> (insert: tag_name + ': ' + 'Checking password with salted encryption');
						if: bytes: (#db -> (field: (self -> 'passwordfield'))) 
							== bytes: (self -> (encrypt: #password, -salt=#db -> (field: (self -> 'saltfield') ), -cipher=(self -> 'encrypt_cipher') ));
							#validlogin=true;
						/if;
					else: (self -> 'encrypt');
						// use encryption with no salt
						(self -> '_debug_trace') -> (insert: tag_name + ': ' + 'Checking password with encryption, no salt');
						if: bytes: (#db -> (field: (self -> 'passwordfield'))) 
							== bytes: (self -> (encrypt: #password, -cipher=(self -> 'encrypt_cipher')));
							#validlogin=true;
						/if;
					else;
						(self -> '_debug_trace') -> (insert: tag_name + ': ' + 'Checking plain text password');
						if: bytes: (#db -> (field: (self -> 'passwordfield'))) 
							== bytes: #password;
							#validlogin=true;
						/if;
					/if;
				/if;
				if(#validlogin);
					(self -> '_debug_trace') -> (insert: tag_name + ': ' + 'id_user: ' + #db -> (field: (self -> 'useridfield')));
					// store user id
					(self -> 'id_user') = #db -> (field: (self -> 'useridfield'));
					// store all user record fields in data map
					(self -> 'data') = #db -> recorddata;
				/if;
			/if; // #username and #password
		/if; // #force

		if: #validlogin; 
			(self -> '_debug_trace') -> (insert: tag_name + ': ' + 'Valid login');
			(self -> 'loginattempt_count') = 0;
			(self -> 'error_code') = 0; // No error
			// set validlogin to true
			(self -> 'validlogin')=true;
			// log the action TODO
			// store client_fingerprint
			(self -> 'client_fingerprint') = (self -> 'client_fingerprint_expression') -> invoke;
			// if singleuser, store uniqueid in server side storage
		else(!(local('username') -> size && local('password') -> size));
			(self -> 'error_code') = 7502; // Username or password missing
			self -> logout;
		else;
			// TODO:
			// - block username for a while after too many attempts
			(self -> 'loginattempt_count') += 1;
			(self -> 'loginattempt_date') = date; // keep track of when last login attempt happened
			(self -> '_debug_trace') -> (insert: tag_name + ': ' + 'Invalid login (' +  (self -> 'loginattempt_count') + ' attempts)');
			(self -> 'error_code') = 7501; // Authentication failed
			self -> logout;
			// exit
		/if;

		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	
	define_tag: 'logout'; 
		local: 'timer'=knop_timer; 
		// set validlogin to false
		(self -> 'validlogin')=false;
		(self -> 'id_user') = null;
		(self -> 'data') = map;
		(self -> 'permissions') = map;
		
		// clear all record locks
		self -> clearlocks;
		// log the action

		(self -> '_debug_trace') -> (insert: tag_name + ': ' + 'Logged out');
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;

	define_tag: 'getdata', -description='Get field data from the data map.',
		-required='field';
		if: (self -> 'data') >> #field;
			return: (self -> 'data') -> (find: #field);
		else;
			(self -> '_debug_trace') -> insert(tag_name + ': ' + #field + ' not known');
		/if;
	/define_tag;

	define_tag: 'removedata', -description='Remove field from the data map.',
		-required='field';
		if: (self -> 'data') >> #field;
			(self -> 'data') -> (remove: #field);
		else;
			(self -> '_debug_trace') -> insert(tag_name + ': ' + #field + ' not known');
		/if;
	/define_tag;

	define_tag: 'id_user', -description='Return the user id';
		if: self -> auth;
			return: (self -> 'id_user');
		else;
			return: false;
		/if;
	/define_tag;

	define_tag: 'setdata', -description='Set field data in the data map. Either -> (setdata: -field=\'fieldname\', -value=\'value\') or -> (setdata: \'fieldname\'=\'value\')',
		-required='field', -copy,	// can also be a pair with field=value
		-optional='value', -copy;
		local: 'timer'=knop_timer; 
		if: #field -> isa('pair');
			local: 'value'=#field -> value;
			#field = #field -> name;
		/if;
		fail_if: !(local_defined: 'value'), -1, (self -> type) '->setdata requires a value parameter';
		(self -> 'data') -> insert(#field = #value);
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;
	
	define_tag: 'getpermission', -description='Returns true if user has permission to perform the specified action, false otherwise',
		-required='permission';
		if((self -> auth) && (self -> 'permissions') >> #permission);
			return((self -> 'permissions') -> find(#permission));
		else;
			return(false);
		/if;
	/define_tag;

	define_tag: 'setpermission', -description='Sets the user\'s permission to perform the specified action (true or false, or just the name of the permission)',
		-required='permission',
		-optional='value';
		if(local_defined('value') && #value != false); // any non-false value is regarded as true
			(self -> 'permissions') -> insert(#permission=true);
		else(local_defined('value') && #value == false); // explicit false
			(self -> 'permissions') -> insert(#permission=false);
		else; // no value specified is regarded as true
			(self -> 'permissions') -> insert(#permission=true);
		/if;
	/define_tag;


	define_tag: 'addlock', -description='Called by database object, adds the name of a database object that has been locked by this user.',
		-required='dbname';
		if: (self -> 'dblocks') !>> #dbname && (var: #dbname) -> (isa: 'database');
			(self -> '_debug_trace') -> insert(tag_name + ': adding database name  ' + #dbname);
			(self -> 'dblocks') -> (insert: #dbname);
		/if;
	/define_tag;

	define_tag: 'clearlocks', -description='Clears all database locks that has been set by this user';
		local: 'timer'=knop_timer; 
		if: (self -> auth);
			(self -> '_debug_trace') -> (insert: tag_name + ': ' + (self -> 'dblocks') -> (join: ', '));
			iterate: (self -> 'dblocks'), local: 'dbname';
				if: (var: #dbname) -> (isa: 'database');
					(var: #dbname) -> (clearlocks: -user=(self -> 'id_user'));
					#dbname = null;
				/if;
			/iterate;
			// remove all locks that has been cleared
			(self -> 'dblocks') -> (removeall: null);
			(self -> '_debug_trace') -> (insert: tag_name + ': done, remaining locks: ' + (self -> 'dblocks') -> (join: ', '));
		/if;
		self -> 'tagtime_tagname'=tag_name;
		self -> 'tagtime'=integer: #timer; // cast to integer to trigger onconvert and to "stop timer"
	/define_tag;


	define_tag: 'encrypt', -description='Internal use. Encrypts the input using digest encryption, optionally with salt. ',
		-required='data', -copy,
		-optional='salt',
		-optional='cipher';
		local: 'output'=string;
		!(local_defined: 'cipher') ? local: 'cipher'=self -> 'encrypt_cipher';
		if: (local_defined: 'salt');
			#data = #salt + #data;
		/if;
		if: (Cipher_List: -digest) !>> #cipher;
			// fall back to default digest cipher
			#cipher = 'MD5';
		/if;
		#output = (cipher_digest: #data, -digest=#cipher, -hex);
		return: #output;
	/define_tag;
	
	define_tag: 'keys', -description='Returns all keys for the stored user data';
		return: (self -> 'data') -> keys;
	/define_tag;

/define_type;



?>
[
//------------------------------------------------------------------
//    End knop_user
//------------------------------------------------------------------

//##################################################################

][define_tag('changenotes', -description='This tag is created on the fly by buildnamespace.lasso',
		-namespace='knop_',
		-optional='type', -optional='date', -copy);
		local('output'=string, 'changenotes'=map('knop_nav'='2010-11-17	JC	Fixed bug so that session links no longer gets added to urls by the nav -> url tag.
2010-11-17	JC	Fixed a bug that would not convert local params to an array under certain situations
2009-09-18	JS	Syntax adjustments for Lasso 9
2009-09-04	JS	->linkparams: Multiple paramaters with the same name (typically checkboxes) are now passed properly
2009-05-06	JS	->directorytree considers _include folders as part of the Knop directory structure. nav->include(\'_include/myfile.inc\') will first look for _include/myfile.inc and if not found it will look for _knop/_include/myfile.inc 
2009-05-05	JS	->include looks for a specified file also inside a _knop folder, if the file does not exist at the specified location
2009-02-09	JS	->filename: Casting path and actionpath to string
2008-12-19	JS	->filename (and consequently ->include and ->library) can now use a specific -path instead of the current location\'s path
2008-12-09	JS	->linkparams: fixed undefined local in trace call (only showed when trace was enabled for the nav object)
2008-11-25	JS	->getlocation will now avoid disabled and hidden pages when looking for the first page if no default page is specified
2008-11-03	JS	->getlocation will not break if no navigation items have been defined
2008-10-30	JS	_mod folders will now work with knop paths with just a single level, so an include file path can end up as _mod_customer/lib_customer.inc or just _mod_customer/lib.inc (the latter variant does not apply to extension-based filenaming, so _mod_customer/.lib will never be a working file path)
2008-07-10	JS	Added -> label to return the name of the current page
2008-05-20	JS	->renderhtml: #current# is not automatically added to the template if the more elaborate template format is used. This makes it easier to hide the currentmarker without changing the nav config (partial revert of fix 2008-01-04). 
2008-05-08	JS	->insert: A duplicate key does no longer cause a fatal error but instead fails silently and logs to the debug log
2008-05-07	JS	->url: will now call ->linkparams so all links that are constructed from ->url will properly send along the parameters specified in the nav item. This also affects pagination and sort links in grid. 
2008-05-07	JS	Added ->linkparams, Returns an array for all parameters that should be sent along with nav links (this was previously embedded in renderhtml)
2008-02-25	JS	->url: new parameter -getargs to add the getargs to the path link if the path equals the current path
2008-02-06	JC	->insert: the -hide flag can now be a boolean (also the -disabled flag)
2008-02-03	JS	->include: can now include any specified filename
2008-02-01	JS	->oncreate: added optional -fileroot to be able to use a root for files that is different from the logical site root used for navigation 
2008-01-23	JS	->url: Added -autoparams that is required to enable the automatic passing of action_params that begin with "-" (this reverts the default behavior to match the the old)
2008-01-22	JS	->url: GET params that begin with "-" are sent as parameters on links. -path, -sort, -desc, -q are explicitly excluded from nav links in renderhtml. 
2008-01-22	JS	When using param based navigation, navigation links now use much cleaner /?path/to/page/ style links instead of /?-path=path/to/page/
2008-01-22	JS	->getlocation: when navmethod is param, the path can now be sent as unnamed parameter insetad of -path parameter (such as /?path/to/page/). 
2008-01-04	JS	->renderbreadcrumb: added flag -plain to output breadcrumb without html
2008-01-04	JS	->renderbreadcrumb: added flag -skipcurrent to not include the current location in the output
2008-01-04	JS	->insert: -template can now be specified also for individual nav items. Use the form #link##label##current##/link##children#. 
2008-01-04	JS	->renderhtml: changed #title# to #label# in template for clarity, for example #link##label##current##/link##children# (#title# will remain supported)
2008-01-04	JS	Added #current# as placeholder for template, to specify where the current marker should occurr. If not specified in the template, the current marker appears immediately after the label.
2007-12-12	JS	->include now logs processing time for the include to debug trace
2007-12-11	JS	Added documentation as -description to most member tags, to be used by the new ->help tag
2007-12-11	JS	Moved ->help to knop_base
2007-12-04	JS	nav item css class is now applied also to disabled nav items (rendered as <span>)
2007-11-08	JS	Changed trace so it tracks some things even when it\'s not enabled (like include etc)
2007-11-05	JS	Added var name to trace output
2007-10-28	JS	->directorytree: should now work also when knop folders are symlinks
2007-09-06	JS	top level nav elements that are -disabled now behave properly when accessed
2007-09-05	JS	-currentmarker can now be set separately on sublevels, not only on topmost level
2007-08-29	JS	Added _knop as optional base folder to put all knop files in
2007-08-29	JS	->include: Removed file_exists check since the filename has already been verified in ->filename.
2007-08-29	JS	Added ->directorytree which returns a map with all knop filenames, to use when searching for includes
2007-08-28	JS	->oncreate: -filenaming to specify how include files are named prefix/suffix/extension
2007-08-28	JS	->filename: Implemented support for flexible folder structures
2007-08-28	JS	Instance variable #actionconfigfile_didrun was not properly declared
2007-06-18	JS	Added tag timer to most member tags
2007-06-14	JS	->insert: -url=\'/\' can now be used to specify the "home" location. 
2007-06-13	JS	added ->children to get a reference to the children nav object for a specified path, so new children can be inserted. Must call ->reindex afterwards. 
2007-06-13	JS	added ->reindex to rebuild the index maps from scratch. Must be done after adding children items. 
2007-06-13	JS	added ->addchildren to replace a current children nav object for a specified path. Will handle the reindexing transparently. 
2007-06-13	JS	added ->keymap, ->pathmap and ->urlmap to access the index maps so they call reindex if they have been invalidated (for example by ->addchildren)
2007-06-13	JS	added inheritance from knop_base
2007-06-11	JC	added handling of xhtml output
2007-06-08	JS	->insert: params with empty values are now ignored
2007-05-04	JS	->insert: added check that default item exists in children before storing the default
2007-04-19	JS added ->data to retrieve data stored for the current path (or specified path). Optional -type ensures the returned data has the correct type. 
2007-04-19	JS ->insert: added -data to store arbitrary data object for each path. The object is stored as reference so a variable can be changed after it has been added to the nav object
2007-04-19	JS	added ->patharray to return the current path as array
2007-04-17	JS	->renderhtml: template now supports #link##title##/link##children# in addition to #item# to provide more flexibility
2007-04-17	JS	->oncreate: added support for -template, -class, currentclass and -currentmarker
2007-04-13	JS	Implemented -class per navitem (only worked globally with setformat before)
2007-04-03	JS	->renderhtml: added -renderpath, -expand and -flat to be able to render parts of the nav menu for more flexible layout
2007-04-03	JS	->renderbreadcrumb: added -home to show the default navigation item first in the breadcrumb 
2007-04-03	JS	Changed namespace from mt_ to knop_
2007-03-01	JS	Changed navmethod path so it uses response_filepath instead of $url_path
2007-02-25	JS	Added ->actionpath
2007-02-24	JS	->renderhtml: improved handling of classes in nav links
2007-02-22	JS	->url: added -except
2007-02-09	JS	->url: corrected the behavior for plain parameters passed to the tag
2007-02-05	JS	->insert: Added -param to be able to specify params that should be propagated in nav for certain nav elements (like -keyvalue to be able to move between different subtabs for a selected record
2007-02-05	JS	->renderhtml now use ->url to get the right links
2007-02-01	JS	Made usage of trace optional to improve performance
2007-02-01	JS	Improvements to debug_trace to log also recursive events
2007-02-01	JS	Added ->haschildren, which is now used by ->renderhtml to properly show or hide child level
2007-01-31	JS	->renderhtml Added renderhtml_levels to keep track of how many levels deep navigation has been rendered, to be able to add proper spacing between navigation and content
2007-01-31	JS 	->url: -urlargs  Improved handling of urlargs and ?/& delimiters, should work better with navmethod path
2007-01-30	JS	Removed automatic link title attribute since it can be confusing to show children page titles there
2007-01-30	JS	Corrected parameter path to -path (bug)
2007-01-23	JS	->include: \'config\' checks if the same config has already run as actionconfig and won\'t run again in that case
2007-01-23	JS	->include checks if the file exists first, so no need for empty placeholder files
2007-01-23	JS	Added ->include and ->library
2007-01-23	JS	Added ->setlocation
2007-01-17	JS	Added insert: -hide to allow a location without showing it in navigation

TODO
Add support for compound expressions for template. The expression could return a map that would override corresponding param values. 
->insert: Add -raw to be able to inject code into the link tag, similar to form->addfield(-raw). 
Exclude file name for example index.lasso from getargs
Optimize nav->url! Very slow with complex nav object. 
Needs to exclude also -keyfield and -lockfield. Maybe better to add an option to ->url to not auto-add any "-" params at all. 
-params are not sent along in breadcrumb links
Need simple way to exclude certain "-" params from ->url, also in config per nav item
Add support for adding nav structure from a database
Move templates to a member tag to be make it easier to subclass
Make it possible to use external URL for -url (make sure there is no / before http)

','knop_base'='2009-09-14	JS	Syntax adjustments for Lasso 9
2009-09-04	JS	Changed $__html_reply__ to content_body
2009-04-07	JS	->error_msg: custom error numbers can now be added, even if the language already exists.
2008-01-10	JS	->error_msg: improved reporting of custom error messages such as from bad database queries
2007-12-13	JS	Added -> error_lang to provide a reference to the knop_lang object for error messages, to be able to add localized error messages to any Knop type (except knop_lang and knop_base)
2007-12-12	JS	Added -html and -xhtml to ->help to get a nicely formatted output. 
2007-12-11	JS	Centralized ->error_code and ->error_msg to knop_base. Moved all error codes to error_msg
2007-12-06	JS	Changed ->help to improve the self-documentation. It will now always return an up to date list of member tags and parameter. 
2007-11-05	JS	Added var name to trace output
2007-06-17	JS	Added ->tagtime (was in nav earlier)
2007-06-13	JS	Added -> varname to be able to retreive the name of the page variable that a type instance is stored in.
2007-06-13	JS	Added -> xhtml to automatically sense if an xhtml doctype exists in the current page buffer. The result is cached in a page variable for performance. 
				This is for internal use for member tags that output html. 
2007-06-13	JS	Introduced page variable $_knop_data for general page level storage and caching, common between different knop objects. 
2007-06-13	JS	Created the data type

TODO: ->help: add output option to format for Google Code Wiki
->xhtml is not working properly when site is run by atbegin handler and explicitly writing to content_body 


','knop_grid'='2011-01-01	SP	Correction of invalid HTML in <thead> and <tr>
2010-12-23	SP	Corrected pagination bug for -numbered.
2010-11-17	JC	Added -startwithfooter flag to grid->renderhtml.  This moves the footer before the column titles in the table header.
2010-11-17	JC	Changed rawheader inclusion to work even if there\'s no quicksearch for a grid
2010-05-14	JC	Added span separation on grid footer for better styling
2010-03-06	SP	Changed default behavior of ->sortparams and ->quicksearch with -sql to add backticks between the table and column names.  Now JOINs may be used.
2010-03-06	SP	Added ->sortparams and ->quicksearch with -removedotbackticks for backward compatibility for fields that contain periods.  If you use periods in a fieldname then you cannot use a JOIN in Knop.
2010-01-27	JC	Adjusted the id support
2010-01-25	JC	Added support for optional id, used by table, quicksearch and quicksearch_reset
2009-09-18	JS	Syntax adjustments for Lasso 9
2009-08-26	JS	Corrected prev/next links when no nav is defined for the grid object
2009-06-29	JS	->renderlisting: documentation correction (renderlisting never calls renderfooter). 
2009-01-11	JS	->renderlisting: Made sure that template isn\'t applied on NULL field values since that would cause an error with ->replace 
2008-12-29	JC	Support for optional classes in table header
2008-11-27	JC	-> implemented support for td specific classes. The class is inserted in both the TH and TD tag for the specified field
2008-09-24	JS	->sortparams: fieldnames specified by the -sort parameter are now validated so they exist in the database
2008-09-24	JS	->sortparams, ->quicksearch: Added protection against backtick sql injection in MySQL object names
2008-09-10	JS	-numbered can now be specified at oncreate in addition to  ->renderhtml and ->renderfooter. 
2008-09-08	JS	->sortfield changed so defaultsort is honored even if -nosort is specified (-nosort is only used to prevent the user from changing sort order on the fly)
2008-09-08	JS	->renderlisting and ->renderfooter optimized by caching the result of nav->url. 
2008-05-15	JS	->renderfooter: minor adjustment to numbered pagination links
2008-05-14	JS	->renderfooter (and ->renderhtml): added -numbered as option to get google style numbered pagination links. Render the grid with -numbered (defaults to show 6 page number links plus the far ends) or -numbered=10 or another number. 
2008-05-13	JS	Added ->renderlisting which is now part of ->renderhtml for more flexibility
2008-02-25	JS	->renderheader and ->renderfooter calls nav -> url with -getargs 
2008-01-22	JS	->renderheader, ->renderfooter, ->renderhtml: addded -autoparams to nav -> url used in links since url was changed to default to no autoparams. 
2008-01-22	JS	->renderheader, ->renderfooter, ->renderhtml: improved support for param based navigation method  in links, cleaned up linking to use nav -> url instead of self -> urlparams when nav is available
2008-01-22	JC	->quicksearch: Changed wordseparators so that \\r and \\n aren\'t placed as \\r\\n, otherwise they are treated as a single character by ->split. 
2007-12-11	JS	Added documentation as -description to most member tags, to be used by the new ->help tag
2007-12-11	JS	Moved ->help to knop_base
2007-12-03	JS	Added optional -language parameter to set the initial language for the grid object
2007-12-03	JS	Added -> lang to provide a reference to the knop_lang object that is used for localized strings
2007-12-03	JS	Added localized strings for English and Swedish
2007-12-03	JS	Added knop_lang to handle localized strings
2007-11-11	JC	Added optional -rawheader for extra header content to be inserted before the Quicksearch form
2007-10-23	JS	->renderheader: added class="grid" to the opening table tag to be able to isolate the css specifications
2007-10-03	JS	->renderfooter: added unique classes for each type of prev/next link to be able to replace with images using css
2007-10-03	JS	->renderfooter: Changed prev/next link texts to simple |< << >> >| instead of unicode glyphs
2007-09-20	JS	Postparams are only sent along for nav params (not "-" params)
2007-09-07	JS	Also send postparams along in prev/next links (not only getparams)
2007-09-06	JS	If nav has params defined, those params will be sent along with prev/next links
2007-09-06	JS	Added encode_url for link params
2007-09-06	JS	Changed -action to -formaction in the quicksearch form
2007-08-08	JS	->urlargs: Added exception for -session
2007-06-18	JS	Added tag timer to most member tags
2007-06-13	JS	added inheritance from knop_base
2007-06-11	JC	added handling of xhtml output
2007-05-30	JS	->quicksearch: added \\r and \\n as word separators. 
2007-05-03	JS	->addfield: Added check for empty dbfield name before adding to dbfieldmap
2007-04-19	JS	->quicksearch: added  -value (flag) that makes quicksearch output just the value of the quicksearch field instead of a query
2007-04-13	JS	->oncreate: added -nosort as global flag (overrides column specific sort options)
2007-04-13	JS	->renderfooter: Added tbody to footer to make it pass validation, but it\'s still not semantically correct. 
2007-04-13	JS	Changed field type for quicksearch for non-WebKit based browsers
2007-04-10	JS	->quicksearch: Added @ as word separator for "word begins with" search
2007-04-10	JS	->quicksearch: Added -contains as option to perform a simple contains search instead of emulating "word begins with" search
2007-04-10	JS	->renderheader: When grid has a defaultsort, there should be no "unsort" option in the sortable headings
2007-04-10	JS	->insert: (-defaultsort=\'desc\') or (-defaultsort=\'descending\') makes the default sort order sort in descending order
2007-04-04	JS	->addfield: -template can now also be a compound expression
2007-04-03	JS	Changed namespace from mt_ to knop_
2007-03-01	JS	Added support for FileMaker with quicksearch (untested)
2007-03-01	JS	Changed all texts to English
2007-02-07	JS	Removed classs="first" and class="notopborder" since CSS border-collapse: collapse; eliminates the need
2007-02-05	JS The -keyvalue parameter for url edit links can be given another name by specifying -keyparamname in addfield
2007-01-31	JS	->addfield: #value# can be used in -url, as a way to provide mailto links etc in lists
2007-01-30	JS	Sortable columns now thave three states instead of two: sorted ascending, sorted descending and unsorted. 
2007-01-30	JS	Improvements to quicksearch, to emulate "word begins with"  searches
2007-01-26	JS	Added support for quicksearch field in grid header. Specify -quicksearch at ->oncreate, and tell what fields to search by specifying -quicksearch for those fields at ->addfield. 
				Fields can be search only (no display) by leaving out name and label. All specified fields are searched word for word of thew search string. 
				Use ->quicksearch to get the search parameters (optionally -sql)
2007-01-26	JS	Added ->urlargs to get a querystring with all "-" GET params, except those in optional -except string or array parameter. 
2007-01-22	JS	Adjustments to highlighting and "(redigera)"-link for records with empty values in the linked field
2007-01-19	JS	Added href titles to sort links and prevnext links
2007-01-19	JS	Corrected sortparams when no sortfield is specified
2007-01-19	JS	Addded go to first page, go to last page in footer
2007-01-19	JS	Added member tag page_skiprecords to help correcting out of bounds page numbers
2007-01-19	JS	Added member tag lastpage
2007-01-18	JS	Support for highlighting of affected record after edit or update (class name "highlight")
2007-01-17	JS	Added addfield: -template

TODO
Make it possible for knop_grid to work independently of a knop_database object so other types of listings can bre created. 
Language of quicksearch buttons can\'t be changed after the grid has been created
tbody is used in renderfooter, which is not semantically correct. can\'t use tfoot though since the footer is rendered twice. 
Move templates to a member tag to be make it easier to subclass
Change ->addfield to ->insert and make ->addfield deprecated

','knop_database'='2012-06-10	SP	Fix for decimal precision bug in 8.6.0.1 in renderfooter.
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
2008-05-27	JS	Implemented record pointer \'current_record\'. The record pointer is reset for each new query. 
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
2007-12-03	JS	Corrected shown_first once again, hoping it\'s right this time
2007-11-29	JS	Added support for field_names and corresponding member tag ->field_names
2007-11-05	JS	Added var name to trace output
2007-10-26	JS	->capturesearchvars: corrected shown_first when no records found
2007-10-26	JS	->oncreate: added default value "keyfield" if the -keyfield parameter is not specified
2007-09-06	JS	Corrected self -> \'tagtime\' typo
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
2007-01-26	JS	Adjusted affectedrecord_keyvalue so it\'s only captured for -add and -update
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


','knop custom tags in util.inc'='	2007-06-17	JS	Created the type
	
	','knop_user'='2012-01-16	SP	Added ->removedata to remove field from the data map.  Thanks to Ric Lewis.
2009-09-18	JS	Syntax adjustments for Lasso 9
2009-06-23	JS	->encrypt now uses default encrypt_cipher from the custom type instead of a hard coded default
2009-02-26	JS	->login: further correction on the search for login with FileMaker, to reduce the risk for false duplicates
2009-02-26	JS	->login: Added optional -searchparams to be able to add more conditions to the login search, for example to exclude users that are not enabled. 
2008-12-02	JS	->encrypt: Changed to -hex cipher instead of encode_base64
2008-11-05	JS	->getdata: corrected a check that prevented the tag from returning anything
2008-11-05	JC	->login: A failed login attempt now results in a logout instead of keeping any old authentication
2008-11-05	JC	->getpermission will always return falseif a user is not logged in
2008-11-05	JC	->logout: The permissions map is now cleared when logging out
2008-11-04	JC	->encrypt: changed incorrect encrypt_cipher to encrypt_digest
2008-09-10	JS	Added ondeserialize to make client_fingerprint_expression survive session
2008-07-17	JS	Implemented ->setpermission and ->getpermission
2008-07-17	JS	Added client_fingerprint_expression as compound expression so it can be configurable by changing the instant variable
2008-05-20	JS	->login: Added delay between more than 5 failed login attempts 
2008-05-08	JS	->login: improved the search for FileMaker datasources to make it work for email address as username
2008-02-08	JS	Added ->keys
2008-02-03	JS	-> login: Corrected storage of id_user
2007-11-27	JS	Coded an incomplete version
2007-06-13	JS	Created the data type

// TODO: 
Make it possible for knop_user to work independently of a knop_database object by creating a custom user lookup - see http://listsearch.com/Lasso/Thread/index.lasso?20528
userdb reference is brooken, probably when stored in session. Can this be fixed?
Make client_fingerprint configurable by specifying a compound expression at oncreate
Add support for role based permisions

','knop_lang'='2010-12-29	JS	->getstring: make sure we only try to do replacement in strings or bytes
2010-05-27	JS	getstring now works also with empty -replace value
2010-04-20	JS	getstring debug output corrected for missing string. 
2009-09-16	JS	Syntax adjustments for Lasso 9
2009-06-26	JS	->addstring: clarified deprecation warning
2009-04-08	JS	Added -debug flag to oncreate (when creating a lang object) to make undefined language keys appear as *key*.
2009-03-24	JS	->getstring prevents replacements if there is no language string found, to prevent null->replace error
2008-11-12	JS	Added ->insert instead of ->addstring for better consistency with other Lasso data types. ->addstring will remain functional for backwards compatibility. 
2008-09-11	JS	->_unknowntag: added missing support for -language
2008-01-22	JS	->getstring: corrected the fallback behavior when a current language has been set 
2008-01-07	JS	Removed instance variable browserlanguage due to problems with transient storage. The browserlanguage is cached on page level so it\'s no loss in reality. 
2007-12-12	JS	Added page level storage of currentlanguage, so all knop_lang instances defaults to the same language once one of them have set a language explictly, but only if the other knop_lang instances don\'t have a language set explicitly. 
2007-12-12	JS	Added page level caching of browser language (stores the value in $_knop_data map)
2007-12-06	JS	Moved -> help to knop_base
2007-12-06	JS	Added ->description to all member tags. 
2007-12-03	JS	Finished first complete version
2007-06-18	JS	Added tag timer to most member tags
2007-06-13	JS	added inheritance from knop_base
2007-06-13	JS	Renamed to knop_lang (formerly knop_strings)
2007-04-04	JS	Created the data type and started coding

TODO:
Provide methods to handle formatting of dates, numbers, currency etc for different languages/locales. 
Weekday names, month names etc. 
Member tag to return the current language
Member tag to set default output encoding unless it follows encode_set. 

Examples
	var: \'lang_messages\'=(knop_lang: -default=\'en\');
	$lang_messages -> (insert: -key=\'welcome\', -value=\'Welcome to the home page\', -language=\'en\');
	$lang_messages -> (insert: -key=\'welcome\', -value=\'Välkommen till hemsidan\', -language=\'sv\');
	$lang_messages -> (insert: -key=\'loggedin\', -value=\'You are logged in as #1# #2#\', -language=\'en\');
	$lang_messages -> (insert: -key=\'loggedin\', -value=\'Du är inloggad som #1# #2#\', -language=\'sv\');

	// proper call, defaults to the browser\'s preferred language
	$lang_messages -> (getstring: \'welcome\');
	// shorthand call
	$lang_messages -> welcome;

	// change language
	$lang_messages -> (setlanguage: \'sv\');
	$lang_messages -> welcome;

	// proper call with replacements
	$lang_messages -> (getstring: -key=\'loggedin\': -replace=(array: (field: \'firstname\'), (field: \'lastname\')));

	// shorthand call with replacements
	$lang_messages -> (loggedin: -replace=(array: (field: \'firstname\'), (field: \'lastname\')));


','knop_form'='2011-02-28	JS	->addfield: Added -template to specify field specific template 
2010-11-22	SP	->init: Correction of -lockvalue handling after L9 syntax adjustment
2010-07-18	SP	Added support for series for -options
2010-06-10	JS	->renderform: avoid adding -upload parameters to post forms since it conflicts with file uploads (found by Steve Piercy)
2010-04-21	JS	->renderhtml:  removed encode_html for label
2010-03-06	SP	Changed default behavior of ->updatefields with -sql to add backticks between the table and column names.  Now JOINs may be used.
2010-03-06	SP	Added ->updatefields with -removedotbackticks for backward compatibility for fields that contain periods.  If you use periods in a fieldname then you cannot use a JOIN in Knop.
2009-11-11	JS	Added class and id to optiongroup div that surrounds for checkbox and radio
2009-11-11	JS	Corrected id for checkbox and radio option labels
2009-10-02	JS	Added id for labels, auto generated from the field\'s id with _label appended
2009-09-16	JS	Syntax adjustments for Lasso 9
2009-09-04	JS	Changed $__html_reply__ to content_body
2009-09-04	JS	->renderhtml: corrected typ for autoparams
2009-07-23	JS	->renderform: removed encode_html that somehow has reappeared for label. 
2009-07-10	SP	added -maxlength option for text fields
2009-06-26	JS	->oncreate: added deprecation warning for -action
2009-06-22	JS	->addfield: corrected -options check to look for set instead of series (besides array)
2009-04-16	JS	->loadfileds can now load field values from -params also inside an inline
2009-03-20	JS	Added  <![CDATA[ ... ]]>  around injected scripts for better xhtml compliance
2009-01-08	JS	->getvalue and _unknowntag: added -index parameter to be able to get value for a specific field instance when there are multiple fields with the same name 
2009-01-08	JS	->loadfields: implemented support for multiple fields with the same name when loading field values from form submission where the number of same name fields matches
2009-01-07	JS	->setvalue: added -index parameter to be able to set value for a specific field instance when there are multiple fields with the same name 
2008-12-08	JS	->renderform: Removed the onclick handlers for checkbox and radio since Safari now supports clicking the label text as click for the checkbox/radio control. 
2008-12-05	JS	->renderform: the fieldset and legend field types will now use id and class on the fieldset tag if specified
2008-12-03	JS	->renderform: fields of type fieldset now uses value as legend (just as field type legend already did) instead of always using an empty legend
2008-09-24	JS	->updatefields: Added protection against backtick sql injection in MySQL object names
2008-09-17	JS	->renderform and ->renderhtml: -from and -to allows negative numbers to count from end of form instead
2008-09-13	JS	Added ->getlabel to return the display name for a field. 
2008-09-13	JS	->addfield and ->validate: Implemented -validate to specify a compound expression to validate the field input. 
2008-09-13	JS	->addfield and ->loadfields: Implemented -filter to specify a compound expression to filter the field input. 
2008-09-11	JS	->updatefields: fixed exclusion of special field types html, legend and fieldset. 
2008-09-11	JS	->renderform: Fixed missing value for password fields
2008-07-02	JS	->renderform: Cleaned up the automatic adding of javascript code so it\'s not added if not needed. Also moved all scripts to the end of the page. More work with with the javascripts is needed.
2008-06-03	JS	->renderform: corrected missing closing </fieldset>
2008-05-15	JS	->renderform and ->renderhtml: adjusted the behavior for nested fieldsets
2008-05-13	JS	Implemented -legend for ->renderhtml, to make it consistent with the new legend field type
2008-05-13	JS	Implemented special field types html, fieldset and legend. Use -value to display data for these fields. A legend field also creates a fieldset (closes any previously open fieldsets). Use fieldset with -value=false to close a fieldset without opening a new one. 
2008-05-06	JS	Added unknowntag as shortcut to getvalue
2008-01-30	JS	Removed duplicate endscript entries for if(dirty) {makedirty()};
2007-12-13	JS	Corrected ->addfield: -dbfield so empty dbfields are properly ignored by ->updatefields. 
2007-12-11	JS	Moved error_msg to knop_base (special version of error_code stays here) 
2007-12-11	JS	Added documentation as -description to most member tags, to be used by the new ->help tag
2007-12-11	JS	Moved ->help to knop_base
2007-11-13	JS	Added -buttontemplate to be able to specify separate template for buttons, defaults to no <br>, but if template has been specified that will be used instead (for backwards compatibility)
2007-11-12	JS	->process delete now works also when not using record locking (not specifying -user)
2007-11-01	JS	->renderform: added support for -hint for textarea fields.
2007-09-27	JS	->renderhtml: multiple values (array) for radio, checkbox and select are now rendered properly with either "," or <br> depending on the presence of -linebreak, and with the display text instead of the actual option value
2007-09-27	JS	->renderform: improved handling of multiple values for checkbox, radio and select
2007-09-21	JS	->addfield: flag parameters now accept false as value
2007-09-06	JS	->oncreate: changed name of -action to -formaction to make it more clear what it is. -action is still supported but deprecated.
2007-09-06	JS	->renderform: Corrected the exception for -session... (duh)
2007-08-08	JS	->renderform: Added exception for -session
2007-06-18	JS	Added tag timer to most member tags
2007-06-13	JS	added inheritance from knop_base
2007-06-12	JC	bugfixed -xhtml form rendering when called by quicksearch
2007-06-11	JC	added handling of xhtml output
2007-04-19	JS	->loadfields: fixed -params that was broken when adding -database
2007-04-19	JS	->renderform: removed invalid wrap="soft" from textarea
2007-04-12	JS	->process: made -user optional (only needed when using record locking)
2007-04-12	JS ->loadfields can now take a -database parameter, either as a flag (no value) where the database object connected to the form will be used, or by specifying a database object as value. 
2007-04-03	JS	Changed namespace from mt_ to knop_
2007-03-01	JS	->renderform fixed unsavedwarning on page load by moving checkdirty() to afterscript
2007-03-01	JS	->formmode and ->init changed so it preserves the right mode after a failed add
2007-02-27	JS	->renderform: added <div class="inputgroup"> around checkboxes and radios for css formating
2007-02-26	JS	->oncreate: added -actionpath to specify the framework action path for the form instead of manually adding the -action hidden field
2007-02-24	JS	Corrected entersubmitblock behavior by adding onfocus handler on form and starting with submitBlock=false
2007-02-23	JS	Removed encode_html from form field labels
2007-02-22	JS	->setformat: Added -legend
2007-02-07	JS	Added ->copyfield to copy a form field to a new name, with the same properties. 
2007-02-07	JS	->errors now returns empty array if validate has not been called, instead of performing validation
2007-02-05	JS	->getbutton can now look for also button names that are not one of the built-in ones (for example button_apply)
2007-02-05	JS The -keyvalue parameter can be given another name by specifying -keyparamname in oncreate
2007-02-02	JS	Added ->lockvalue_decrypted
2007-02-02	JS	->addfield: -value is now stored as reference
2007-02-02	JS	error_code now returns an error for when the form contains validation errors
2007-02-02	JS	Improved reporting of Lasso error messaged in error_msg
2007-02-02	JS 	Added real error codes
2007-01-31	JS	->rederform action_params now also exclude "-" params that appear in the form action
2007-01-29	JS	->renderform: The first field with input error will get focus when loading page
2007-01-29	JS	Added -focus to ->addfield to give default field focus when loading page with form
2007-01-29	JS	Added -disabled to ->addfield, and handling of it in ->renderform
2007-01-29	JS	Added -noautoparams to ->oncreate to disable the automatic passing of action_params that begin with "-"
2007-01-29	JS	->renderform now renders label also for submit, reset to format properly with css
2007-01-26	JS	Added support for Safari specific <input type="search">
2007-01-26	JS	->renderform action_params that begin with "-" now exclude params that exist in the form. Minor corrections to the behavior. 
2007-01-25	JS	Added -nowarning to ->oncreate to disable unsaved warnings for the entire form
2007-01-25	JS	Added -required to ->oncreate (and a few more from ->setformat)
2007-01-23	JS	Autogenerates id for the form itself
2007-01-23	JS	Added ->getbutton to return the button that was clicked when submitting a form (cancel, add, save, delete)
2007-01-23	JS	Added auto conversion of options left hand pair member to string, to make comparsions work reliably. Integer zeros don\'t compare nicely to strings. 
2007-01-23	JS	Added support for submit-on-enter prevention: specify -entersubmitblock at oncreate
2007-01-19	JS	Addes renderform: -legend to be able to group form fields at render time
2007-01-19	JS	added support for -optgroup in -options for select. Also works for radio and checkbox. Specify empty -optgroup to close optgroup in select without starting a new, or to add extra linebreak between checkboxes/radio buttons. 
2007-01-19	JS	added -template for oncreate
2007-01-19	JS	added optional fieldset and legend to form, legend can be specified as -legend at oncreate. if -legend is specified, the form will be wrapped in a fieldset. 
2007-01-19	JS	method now defaults to post
2007-01-19	JS	Corrected line separator for FileMaker checkboxes and added the same handling also for radio
2007-01-18	JS	renderform: any action_params that begin with "-" (except -keyvalue and -lockvalue) are added as form parameters
2007-01-18	JS	renderform: checkboxes and multiselects now show checked and selected properly when loading values from database
2007-01-18	JS	updatefields: added support for multiple values for one fieldname, like checkboxes (multiple fields in the update pair array, -sql generates comma separated values)
2007-01-17	JS	reset button now makes form undirty
2007-01-17	JS	addfield: -confirmmessage can now be specified for any submit or reset button
2007-01-17	JS	added addfield: -nowarning to avoid unsaved warning when the field is changed
2007-01-17	JS	changed default class name for unsaved marker from dirty to unsaved
2007-01-17	JS	changed name of -dirtymarker and -dirtymarkerclass to unsavedmarker and -unsavedmarkerclass for userfriendlyness
2007-01-17	JS	added setformat: -unsavedwarning to dynamically set the javascript form dirty warning message
2007-01-17	JS	renderform: -field changed to renderform: -name for consistency
2007-01-16	JS	renderform: -field with wrong field name does not output anything, instead of the entire form
2007-01-16	JS	fixed onbeforeunload in javascript form dirty handler

TODO:
->addfield: Add -format to manipulate the field value before it is displayed by ->renderform and ->renderhtml, much like -filter but only for display and without affecting input. 
->addfield: Add -fieldgroup to be able to group related fields together, useful for ->updatefields to return just fields that belong to a specific db table, or ->renderform as another way to render a form selectively
->renderform needs a better way to display errors inline together with the fields
Make _unknowntag also work as shortcut to setvalue if a value is specified
Add a new special field type to the form object, let\'s say "data". That field type will not interact with forms and will never be touched by loadfields, but it will populate ->updatefields.
Add -> searchfields, which will return a fulltext enabled pair array better suited for searchs than ->updatefields is. -fulltext needs to be specified per field. 
Review and clean up the javascripts inserted automatically by knop_form - partially done
Option to let textarea grow automatically depending on the amount of text in it.  
Use http://bassistance.de/jquery-plugins/jquery-plugin-validation/ instead of client side validation
Possibly add support for the same validation expressions as the jquery validation plugin uses, so server side a nd client side validation can be specified at once. 
Add -path as parameter for oncreate so the form action can be set with less confusion...  In that case -formaction will be a physical url, while -path would be a framework path. 
Fix actionpath reference so it updates properly when altering the value (not possible?)
Should loadfields load "-" params?
Unsavedwarning made optional, does not seem to work properly now?
More flexible error hightlighting
Move templates to a member tag to be make it easier to subclass (Douglas Burchard)
Add "button". <button></button>. Subtypes are submit, reset and button. How to specify the subtype? (Douglas Burchard)
Change ->addfield to ->insert and make ->addfield deprecated
There is no src for input type image!
Add ->size and ->get so the form object can be iterated
Add -skipemtpy to to ->renderhtml
Option for -> renderhtml to output without html encoding
->renderhtml should never html encode fields of type html

',));
		if(local_defined('type'));return(#changenotes -> find(#type));else;
		!local_defined('date') ? local('date'=date('1900-01-01')) | #date = date(#date);
		iterate(#changenotes, local('changenote'));
			#output += #changenote -> name + '\n';
			iterate(#changenote ->value -> split('\n'), local('changenote_row'));
				if(date(#changenote_row -> split(regexp('\\s')) -> first) >= #date);
					#output += #changenote_row + '\n';
				/if;
			/iterate;
			#output += '\n';
		/iterate;
		return(@#output);/if;
		/define_tag]