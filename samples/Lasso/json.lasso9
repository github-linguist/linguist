
/**
	trait_json_serialize
	Objects with this trait will be assumed to convert to json data
	when its ->asString method is called
*/
define trait_json_serialize => trait {
	require asString()
}

define json_serialize(e::bytes)::string => ('"' + (string(#e)->Replace(`\`, `\\`) & Replace('\"', '\\"') & Replace('\r', '\\r') & Replace('\n', '\\n') & Replace('\t', '\\t') & Replace('\f', '\\f') & Replace('\b', '\\b') &) + '"')
define json_serialize(e::string)::string => ('"' + (string(#e)->Replace(`\`, `\\`) & Replace('\"', '\\"') & Replace('\r', '\\r') & Replace('\n', '\\n') & Replace('\t', '\\t') & Replace('\f', '\\f') & Replace('\b', '\\b') &) + '"')
define json_serialize(e::json_literal)::string => (#e->asstring)
define json_serialize(e::integer)::string => (#e->asstring)
define json_serialize(e::decimal)::string => (#e->asstring)
define json_serialize(e::boolean)::string => (#e->asstring)
define json_serialize(e::null)::string => ('null')
define json_serialize(e::date)::string => ('"' + #e->format(#e->gmt ? '%QT%TZ' | '%Q%T') + '"')
/*
define json_serialize(e::array)::string => {
	local(output) = '';
	local(delimit) = '';
	#e->foreach => { #output += #delimit + json_serialize(#1); #delimit = ', '; }
	return('[' + #output + ']');
}
define json_serialize(e::staticarray)::string => {
	local(output) = '';
	local(delimit) = '';
	#e->foreach => { #output += #delimit + json_serialize(#1); #delimit = ', '; }
	return('[' + #output + ']');
}
*/
define json_serialize(e::trait_forEach)::string => {
	local(output) = '';
	local(delimit) = '';
	#e->foreach => { #output += #delimit + json_serialize(#1); #delimit = ', '; }
	return('[' + #output + ']');
}
define json_serialize(e::map)::string => {
	local(output = with pr in #e->eachPair 
					select json_serialize(#pr->first->asString) + ': ' + json_serialize(#pr->second))
	return '{' + #output->join(',') + '}'
}
define json_serialize(e::json_object)::string => {
	local(output) = '';
	local(delimit) = '';
	#e->foreachpair => { #output += #delimit + #1->first + ': ' + json_serialize(#1->second); #delimit = ', '; }
	return('{' + #output + '}');
}
define json_serialize(e::trait_json_serialize) => #e->asString
define json_serialize(e::any)::string => json_serialize('<LassoNativeType>' + #e->serialize + '</LassoNativeType>')

// Bil Corry fixes for decoding json
define json_consume_string(ibytes::bytes) => {
	local(obytes) = bytes;
	local(temp) = 0;
	while((#temp := #ibytes->export8bits) != 34);
		#obytes->import8bits(#temp);
		(#temp == 92) ? #obytes->import8bits(#ibytes->export8bits); // Escape \
 	/while;
	local(output = string(#obytes)->unescape)
	//Replace('\\"', '\"') & Replace('\\r', '\r') & Replace('\\n', '\n') & Replace('\\t', '\t') & Replace('\\f', '\f') & Replace('\\b', '\b') &;
	if(#output->BeginsWith('<LassoNativeType>') && #output->EndsWith('</LassoNativeType>'));
		Protect;
			return serialization_reader(xml(#output - '<LassoNativeType>' - '</LassoNativeType>'))->read
		/Protect;
	else( (#output->size == 16 or #output->size == 15) and regexp(`\d{8}T\d{6}Z?`, '', #output)->matches)
		return date(#output, -Format=#output->size == 16?`yyyyMMdd'T'HHmmssZ`|`yyyyMMdd'T'HHmmss`)
	/if
	return #output
}

// Bil Corry fix + Ke fix
define json_consume_token(ibytes::bytes, temp::integer) => {

	local(obytes = bytes->import8bits(#temp) &,
		delimit = array(9, 10, 13, 32, 44, 58, 93, 125)) // \t\r\n ,:]}

	while(#delimit !>> (#temp := #ibytes->export8bits))
		#obytes->import8bits(#temp)
	/while

	#temp == 125? // }
		#ibytes->marker -= 1
//============================================================================
//	Is also end of token if end of array[]
	#temp == 93? // ]
		#ibytes->marker -= 1
//............................................................................		

	local(output = string(#obytes))
	#output == 'true'?
		return true
	#output == 'false'?
		return false
	#output == 'null'?
		return null
	string_IsNumeric(#output)?
	return (#output >> '.')? decimal(#output) | integer(#output)

	return #output
}

// Bil Corry fix
define json_consume_array(ibytes::bytes)::array => {
	Local(output) = array;
	local(delimit) = array( 9, 10, 13, 32, 44); // \t\r\n ,
	local(temp) = 0;
	While((#temp := #ibytes->export8bits) != 93); // ]
		If(#delimit >> #temp);
			// Discard whitespace
		Else(#temp == 34); // "
			#output->insert(json_consume_string(#ibytes));
		Else(#temp == 91); // [
			#output->insert(json_consume_array(#ibytes));
		Else(#temp == 123); // {
			#output->insert(json_consume_object(#ibytes));
		Else;
			#output->insert(json_consume_token(#ibytes, #temp));
			(#temp == 93) ? Loop_Abort;
		/If;
	/While;
	Return(#output);
}

// Bil Corry fix
define json_consume_object(ibytes::bytes)::map => {
	Local('output' = map,
		'delimit' = array( 9, 10, 13, 32, 44), // \t\r\n ,
		'temp' = 0,
		'key' = null,
		'val' = null);
	While((#temp := #ibytes->export8bits) != 125); // }
		If(#delimit >> #temp);
			// Discard whitespace
		Else((#key !== null) && (#temp == 34)); // "
			#output->insert(#key = json_consume_string(#ibytes));
			#key = null;
		Else((#key !== null) && (#temp == 91)); // [
			#output->insert(#key = json_consume_array(#ibytes));
			#key = null;
		Else((#key !== null) && (#temp == 123)); // {
			#output->insert(#key = json_consume_object(#ibytes));
			#key = null;
		Else((#key !== null));
			#output->insert(#key = json_consume_token(#ibytes, #temp));
			#key = null;
		Else;
			#key = json_consume_string(#ibytes);
			while(#delimit >> (#temp := #ibytes->export8bits));
			/while;
			#temp != 58 ? Loop_Abort;
		/If;
	/While;

	If((#output >> '__jsonclass__') && (#output->Find('__jsonclass__')->isa('array')) && (#output->Find('__jsonclass__')->size >= 2) && (#output->Find('__jsonclass__')->First == 'deserialize'));
		Return(#output->find('__jsonclass__')->Second->First);
	Else((#output >> 'native') && (#output >> 'comment') && (#output->find('comment') == 'http://www.lassosoft.com/json'));
		Return(#output->find('native'));
	/If;
	Return(#output);
}

// Bil Corry fix + Ke fix
define json_deserialize(ibytes::bytes)::any => {
	#ibytes->removeLeading(bom_utf8);

//============================================================================
//	Reset marker on provided bytes
	#ibytes->marker = 0
//............................................................................		
	
	Local(temp) = #ibytes->export8bits;
	If(#temp == 91); // [
		Return(json_consume_array(#ibytes));
	Else(#temp == 123); // {
		Return(json_consume_object(#ibytes));
	else(#temp == 34) // "
		return json_consume_string(#ibytes)
	/If;
}

define json_deserialize(s::string) => json_deserialize(bytes(#s))

/**! json_literal - This is a subclass of String used for JSON encoding.

	A json_literal works exactly like a string, but will be inserted directly
	rather than being encoded into JSON. This allows JavaScript elements
	like functions to be inserted into JSON objects. This is most useful
	when the JSON object will be used within a JavaScript on the local page.
	[Map: 'fn'=Literal('function(){ ...})] => {'fn': function(){ ...}}
**/
define json_literal => type {
	parent string
}

/**! json_object - This is a subclass of Map used for JSON encoding.

	An object works exactly like a map, but when it is encoded into JSON all
	of the keys will be inserted literally. This makes it easy to create a
	JavaScript object without extraneous quote marks.
	Object('name'='value') => {name: "value"}
**/
define json_object => type {
	parent map
	public onCreate(...) => ..onCreate(:#rest or (:))
}

define json_rpccall(method::string, params=map, id='', host='') => {
	#id == '' ? #host = Lasso_UniqueID;
	#host == '' ? #host = 'http://localhost/lassoapps.8/rpc/rpc.lasso';
	Return(Decode_JSON(Include_URL(#host, -PostParams=Encode_JSON(Map('method' = #method, 'params' = #params, 'id' = #id)))));
}
