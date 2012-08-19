<?LassoScript

	//
	// JSON Encoding and Decoding
	//
	// Copyright 2007-2012 LassoSoft Inc.
	//
	// <http://json.org/>
	// <http://json-rpc.org/>
	// <http://www.ietf.org/rfc/rfc4627.txt?number=4627>
	//
	
If: (Lasso_TagExists: 'Encode_JSON') == False;

	Define_Tag: 'JSON', -Namespace='Encode_', -Required='value', -Optional='options';
	
		Local: 'escapes' = Map('\\' = '\\', '"'  = '"', '\r' = 'r', '\n' = 'n', '\t' = 't', '\f' = 'f', '\b' = 'b');
		Local: 'output' = '';
		Local: 'newoptions' = (Array: -Internal);
		If: !(Local_Defined: 'options') || (#options->(IsA: 'array') == False);
			Local: 'options' = (Array);
		/If;
		If: (#options >> -UseNative) || (Params >> -UseNative);
			#newoptions->(Insert: -UseNative);
		/If;
		If: (#options >> -NoNative) || (Params >> -NoNative);
			#newoptions->(Insert: -NoNative);
		/If;
		If: (#options !>> -UseNative) && ((#value->(IsA: 'set')) || (#value->(IsA: 'list')) || (#value->(IsA: 'queue')) || (#value->(IsA: 'priorityqueue')) || (#value->(IsA: 'stack')));
			#output += (Encode_JSON: Array->(insertfrom: #value->iterator) &, -Options=#newoptions);
		Else: (#options !>> -UseNative) && (#value->(IsA: 'pair'));
			#output += (Encode_JSON: (Array: #value->First, #value->Second));
		Else: (#options !>> -Internal) && (#value->(Isa: 'array') == False) && (#value->(IsA: 'map') == False);
			#output += '[' + (Encode_JSON: #value, -Options=#newoptions) + ']';
		Else: (#value->(IsA: 'literal'));
			#output += #value;
		Else: (#value->(IsA: 'string'));
			#output += '"';
			Loop: (#value->Length);
				Local('character' = #value->(Get: Loop_Count));
				#output->(Append:
					(Match_RegExp('[\\x{0020}-\\x{21}\\x{23}-\\x{5b}\\x{5d}-\\x{10fff}]') == #character) ? #character |
						'\\' + (#escapes->(Contains: #character) ? #escapes->(Find: #character) | 'u' + String(Encode_Hex(#character))->PadLeading(4, '0')&)
				);
			/Loop;
			#output += '"';
		Else: (#value->(IsA: 'integer')) || (#value->(IsA: 'decimal')) || (#value->(IsA: 'boolean'));
			#output += (String: #value);
		Else: (#value->(IsA: 'null'));
			#output += 'null';
		Else: (#value->(IsA: 'date'));
			If: #value->gmt;
				#output += '"' + #value->(format: '%QT%TZ') + '"';
			Else;
				#output += '"' + #value->(format: '%QT%T') + '"';
			/If;
		Else: (#value->(IsA: 'array'));
			#output += '[';
			Iterate: #value, (Local: 'temp');
				#output += (Encode_JSON: #temp, -Options=#newoptions);
				If: #value->Size != Loop_Count;
					#output += ', ';
				/If;
			/Iterate;
			#output += ']';
		Else: (#value->(IsA: 'object'));
			#output += '{';
			Iterate: #value, (Local: 'temp');
				#output += #temp->First + ': ' + (Encode_JSON: #temp->Second, -Options=#newoptions);
				If: (#value->Size != Loop_Count);
					#output += ', ';
				/If;
			/Iterate;
			#output += '}';
		Else: (#value->(IsA: 'map'));
			#output += '{';
			Iterate: #value, (Local: 'temp');
				#output += (Encode_JSON: #temp->First, -Options=#newoptions) + ': ' + (Encode_JSON: #temp->Second, -Options=#newoptions);
				If: (#value->Size != Loop_Count);
					#output += ', ';
				/If;
			/Iterate;
			#output += '}';
		Else: (#value->(IsA: 'client_ip')) || (#value->(IsA: 'client_address'));
			#output += (Encode_JSON: (String: #value), -Options=#newoptions);
		Else: (#options !>> -UseNative) && (#value->(IsA: 'set')) || (#value->(IsA: 'list')) || (#value->(IsA: 'queue')) || (#value->(IsA: 'priorityqueue')) || (#value->(IsA: 'stack'));
			#output += (Encode_JSON: Array->(insertfrom: #value->iterator) &, -Options=#newoptions);
		Else: (#options !>> -NoNative);
			#output += (Encode_JSON: (Map: '__jsonclass__'=(Array:'deserialize',(Array:'<LassoNativeType>' + #value->Serialize + '</LassoNativeType>'))));
		/If;
		Return: @#output;
		
	/Define_Tag;

/If;

If: (Lasso_TagExists: 'Decode_JSON') == False;

	Define_Tag: 'JSON', -Namespace='Decode_', -Required='value';

		(#value == '') ? Return: Null;
		
		Define_Tag: 'consume_string', -Required='ibytes';
			Local: 'unescapes' = (map: 34 = '"', 92 = '\\', 98 = '\b', 102 = '\f', 110 = '\n', 114 = '\r', 116 = '\t');
			Local: 'temp' = 0, 'obytes' = Bytes;
			While: ((#temp := #ibytes->export8bits) != 34); // '"'
				If: (#temp === 92); // '\'
					#temp = #ibytes->export8bits;
					If: (#temp === 117); // 'u'
						#obytes->(ImportString: (Decode_Hex: (String: #ibytes->(GetRange: #ibytes->Position + 1, 4)))->(ExportString: 'UTF-16'), 'UTF-8');
						#ibytes->(SetPosition: #ibytes->Position + 4);
					Else;
						If: (#unescapes->(Contains: #temp));
							#obytes->(ImportString: #unescapes->(Find: #temp), 'UTF-8');
						Else;
							#obytes->(Import8Bits: #temp);
						/If;
					/If;
				Else;
					#obytes->(Import8Bits: #temp);
				/If;
			/While;
			Local('output' = #obytes->(ExportString: 'UTF-8'));
			If: #output->(BeginsWith: '<LassoNativeType>') && #output->(EndsWith: '</LassoNativeType>');
				Local: 'temp' = #output - '<LassoNativeType>' - '</LassoNativeType>';
				Local: 'output' = null;
				Protect;
					#output->(Deserialize: #temp);
				/Protect;
			Else: (Valid_Date: #output, -Format='%QT%TZ');
				Local: 'output' = (Date: #output, -Format='%QT%TZ');
			Else: (Valid_Date: #output, -Format='%QT%T');
				Local: 'output' = (Date: #output, -Format='%QT%T');
			/If;			
			Return: @#output;
		/Define_Tag;

		Define_Tag: 'consume_token', -Required='ibytes', -required='temp';
			Local: 'obytes' = bytes->(import8bits: #temp) &;
			local: 'delimit' = (array: 9, 10, 13, 32, 44, 58, 93, 125); // \t\r\n ,:]}
			While: (#delimit !>> (#temp := #ibytes->export8bits));
				#obytes->(import8bits: #temp);
			/While;
			Local: 'output' = (String: #obytes);
			If: (#output == 'true') || (#output == 'false');
				Return: (Boolean: #output);
			Else: (#output == 'null');
				Return: Null;
			Else: (String_IsNumeric: #output);
				Return: (#output >> '.') ? (Decimal: #output) | (Integer: #output);
			/If;
			Return: @#output;
		/Define_Tag;

		Define_Tag: 'consume_array', -Required='ibytes';
			Local: 'output' = array;
			local: 'delimit' = (array:  9, 10, 13, 32, 44); // \t\r\n ,
			local: 'temp' = 0;
			While: ((#temp := #ibytes->export8bits) != 93); // ]
				If: (#delimit >> #temp);
					// Discard whitespace 
				Else: (#temp == 34); // "
					#output->(insert: (consume_string: @#ibytes));
				Else: (#temp == 91); // [
					#output->(insert: (consume_array: @#ibytes));
				Else: (#temp == 123); // {
					#output->(insert: (consume_object: @#ibytes));
				Else;
					#output->(insert: (consume_token: @#ibytes, @#temp));
					(#temp == 93) ? Loop_Abort;
				/If;
			/While;
			Return: @#output;
		/Define_Tag;

		Define_Tag: 'consume_object', -Required='ibytes';
			Local: 'output' = map;
			local: 'delimit' = (array:  9, 10, 13, 32, 44); // \t\r\n ,
			local: 'temp' = 0;
			local: 'key' = null;
			local: 'val' = null;
			While: ((#temp := #ibytes->export8bits) != 125); // }
				If: (#delimit >> #temp);
					// Discard whitespace 
				Else: (#key !== null) && (#temp == 34); // "
					#output->(insert: #key = (consume_string: @#ibytes));
					#key = null;
				Else: (#key !== null) && (#temp == 91); // [
					#output->(insert: #key = (consume_array: @#ibytes));
					#key = null;
				Else: (#key !== null) && (#temp == 123); // {
					#output->(insert: #key = (consume_object: @#ibytes));
					#key = null;
				Else: (#key !== null);
					#output->(insert: #key = (consume_token: @#ibytes, @#temp));
					(#temp == 125) ? Loop_abort;
					#key = null;
				Else;
					#key = (consume_string: @#ibytes);
 				    while(#delimit >> (#temp := #ibytes->export8bits));
					/while;
  					#temp != 58 ? Loop_Abort;
				/If;
			/While;
			If: (#output >> '__jsonclass__') && (#output->(Find: '__jsonclass__')->(isa: 'array')) && (#output->(Find: '__jsonclass__')->size >= 2) && (#output->(Find: '__jsonclass__')->First == 'deserialize');
				Return: #output->(find: '__jsonclass__')->Second->First;
			Else: (#output >> 'native') && (#output >> 'comment') && (#output->(find: 'comment') == 'http://www.lassosoft.com/json');
				Return: #output->(find: 'native');
			/If;
			Return: @#output;
		/Define_Tag;
		
		Local: 'ibytes' = (bytes: #value);
		Local: 'start' = 1;
 	  	#ibytes->removeLeading(BOM_UTF8);
		Local: 'temp' = #ibytes->export8bits;
		If: (#temp == 91); // [
			Local: 'output' = (consume_array: @#ibytes);
			Return: @#output;
		Else: (#temp == 123); // {
			Local: 'output' = (consume_object: @#ibytes);
			Return: @#output;
		/If;
		
	/Define_Tag;

/If;
	
If: (Lasso_TagExists: 'Literal') == False;

	Define_Type: 'Literal', 'String';
	/Define_Type;

/If;
	
If: (Lasso_TagExists: 'Object') == False;
	
	Define_Type: 'Object', 'Map';
	/Define_Type;
	
/If;

If: (Lasso_TagExists: 'JSON_RPCCall') == False;
	
	Define_Tag: 'RPCCall', -Namespace='JSON_',
			-Required='method',
			-Optional='params',
			-Optional='id',
			-Optional='host';

		!(Local_Defined: 'host') ? Local: 'host' = 'http://localhost/lassoapps.8/rpc/rpc.lasso';
		!(Local_Defined: 'id') ? Local: 'id' = Lasso_UniqueID;
		Local: 'request' = (Map: 'method' = #method, 'params' = #params, 'id' = #id);
		Local: 'request' = (Encode_JSON: #request);
		Local: 'result' = (Include_URL: #host, -PostParams=#request);
		Local: 'result' = (Decode_JSON: #result);
		Return: @#result;

	/Define_Tag;
	
/If;

If: (Lasso_TagExists: 'JSON_Records') == False;

	Define_Tag: 'JSON_Records',
			-Optional='KeyField',
			-Optional='ReturnField',
			-Optional='ExcludeField',
			-Optional='Fields';

		Local: '_fields' = (Local_Defined: 'fields') && #fields->(IsA: 'array') ? #fields | Field_Names;
		Fail_If: #_fields->size == 0, -1, 'No fields found for [JSON_Records]';
		Local: '_keyfield' = (Local: 'keyfield');
		If: #_fields !>> #_keyfield;
			Local: '_keyfield' = (KeyField_Name);
			If: #_fields !>> #_keyfield;
				Local: '_keyfield' = 'ID';
				If: #_fields !>> #_keyfield;
					Local: '_keyfield' = #_fields->First;
				/If;
			/If;
		/If;
		Local: '_index' = #_fields->(FindPosition: #_keyfield)->First;
		Local: '_return' = (Local_Defined: 'returnfield') ? (Params->(Find: -ReturnField)->(ForEach: {Params->First = Params->First->Second; Return: True}) &) | @#_fields;
		Local: '_exclude' = (Local_Defined: 'excludefield') ? (Params->(Find: -ExcludeField)->(ForEach: {Params->First = Params->First->Second; Return: True}) &) | Array;
		Local: '_records' =  Array;
		Iterate: Records_Array, (Local: '_record');
			Local: '_temp' = Map;
			Iterate: #_fields, (Local: '_field');
				((#_return >> #_field) && (#_exclude !>> #_field)) ? #_temp->Insert(#_field = #_record->(Get: Loop_Count));
			/Iterate;
			#_records->Insert(#_temp);
		/Iterate;
		Local: '_output' = (Encode_JSON: (Object: 'error_msg'=Error_Msg, 'error_code'=Error_Code, 'found_count'=Found_Count, 'keyfield'=#_keyfield, 'rows'=#_records));
		Return: @#_output;

	/Define_Tag;

/If;

?>
