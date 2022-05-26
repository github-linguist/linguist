note
	description: "Summary description for {APP_EMBEDDED_WEB_EXECUTION}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	APP_EMBEDDED_WEB_EXECUTION

inherit
	EMBEDDED_WEB_EXECUTION
		redefine
			initialize
		end

create
	make

feature {NONE} -- Initialization

	initialize
		do
			Precursor
			create request_exit_operation_actions
			local_connection_restriction_enabled := True
		end

feature -- Execution

	request_exit_operation_actions: ACTION_SEQUENCE [TUPLE]

	execute
			-- Execute the request
			-- See `request.input' for input stream
    		--     `request.meta_variables' for the CGI meta variable
			-- and `response' for output buffer
		local
			router: WSF_ROUTER
			sess: detachable WSF_ROUTER_SESSION
			m: WSF_HTML_PAGE_RESPONSE
			b: STRING
			fs: WSF_FILE_SYSTEM_HANDLER
			req: like request
		do
			req := request

			create router.make (3)
			router.handle ("/test/{var}", create {WSF_URI_TEMPLATE_AGENT_HANDLER}.make (agent handle_test), Void)
			router.handle ("/env", create {WSF_URI_AGENT_HANDLER}.make (agent handle_env), Void)
			router.handle ("/exit", create {WSF_URI_TEMPLATE_AGENT_HANDLER}.make (agent handle_exit), Void)
			create fs.make_with_path ((create {EXECUTION_ENVIRONMENT}).current_working_path.extended ("files"))
			router.handle ("/files", fs, Void)
			create sess
			router.dispatch (req, response, sess)
			if not sess.dispatched then
				create m.make
				create b.make_from_string ("<h1>Hello Eiffel desktop user</h1>")
				b.append ("<li><a href=%"" + req.script_url ("/test/start") + "%">test</a></li>")
				b.append ("<li><a href=%"" + req.script_url ("/env") + "%">env</a></li>")
				b.append ("<li><a href=%"" + req.script_url ("/files") + "%">files</a></li>")
				b.append ("<li><a href=%"" + req.script_url ("/exit") + "%">exit</a></li>")
				m.set_body (b)
				response.send (m)
			end
		end

	handle_test (req: WSF_REQUEST; res: WSF_RESPONSE)
		local
			m: WSF_HTML_PAGE_RESPONSE
			b: STRING
			l_name: READABLE_STRING_32
		do
			if attached {WSF_STRING} req.item ("var") as p_name then
				l_name := p_name.value
			else
				l_name := {STRING_32} "Embedded web service and web_browser in vision2 application"
			end
			create m.make
			create b.make_from_string ("<h1>This is a test about "+ m.html_encoded_string (l_name) +"</h1>")
			b.append ("<li><a href=%"" + req.script_url ("/") + "%">back to home</a></li>")
			if l_name.is_case_insensitive_equal_general ("start") then
				b.append ("<li><a href=%"" + req.script_url ("/test/js") + "%">test javascript+ajax</a></li>")
			elseif l_name.is_case_insensitive_equal_general ("js") then
				b.append ("[
					<div id="myDiv"><h2>Let AJAX change this text</h2>
					<button type="button" onclick="loadXMLDoc()">Change Content</button>
					</div>
				]")
				m.add_javascript_content ("[
					function loadXMLDoc()
					{
					var xmlhttp;
					if (window.XMLHttpRequest)
					  {// code for IE7+, Firefox, Chrome, Opera, Safari
					  xmlhttp=new XMLHttpRequest();
					  }
					else
					  {// code for IE6, IE5
					  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
					  }
					xmlhttp.onreadystatechange=function()
					  {
					  if (xmlhttp.readyState==4 && xmlhttp.status==200)
					    {
					    document.getElementById("myDiv").innerHTML=xmlhttp.responseText;
					    }
					  }
					xmlhttp.open("GET","/test/ajax.txt",true);
					xmlhttp.send();
					}
				]")
			elseif l_name.is_case_insensitive_equal_general ("ajax.txt") then
				b := "This is AJAX response ... from " + req.absolute_script_url ("")
			end
			m.set_body (b)
			res.send (m)
		end

	handle_env (req: WSF_REQUEST; res: WSF_RESPONSE)
		local
			s: STRING_8
			p: WSF_PAGE_RESPONSE
			v: STRING_8
		do
			create s.make (2048)
			s.append ("**DEBUG**%N")
			req.set_raw_input_data_recorded (True)

			append_iterable_to ("Meta variables:", req.meta_variables, s)
			s.append_character ('%N')

			append_iterable_to ("Path parameters", req.path_parameters, s)
			s.append_character ('%N')

			append_iterable_to ("Query parameters", req.query_parameters, s)
			s.append_character ('%N')

			append_iterable_to ("Form parameters", req.form_parameters, s)
			s.append_character ('%N')

			if attached req.content_type as l_type then
				s.append ("Content: type=" + l_type.debug_output)
				s.append (" length=")
				s.append_natural_64 (req.content_length_value)
				s.append_character ('%N')
				create v.make (req.content_length_value.to_integer_32)
				req.read_input_data_into (v)
				across
					v.split ('%N') as v_cursor
				loop
					s.append ("     |")
					s.append (v_cursor.item)
					s.append_character ('%N')
				end
			end

			create p.make_with_body (s)
			p.header.put_content_type_text_plain
			res.send (p)
		end

	handle_exit (req: WSF_REQUEST; res: WSF_RESPONSE)
		local
			m: WSF_HTML_PAGE_RESPONSE
			b: STRING
		do
			create m.make
			create b.make_from_string ("<h1>Embedded server is about to shutdown</h1>")
			b.append ("<li><a href=%"" + req.script_url ("/") + "%">back to home</a></li>")
			b.append ("<li><a href=%"" + req.script_url ("/bye") + "%">Click to confirm exit operation</a></li>")
			m.set_body (b)
			res.send (m)
			if attached {separate WGI_STANDALONE_CONNECTOR [WGI_EXECUTION]} req.wgi_connector as conn then
				shutdown_server (conn)
			end
			request_exit_operation_actions.call (Void)
		end

	shutdown_server (conn: separate WGI_STANDALONE_CONNECTOR [WGI_EXECUTION])
		do
			conn.shutdown_server
		end

feature {NONE} -- Implementation

	append_iterable_to (a_title: READABLE_STRING_8; it: detachable ITERABLE [WSF_VALUE]; s: STRING_8)
		local
			n: INTEGER
			t: READABLE_STRING_8
			v: READABLE_STRING_8
		do
			s.append (a_title)
			s.append_character (':')
			if it /= Void then
				across it as c loop
					n := n + 1
				end
				if n = 0 then
					s.append (" empty")
					s.append_character ('%N')
				else
					s.append_character ('%N')
					across
						it as c
					loop
						s.append ("  - ")
						s.append (c.item.url_encoded_name)
						t := c.item.generating_type.name
						if t.same_string ("WSF_STRING") then
						else
							s.append_character (' ')
							s.append_character ('{')
							s.append (t)
							s.append_character ('}')
						end
						s.append_character ('=')
						v := c.item.string_representation.as_string_8
						if v.has ('%N') then
							s.append_character ('%N')
							across
								v.split ('%N') as v_cursor
							loop
								s.append ("     |")
								s.append (v_cursor.item)
								s.append_character ('%N')
							end
						else
							s.append (v)
							s.append_character ('%N')
						end
					end
				end
			else
				s.append (" none")
				s.append_character ('%N')
			end
		end

end
