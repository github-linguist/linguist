// Copyright 2022 Jo-Philipp Wich <jo@mein.io>
// Licensed to the public under the Apache License 2.0.

import {
	urlencode as _urlencode,
	urldecode as _urldecode,
	urlencoded_parser, multipart_parser, header_attribute,
	ENCODE_IF_NEEDED, ENCODE_FULL, DECODE_IF_NEEDED, DECODE_PLUS
} from 'lucihttp';

import {
	error as fserror,
	stdin, stdout, mkstemp
} from 'fs';

import {
	openlog, syslog, closelog, LOG_NOTICE, LOG_LOCAL0
} from 'log';

import { run_plugins } from 'luciplugins';

// luci.http module scope
export let HTTP_MAX_CONTENT = 1024*100;		// 100 kB maximum content size

// Decode a mime encoded http message body with multipart/form-data
// Content-Type. Stores all extracted data associated with its parameter name
// in the params table within the given message object. Multiple parameter
// values are stored as tables, ordinary ones as strings.
// If an optional file callback function is given then it is fed with the
// file contents chunk by chunk and only the extracted file name is stored
// within the params table. The callback function will be called subsequently
// with three arguments:
//  o Table containing decoded (name, file) and raw (headers) mime header data
//  o String value containing a chunk of the file data
//  o Boolean which indicates whether the current chunk is the last one (eof)
export function mimedecode_message_body(src, msg, file_cb) {
	let len = 0, maxlen = +msg.env.CONTENT_LENGTH;
	let err, header, field, parser;

	parser = multipart_parser(msg.env.CONTENT_TYPE, function(what, buffer, length) {
		if (what == parser.PART_INIT) {
			field = {};
		}
		else if (what == parser.HEADER_NAME) {
			header = lc(buffer);
		}
		else if (what == parser.HEADER_VALUE && header) {
			if (lc(header) == 'content-disposition' &&
			    header_attribute(buffer, null) == 'form-data') {
				field.name = header_attribute(buffer, 'name');
				field.file = header_attribute(buffer, 'filename');
				field[1] = field.file;
			}

			field.headers = field.headers || {};
			field.headers[header] = buffer;
		}
		else if (what == parser.PART_BEGIN) {
			return !field.file;
		}
		else if (what == parser.PART_DATA && field.name && length > 0) {
			if (field.file) {
				if (file_cb) {
					file_cb(field, buffer, false);

					msg.params[field.name] = msg.params[field.name] || field;
				}
				else {
					if (!field.fd)
						field.fd = mkstemp(field.name);

					if (field.fd) {
						field.fd.write(buffer);
						msg.params[field.name] = msg.params[field.name] || field;
					}
				}
			}
			else {
				field.value = buffer;
			}
		}
		else if (what == parser.PART_END && field.name) {
			if (field.file && msg.params[field.name]) {
				if (file_cb)
					file_cb(field, '', true);
				else if (field.fd)
					field.fd.seek(0);
			}
			else {
				let val = msg.params[field.name];

				if (type(val) == 'array')
					push(val, field.value || '');
				else if (val != null)
					msg.params[field.name] = [ val, field.value || '' ];
				else
					msg.params[field.name] = field.value || '';
			}

			field = null;
		}
		else if (what == parser.ERROR) {
			err = buffer;
		}

		return true;
	}, HTTP_MAX_CONTENT);

	while (true) {
		let chunk = src();

		len += length(chunk);

		if (maxlen && len > maxlen + 2)
			die('Message body size exceeds Content-Length');

		if (!parser.parse(chunk))
			die(err);

		if (chunk == null)
			break;
	}
};

// Decode an urlencoded http message body with application/x-www-urlencoded
// Content-Type. Stores all extracted data associated with its parameter name
// in the params table within the given message object. Multiple parameter
// values are stored as tables, ordinary ones as strings.
export function urldecode_message_body(src, msg) {
	let len = 0, maxlen = +msg.env.CONTENT_LENGTH;
	let err, name, value, parser;

	parser = urlencoded_parser(function (what, buffer, length) {
		if (what == parser.TUPLE) {
			name = null;
			value = null;
		}
		else if (what == parser.NAME) {
			name = _urldecode(buffer, DECODE_PLUS);
		}
		else if (what == parser.VALUE && name) {
			let val = msg.params[name];

			if (type(val) == 'array')
				push(val, _urldecode(buffer, DECODE_PLUS) || '');
			else if (val != null)
				msg.params[name] = [ val, _urldecode(buffer, DECODE_PLUS) || '' ];
			else
				msg.params[name] = _urldecode(buffer, DECODE_PLUS) || '';
		}
		else if (what == parser.ERROR) {
			err = buffer;
		}

		return true;
	}, HTTP_MAX_CONTENT);

	while (true) {
		let chunk = src();

		len += length(chunk);

		if (maxlen && len > maxlen + 2)
			die('Message body size exceeds Content-Length');

		if (!parser.parse(chunk))
			die(err);

		if (chunk == null)
			break;
	}
};

// This function will examine the Content-Type within the given message object
// to select the appropriate content decoder.
// Currently the application/x-www-urlencoded and application/form-data
// mime types are supported. If the encountered content encoding can't be
// handled then the whole message body will be stored unaltered as 'content'
// property within the given message object.
export function parse_message_body(src, msg, filecb) {
	if (msg.env.CONTENT_LENGTH || msg.env.REQUEST_METHOD == 'POST') {
		let ctype = header_attribute(msg.env.CONTENT_TYPE, null);

		// Is it multipart/mime ?
		if (ctype == 'multipart/form-data')
			return mimedecode_message_body(src, msg, filecb);

		// Is it application/x-www-form-urlencoded ?
		else if (ctype == 'application/x-www-form-urlencoded')
			return urldecode_message_body(src, msg);

		// Unhandled encoding
		// If a file callback is given then feed it chunk by chunk, else
		// store whole buffer in message.content
		let sink;

		// If we have a file callback then feed it
		if (type(filecb) == 'function') {
			let meta = {
				name: 'raw',
				encoding: msg.env.CONTENT_TYPE
			};

			sink = (chunk) => {
				if (chunk != null)
					return filecb(meta, chunk, false);
				else
					return filecb(meta, null, true);
			};
		}

		// ... else append to .content
		else {
			let chunks = [], len = 0;

			sink = (chunk) => {
				len += length(chunk);

				if (len > HTTP_MAX_CONTENT)
					die('POST data exceeds maximum allowed length');

				if (chunk != null) {
					push(chunks, chunk);
				}
				else {
					msg.content = join('', chunks);
					msg.content_length = len;
				}
			};
		}

		// Pump data...
		while (true) {
			let chunk = src();

			sink(chunk);

			if (chunk == null)
				break;
		}

		return true;
	}

	return false;
};

export function build_querystring(q) {
	let s = [];

	for (let k, v in q) {
		push(s,
			length(s) ? '&' : '?',
			_urlencode(k, ENCODE_IF_NEEDED | ENCODE_FULL) || k,
			'=',
			_urlencode(v, ENCODE_IF_NEEDED | ENCODE_FULL) || v
		);
	}

	return join('', s);
};

export function urlencode(value) {
	if (value == null)
		return null;

	value = '' + value;

	return _urlencode(value, ENCODE_IF_NEEDED | ENCODE_FULL) || value;
};

export function urldecode(value, decode_plus) {
	if (value == null)
		return null;

	value = '' + value;

	return _urldecode(value, DECODE_IF_NEEDED | (decode_plus ? DECODE_PLUS : 0)) || value;
};

// Extract and split urlencoded data pairs, separated bei either "&" or ";"
// from given url or string. Returns a table with urldecoded values.
// Simple parameters are stored as string values associated with the parameter
// name within the table. Parameters with multiple values are stored as array
// containing the corresponding values.
export function urldecode_params(url, tbl) {
	let parser, name, value;
	let params = tbl || {};

	parser = urlencoded_parser(function(what, buffer, length) {
		if (what == parser.TUPLE) {
			name = null;
			value = null;
		}
		else if (what == parser.NAME) {
			name = _urldecode(buffer);
		}
		else if (what == parser.VALUE && name) {
			params[name] = _urldecode(buffer) || '';
		}

		return true;
	});

	if (parser) {
		let m = match(('' + (url || '')), /[^?]*$/);

		parser.parse(m ? m[0] : '');
		parser.parse(null);
	}

	return params;
};

// Encode each key-value-pair in given table to x-www-urlencoded format,
// separated by '&'. Tables are encoded as parameters with multiple values by
// repeating the parameter name with each value.
export function urlencode_params(tbl) {
	let enc = [];

	for (let k, v in tbl) {
		if (type(v) == 'array') {
			for (let v2 in v) {
				if (length(enc))
					push(enc, '&');

				push(enc,
					_urlencode(k),
					'=',
					_urlencode('' + v2));
			}
		}
		else {
			if (length(enc))
				push(enc, '&');

			push(enc,
				_urlencode(k),
				'=',
				_urlencode('' + v));
		}
	}

	return join(enc, '');
};


// Default IO routines suitable for CGI invocation
let avail_len = +getenv('CONTENT_LENGTH');

const default_source = () => {
	let rlen = min(avail_len, 4096);

	if (rlen == 0) {
		stdin.close();

		return null;
	}

	let chunk = stdin.read(rlen);

	if (chunk == null)
		die(`Input read error: ${fserror()}`);

	avail_len -= length(chunk);

	return chunk;
};

const default_sink = (...chunks) => {
	for (let chunk in chunks)
		stdout.write(chunk);

	stdout.flush();
};

const Class = {
	formvalue: function(name, noparse) {
		if (!noparse && !this.parsed_input)
			this._parse_input();

		if (name != null)
			return this.message.params[name];
		else
			return this.message.params;
	},

	formvaluetable: function(prefix) {
		let vals = {};

		prefix = (prefix || '') + '.';

		if (!this.parsed_input)
			this._parse_input();

		for (let k, v in this.message.params)
			if (index(k, prefix) == 0)
				 vals[substr(k, length(prefix))] = '' + v;

		return vals;
	},

	content: function() {
		if (!this.parsed_input)
			this._parse_input();

		return this.message.content;
	},

	getcookie: function(name) {
		return header_attribute(`cookie; ${this.getenv('HTTP_COOKIE') ?? ''}`, name);
	},

	getenv: function(name) {
		if (name != null)
			return this.message.env[name];
		else
			return this.message.env;
	},

	setfilehandler: function(callback) {
		if (type(callback) == 'resource' && type(callback.call) == 'function')
			this.filehandler = (...args) => callback.call(...args);
		else if (type(callback) == 'function')
			this.filehandler = callback;
		else
			die('Invalid callback argument for setfilehandler()');

		if (!this.parsed_input)
			return;

		// If input has already been parsed then uploads are stored as unlinked
		// temporary files pointed to by open file handles in the parameter
		// value table. Loop all params, and invoke the file callback for any
		// param with an open file handle.
		for (let name, value in this.message.params) {
			while (value?.fd) {
				let data = value.fd.read(1024);
				let eof = (length(data) == 0);

				this.filehandler(value, data, eof);

				if (eof) {
					value.fd.close();
					value.fd = null;
				}
			}
		}
	},

	_parse_input: function() {
		parse_message_body(
			this.input,
			this.message,
			this.filehandler
		);

		this.parsed_input = true;
	},

	close: function() {
		this.write_headers();
		this.closed = true;
	},

	header: function(key, value) {
		this.headers ??= {};
		this.headers[lc(key)] = value;
	},

	prepare_content: function(mime) {
		if (!this.headers?.['content-type']) {
			if (mime == 'application/xhtml+xml') {
				if (index(this.getenv('HTTP_ACCEPT'), mime) == -1) {
					mime = 'text/html; charset=UTF-8';
					this.header('Vary', 'Accept');
				}
			}

			this.header('Content-Type', mime);
		}
	},

	status: function(code, message) {
		this.status_code = code ?? 200;
		this.status_message = message ?? 'OK';
	},

	write_headers: function() {
		if (this.eoh)
			return;

		if (!this.status_code)
			this.status();

		if (!this.headers?.['content-type'])
			this.header('Content-Type', 'text/html; charset=UTF-8');

		if (!this.headers?.['cache-control']) {
			this.header('Cache-Control', 'no-cache');
			this.header('Expires', '0');
		}

		if (!this.headers?.['x-frame-options'])
			this.header('X-Frame-Options', 'SAMEORIGIN');

		if (!this.headers?.['x-xss-protection'])
			this.header('X-XSS-Protection', '1; mode=block');

		if (!this.headers?.['x-content-type-options'])
			this.header('X-Content-Type-Options', 'nosniff');

		/* http header plugins */
		let log_class = 'http.uc';
		openlog(log_class);
		for (let plugin_id, p_output in run_plugins('/luci/plugins/http/headers', 'http_headers_enabled')) {

			/* header plugins shall return e.g.: ['X-Header', 'foo'] */
			if (type(p_output) !== 'array' || length(p_output) !== 2)
				continue;

			if (type(p_output[0]) !== 'string' || type(p_output[1]) !== 'string')
				continue;

			if (!match(p_output[0], /^[A-Za-z0-9-]+$/)) {
				syslog(LOG_NOTICE|LOG_LOCAL0,
					sprintf("Invalid header name from plugin %s output: %s", plugin_id, p_output[0]));
				continue;
			}

			/* header plugin values shall not contain line-feeds */
			if (match(p_output[1], /[\r\n]/)) {
				syslog(LOG_NOTICE|LOG_LOCAL0,
					sprintf("\\r and/or \\n in plugin %s output", plugin_id));
				continue;
			}

			if(!this.headers?.[p_output[0]])
				this.header(p_output[0], p_output[1]);

		}
		closelog();

		this.output('Status: ');
		this.output(this.status_code);
		this.output(' ');
		this.output(this.status_message);
		this.output('\r\n');

		for (let k, v in this.headers) {
			this.output(k);
			this.output(': ');
			this.output(v);
			this.output('\r\n');
		}

		this.output('\r\n');

		this.eoh = true;
	},

	// If the content chunk is nil this function will automatically invoke close.
	write: function(content) {
		if (content != null && !this.closed) {
			this.write_headers();
			this.output(content);

			return true;
		}
		else {
			this.close();
		}
	},

	redirect: function(url) {
		this.status(302, 'Found');
		this.header('Location', url ?? '/');
		this.close();
	},

	write_json: function(value) {
		this.write(sprintf('%.J', value));
	},

	urlencode,
	urlencode_params,

	urldecode,
	urldecode_params,

	build_querystring
};

export default function(env, sourcein, sinkout) {
	return proto({
		input: sourcein ?? default_source,
		output: sinkout ?? default_sink,

		// File handler nil by default to let .content() work
		file: null,

		// HTTP-Message table
		message: {
			env,
			headers: {},
			params: urldecode_params(env?.QUERY_STRING ?? '')
		},

		parsed_input: false
	}, Class);
};
