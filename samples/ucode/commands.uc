// Copyright 2012-2022 Jo-Philipp Wich <jow@openwrt.org>
// Licensed to the public under the Apache License 2.0.

'use strict';

import { basename, mkstemp, popen } from 'fs';
import { urldecode } from 'luci.http';

// Decode a given string into arguments following shell quoting rules
// [[abc\ def "foo\"bar" abc'def']] -> [[abc def]] [[foo"bar]] [[abcdef]]
function parse_args(str) {
	let args = [];

	function isspace(c) {
		if (c == 9 || c == 10 || c == 11 || c == 12 || c == 13 || c == 32)
			return c;
	}

	function isquote(c) {
		if (c == 34 || c == 39 || c == 96)
			return c;
	}

	function isescape(c) {
		if (c == 92)
			return c;
	}

	function ismeta(c) {
		if (c == 36 || c == 92 || c == 96)
			return c;
	}

	// Scan substring defined by the indexes [s, e] of the string "str",
	// perform unquoting and de-escaping on the fly and store the result
	function unquote(start, end) {
		let esc, quote, res = [];

		for (let off = start; off < end; off++) {
			const byte = ord(str, off);
			const q = isquote(byte);
			const e = isescape(byte);
			const m = ismeta(byte);

			if (esc) {
				if (!m)
					push(res, 92);

				push(res, byte);
				esc = false;
			}
			else if (e && quote != 39) {
				esc = true;
			}
			else if (q && quote && q == quote) {
				quote = null;
			}
			else if (q && !quote) {
				quote = q;
			}
			else {
				push(res, byte);
			}
		}

		push(args, chr(...res));
	}

	// Find substring boundaries in "str". Ignore escaped or quoted
	// whitespace, pass found start- and end-index for each substring
	// to unquote()
	let esc, start, quote;

	for (let off = 0; off <= length(str); off++) {
		const byte = ord(str, off);
		const q = isquote(byte);
		const s = isspace(byte) ?? (byte === null);
		const e = isescape(byte);

		if (esc) {
			esc = false;
		}
		else if (e && quote != 39) {
			esc = true;
			start ??= off;
		}
		else if (q && quote && q == quote) {
			quote = null;
		}
		else if (q && !quote) {
			start ??= off;
			quote = q;
		}
		else if (s && !quote) {
			if (start !== null) {
				unquote(start, off);
				start = null;
			}
		}
		else {
			start ??= off;
		}
	}

	// If the "quote" is still set we encountered an unfinished string
	if (quote)
		unquote(start, length(str));

	return args;
}

function test_binary(str) {
	for (let off = 0, byte = ord(str); off < length(str); byte = ord(str, ++off))
		if (byte <= 8 || (byte >= 14 && byte <= 31))
			return true;

	return false;
}

function parse_cmdline(cmdid, args) {
	if (uci.get('luci', cmdid) == 'command') {
		let cmd = uci.get_all('luci', cmdid);
		let argv = parse_args(cmd?.command);

		if (cmd?.param == '1') {
			if (length(args))
				push(argv, ...(parse_args(urldecode(args)) ?? []));
			else if (length(args = http.formvalue('args')))
				push(argv, ...(parse_args(args) ?? []));
		}

		return map(argv, v => match(v, /[^\w.\/|-]/) ? `'${replace(v, "'", "'\\''")}'` : v);
	}
}

function execute_command(callback, ...args) {
	let argv = parse_cmdline(...args);

	if (argv) {
		let outfd = mkstemp();
		let errfd = mkstemp();

		const exitcode = system(`${join(' ', argv)} >&${outfd.fileno()} 2>&${errfd.fileno()}`);

		outfd.seek(0);
		errfd.seek(0);

		const stdout = outfd.read(1024 * 512) ?? '';
		const stderr = errfd.read(1024 * 512) ?? '';

		outfd.close();
		errfd.close();

		const binary = test_binary(stdout);

		callback({
			ok:      true,
			command: join(' ', argv),
			stdout:  binary ? null : stdout,
			stderr,
			exitcode,
			binary
		});
	}
	else {
		callback({
			ok:     false,
			code:   404,
			reason: "No such command"
		});
	}
}

function return_json(result) {
	if (result.ok) {
		http.prepare_content('application/json');
		http.write_json(result);
	}
	else {
		http.status(result.code, result.reason);
	}
}


function return_html(result) {
	if (result.ok) {
		include('commands_public', result);
	}
	else {
		http.status(result.code, result.reason);
	}
}

return {
	action_run: function(...args) {
		execute_command(return_json, ...args);
	},

	action_download: function(...args) {
		const argv = parse_cmdline(...args);

		if (argv) {
			const fd = popen(`${join(' ', argv)} 2>/dev/null`);

			if (fd) {
				let filename = replace(basename(argv[0]), /\W+/g, '.');
				let chunk = fd.read(4096) ?? '';
				let name;

				if (test_binary(chunk)) {
					http.header("Content-Disposition", `attachment; filename=${filename}.bin`);
					http.prepare_content("application/octet-stream");
				}
				else {
					http.header("Content-Disposition", `attachment; filename=${filename}.txt`);
					http.prepare_content("text/plain");
				}

				while (length(chunk)) {
					http.write(chunk);
					chunk = fd.read(4096);
				}

				fd.close();
			}
			else {
				http.status(500, "Failed to execute command");
			}
		}
		else {
			http.status(404, "No such command");
		}
	},

	action_public: function(cmdid, ...args) {
		let disp = false;

		if (substr(cmdid, -1) == "s") {
			disp = true;
			cmdid = substr(cmdid, 0, -1);
		}

		if (cmdid &&
		    uci.get('luci', cmdid) == 'command' &&
		    uci.get('luci', cmdid, 'public') == '1')
		{
			if (disp)
				execute_command(return_html, cmdid, ...args);
			else
				this.action_download(cmdid, args);
		}
		else {
			http.status(403, "Access to command denied");
		}
	}
};
