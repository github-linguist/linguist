--// Authored by @Roblox (https://github.com/Roblox)
--// Fetched from (https://github.com/Roblox/roact-navigation/blob/master/src/routers/pathToRegexp.lua)
--// Licensed under the MIT License (https://github.com/Roblox/roact-navigation/blob/master/LICENSE)

-- upstream: https://github.com/pillarjs/path-to-regexp/blob/feddb3d3391d843f21ea9cde195f066149dba0be/src/index.ts
--[[
The MIT License (MIT)

Copyright (c) 2014 Blake Embrey (hello@blakeembrey.com)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
]]

--!strict
--!nolint LocalShadow
local root = script.Parent.Parent
local Packages = root.Parent
local LuauPolyfill = require(Packages.LuauPolyfill)
local Array = LuauPolyfill.Array
local RegExp = require(Packages.RegExp)

type Record<T, U> = { [T]: U }

local function TypeError(message)
	return message
end

local exports = {}

-- Roblox deviation: predeclare function variables
local escapeString
local flags

--[[
 * Tokenizer results.
 ]]
type LexToken = {
	type: string,
	-- | "OPEN"
	-- | "CLOSE"
	-- | "PATTERN"
	-- | "NAME"
	-- | "CHAR"
	-- | "ESCAPED_CHAR"
	-- | "MODIFIER"
	-- | "END",
	index: number,
	value: string,
}

--[[
 * Tokenize input string.
 ]]
local function lexer(str: string): { LexToken }
	local tokens: { LexToken } = {}
	local i = 1

	-- Roblox deviation: the original JavaScript contains a lot of `i++`, which
	-- does not translate really well to Luau. This function mimic the operation
	-- while still being an expression (because it's a function call)
	local function preIncrement_i(value)
		i += 1
		return value
	end
	-- Roblox deviation: use this function to translate `str[n]` directly
	local function getChar(n)
		return string.sub(str, n, n)
	end

	local strLength = string.len(str)

	while i <= strLength do
		local char = string.sub(str, i, i)

		if char == "*" or char == "+" or char == "?" then
			table.insert(tokens, {
				type = "MODIFIER",
				index = i,
				value = getChar(preIncrement_i(i)),
			})
			continue
		end

		if char == "\\" then
			table.insert(tokens, {
				type = "ESCAPED_CHAR",
				index = preIncrement_i(i),
				value = getChar(preIncrement_i(i)),
			})
			continue
		end

		if char == "{" then
			table.insert(tokens, {
				type = "OPEN",
				index = i,
				value = getChar(preIncrement_i(i)),
			})
			continue
		end

		if char == "}" then
			table.insert(tokens, {
				type = "CLOSE",
				index = i,
				value = getChar(preIncrement_i(i)),
			})
			continue
		end

		if char == ":" then
			local name = ""
			local j = i + 1

			-- Roblox deviation: the original JavaScript contains a lot of `j++`, which
			-- does not translate really well to Luau. This function mimic the operation
			-- while still being an expression (because it's a function call)
			local function preIncrement_j(value)
				j += 1
				return value
			end

			while j <= strLength do
				local code = string.byte(str, j)

				if
					-- // `0-9`
					(code >= 48 and code <= 57)
					-- // `A-Z`
					or (code >= 65 and code <= 90)
					-- // `a-z`
					or (code >= 97 and code <= 122)
					-- // `_`
					or code == 95
				then
					name = name .. getChar(preIncrement_j(j))
					continue
				end

				break
			end

			if name == "" then
				error(TypeError(("Missing parameter name at %d"):format(i)))
			end

			table.insert(tokens, {
				type = "NAME",
				index = i,
				value = name,
			})
			i = j
			continue
		end

		if char == "(" then
			local count = 1
			local pattern = ""
			local j = i + 1

			if getChar(j) == "?" then
				error(TypeError(('Pattern cannot start with "?" at %d'):format(j)))
			end

			-- Roblox deviation: the original JavaScript contains a lot of `j++`, which
			-- does not translate really well to Luau. This function mimic the operation
			-- while still being an expression (because it's a function call)
			local function preIncrement_j(value)
				j += 1
				return value
			end

			while j <= strLength do
				if getChar(j) == "\\" then
					pattern = pattern .. (getChar(preIncrement_j(j)) .. getChar(preIncrement_j(j)))
				end

				if getChar(j) == ")" then
					count = count - 1
					if count == 0 then
						j = j + 1
						break
					end
				elseif getChar(j) == "(" then
					count = count + 1

					if getChar(j + 1) ~= "?" then
						error(TypeError(("Capturing groups are not allowed at %d"):format(j)))
					end
				end

				pattern = pattern .. getChar(preIncrement_j(j))
			end

			if count ~= 0 then
				error(TypeError(("Unbalanced pattern at %d"):format(i)))
			end
			if pattern == "" then
				error(TypeError(("Missing pattern at %d"):format(i)))
			end

			table.insert(tokens, {
				type = "PATTERN",
				index = i,
				value = pattern,
			})
			i = j
			continue
		end

		table.insert(tokens, {
			type = "CHAR",
			index = i,
			value = getChar(preIncrement_i(i)),
		})
	end

	table.insert(tokens, {
		type = "END",
		index = i,
		value = "",
	})

	return tokens
end

export type ParseOptions = {
	--[[
	 * Set the default delimiter for repeat parameters. (default: `'/'`)
	 ]]
	delimiter: string?,
	--[[
	 * List of characters to automatically consider prefixes when parsing.
	 ]]
	prefixes: string?,
}

--[[
 * Parse a string for the raw tokens.
 ]]
function exports.parse(str: string, optionalOptions: ParseOptions?): { Token }
	local options = optionalOptions or {}
	local tokens = lexer(str)

	local prefixes = "./"
	if options.prefixes ~= nil and options.prefixes ~= "" then
		prefixes = options.prefixes
	end
	local defaultPattern = string.format("[^%s]+?", escapeString(options.delimiter or "/#?"))
	local result: { Token } = {}
	local key = 0
	local i = 1
	local path = ""

	-- Roblox deviation: the original JavaScript contains a lot of `i++`, which
	-- does not translate really well to Luau. This function mimic the operation
	-- while still being an expression (because it's a function call)
	local function preIncrement_i(value)
		i += 1
		return value
	end

	-- Roblox deviation: the original JavaScript contains a lot of `key++`, which
	-- does not translate really well to Luau. This function mimic the operation
	-- while still being an expression (because it's a function call)
	local function preIncrement_key(value)
		key += 1
		return value
	end

	local function tryConsume(type_): string?
		if i <= #tokens and tokens[i].type == type_ then
			local v = tokens[preIncrement_i(i)].value
			return v
		end
		return nil
	end

	local function mustConsume(type_): string
		local value = tryConsume(type_)
		if value ~= nil then
			return value
		end
		local token = tokens[i]
		if token == nil then
			error(TypeError(("Expected token %s, got nil"):format(type_)))
		end
		local nextType = token.type
		local index = token.index
		error(TypeError(("Unexpected %s at %d, expected %s"):format(nextType, index, type_)))
	end

	local function consumeText(): string
		local result = ""
		local value: string? = tryConsume("CHAR") or tryConsume("ESCAPED_CHAR")
		while value and value ~= "" do
			result ..= value
			value = tryConsume("CHAR") or tryConsume("ESCAPED_CHAR")
		end
		return result
	end

	while i <= #tokens do
		local char = tryConsume("CHAR")
		local name = tryConsume("NAME")
		local pattern = tryConsume("PATTERN")

		if (name and name ~= "") or (pattern and pattern ~= "") then
			local prefix = char or ""

			if string.find(prefixes, prefix) == nil then
				path = path .. prefix
				prefix = ""
			end

			if path ~= nil and path ~= "" then
				table.insert(result, path)
				path = ""
			end

			local resultName = name
			if name == nil or name == "" then
				resultName = preIncrement_key(key)
			end
			local resultPattern = pattern
			if pattern == nil or pattern == "" then
				resultPattern = defaultPattern
			end

			table.insert(result, {
				name = resultName,
				prefix = prefix,
				suffix = "",
				pattern = resultPattern,
				modifier = tryConsume("MODIFIER") or "",
			})
			continue
		end

		local value = char or tryConsume("ESCAPED_CHAR")

		if value and value ~= "" then
			path ..= value
			continue
		end

		if path and path ~= "" then
			table.insert(result, path)
			path = ""
		end

		local open = tryConsume("OPEN")

		if open and open ~= "" then
			local prefix = consumeText()
			local name = tryConsume("NAME") or ""
			local pattern = tryConsume("PATTERN") or ""
			local suffix = consumeText()

			mustConsume("CLOSE")

			if name == "" and pattern ~= "" then
				name = preIncrement_key(key)
			end
			-- Roblox deviation: we need to check if name is not 0, because 0 is false in JavaScript, and
			-- it could be number because it could be assigned to the key value
			if (name ~= "" and name ~= 0) and (pattern == nil or pattern == "") then
				pattern = defaultPattern
			end

			table.insert(result, {
				name = name,
				pattern = pattern,
				prefix = prefix,
				suffix = suffix,
				modifier = tryConsume("MODIFIER") or "",
			})
			continue
		end

		mustConsume("END")
	end

	return result
end

export type TokensToFunctionOptions = {
	--[[
	 * When `true` the regexp will be case sensitive. (default: `false`)
	 ]]
	sensitive: boolean?,
	--[[
	 * Function for encoding input strings for output.
	 ]]
	encode: nil | (string, Key) -> string,
	--[[
	 * When `false` the function can produce an invalid (unmatched) path. (default: `true`)
	 ]]
	validate: boolean?,
}

--[[
 * Compile a string to a template function for the path.
 ]]
function exports.compile(str: string, options: (ParseOptions & TokensToFunctionOptions)?)
	return exports.tokensToFunction(exports.parse(str, options), options)
end

export type PathFunction<P> = (P?) -> string

--[[
 * Expose a method for transforming tokens into the path function.
 ]]
function exports.tokensToFunction(tokens: { Token }, optionalOptions: TokensToFunctionOptions?)
	if optionalOptions == nil then
		optionalOptions = {}
	end
	local options = optionalOptions :: TokensToFunctionOptions
	local reFlags = flags(options)
	local encode = options.encode or function(x: string): string
		return x
	end
	local validate = options.validate
	if validate == nil then
		validate = true
	end

	-- Compile all the tokens into regexps.
	local matches = Array.map(tokens, function(token)
		if type(token) == "table" then
			return RegExp(("^(?:%s)$"):format(token.pattern), reFlags)
		end
		return nil
	end)

	return function(data: Record<string, any>?)
		local path = ""

		for i, token in tokens do
			if type(token) == "string" then
				path ..= token
				continue
			end

			-- Roblox deviation: in JavaScript, indexing an object with a number will coerce the number
			-- value into a string
			local value = if data then data[tostring(token.name)] else nil
			local optional = token.modifier == "?" or token.modifier == "*"
			local repeat_ = token.modifier == "*" or token.modifier == "+"

			if Array.isArray(value) then
				if not repeat_ then
					error(TypeError(('Expected "%s" to not repeat, but got an array'):format(token.name)))
				end

				if #value == 0 then
					if optional then
						continue
					end

					error(TypeError(('Expected "%s" to not be empty'):format(token.name)))
				end

				for _, element in value do
					local segment = encode(element, token)

					if validate and not matches[i]:test(segment) then
						error(
							TypeError(
								('Expected all "%s" to match "%s", but got "%s"'):format(
									token.name,
									token.pattern,
									segment
								)
							)
						)
					end

					path ..= token.prefix .. segment .. token.suffix
				end

				continue
			end

			local valueType = type(value)
			if valueType == "string" or valueType == "number" then
				local segment = encode(tostring(value), token)

				if validate and not matches[i]:test(segment) then
					error(
						TypeError(
							('Expected "%s" to match "%s", but got "%s"'):format(token.name, token.pattern, segment)
						)
					)
				end

				path ..= token.prefix .. segment .. token.suffix
				continue
			end

			if optional then
				continue
			end

			local typeOfMessage = if repeat_ then "an array" else "a string"
			error(TypeError(('Expected "%s" to be %s'):format(tostring(token.name), typeOfMessage)))
		end

		return path
	end
end

export type RegexpToFunctionOptions = {
	--[[
   * Function for decoding strings for params.
   ]]
	decode: nil | (string, Key) -> string,
}

--[[
 * A match result contains data about the path match.
 ]]
export type MatchResult<P> = {
	path: string,
	index: number,
	params: P,
}

--[[
 * A match is either `false` (no match) or a match result.
 ]]
-- export type Match<P> = false | MatchResult<P>
export type Match<P> = boolean | MatchResult<P>

--[[
 * The match function takes a string and returns whether it matched the path.
 ]]
export type MatchFunction<P> = (string) -> Match<P>

--[[
 * Create path match function from `path-to-regexp` spec.
 ]]
function exports.match(str, options)
	local keys: { Key } = {}
	local re = exports.pathToRegexp(str, keys, options)
	return exports.regexpToFunction(re, keys, options)
end

--[[
 * Create a path match function from `path-to-regexp` output.
 ]]
function exports.regexpToFunction(re: any, keys: { Key }, options: RegexpToFunctionOptions)
	if options == nil then
		options = {}
	end
	local decode = options.decode or function(x: string)
		return x
	end

	return function(pathname: string)
		local matches = re:exec(pathname)
		if not matches then
			return false
		end

		local path = matches[1]
		local index = matches.index or 0
		local params = {}

		-- Roblox deviation: start the iteration from 2 instead of 1 because the individual
		-- matches start at 2 in our polyfill version of RegExp objects.
		for i = 2, matches.n do
			if matches[i] == nil then
				continue
			end

			-- Roblox comment: keep the `-1` because our matches array start from 1,
			-- so the loop starts at index 2 (index 1 is the full matched string)
			local key = keys[i - 1]
			if key.modifier == "*" or key.modifier == "+" then
				params[key.name] = Array.map(string.split(matches[i], key.prefix .. key.suffix), function(value)
					return decode(value, key)
				end)
			else
				params[key.name] = decode(matches[i], key)
			end
		end

		return {
			path = path,
			index = index,
			params = params,
		}
	end
end

--[[
 * Escape a regular expression string.
 ]]
function escapeString(str: string)
	return string.gsub(str, "[%.%+%*%?=%^!:${}%(%)%[%]|/\\]", function(match)
		return "\\" .. match
	end)
end

--[[
 * Get the flags for a regexp from the options.
 ]]
function flags(options: { sensitive: boolean? }?)
	if options and options.sensitive then
		return ""
	else
		return "i"
	end
end

--[[
 * Metadata about a key.
 ]]
export type Key = {
	name: string | number,
	prefix: string,
	suffix: string,
	pattern: string,
	modifier: string,
}

--[[
 * A token is a string (nothing special) or key metadata (capture group).
 ]]
export type Token = string | Key

-- Roblox deviation: this functionality is not required so it has been omitted
--[[
 * Pull out keys from a regexp.
 ]]
-- local function regexpToRegexp(path: string, keys: { Key }?): string
-- 	if not keys then
-- 		return path
-- 	end

-- 	local groupsRegex = "%(" .. "(%?<(.*)>)?" .. "[^%?]"

-- 	local index = 0
-- 	local matchGenerator = path.source:gmatch(groupsRegex)
-- 	-- local execResult = groupsRegex.exec(path.source)
-- 	local execResult = matchGenerator()

-- 	while execResult do
-- 		error('got match -> ' .. execResult .. " for path = " .. path)
-- 		local name = execResult[1]
-- 		if name then
-- 			name = index
-- 			index += 1
-- 		end
-- 		table.insert(keys, {
-- 			-- // Use parenthesized substring match if available, index otherwise
-- 			name = name,
-- 			prefix = "",
-- 			suffix = "",
-- 			modifier = "",
-- 			pattern = "",
-- 		})

-- 		-- execResult = groupsRegex.exec(path.source)
-- 		execResult = matchGenerator()
-- 	end

-- 	return path
-- end

--[[
 * Transform an array into a regexp.
 ]]
local function arrayToRegexp(
	paths: { string },
	keys: { Key }?,
	options: (TokensToRegexpOptions & ParseOptions)?
): string
	local parts = Array.map(paths, function(path)
		return exports.pathToRegexp(path, keys, options).source
	end)

	return RegExp(("(?:%s)"):format(table.concat(parts, "|")), flags(options))
end

--[[
 * Create a path regexp from string input.
 ]]
local function stringToRegexp(path, keys, options)
	return exports.tokensToRegexp(exports.parse(path, options), keys, options)
end

export type TokensToRegexpOptions = {
	--[[
	 * When `true` the regexp will be case sensitive. (default: `false`)
	 ]]
	sensitive: boolean?,
	--[[
	 * When `true` the regexp won't allow an optional trailing delimiter to match. (default: `false`)
	 ]]
	strict: boolean?,
	--[[
	 * When `true` the regexp will match to the end of the string. (default: `true`)
	 ]]
	end_: boolean?,
	--[[
	 * When `true` the regexp will match from the beginning of the string. (default: `true`)
	 ]]
	start: boolean?,
	--[[
	 * Sets the final character for non-ending optimistic matches. (default: `/`)
	 ]]
	delimiter: string?,
	--[[
	 * List of characters that can also be "end" characters.
	 ]]
	endsWith: string?,
	--[[
	 * Encode path tokens for use in the `RegExp`.
	 ]]
	encode: nil | (string) -> string,
}

--[[
   * Expose a function for taking tokens and returning a RegExp.
 ]]
function exports.tokensToRegexp(tokens: { Token }, keys: { Key }?, optionalOptions: TokensToRegexpOptions?)
	local options = {}
	if optionalOptions ~= nil then
		options = optionalOptions
	end
	local strict = options.strict
	if strict == nil then
		strict = false
	end
	local start = options.start
	if start == nil then
		start = true
	end
	local end_ = options.end_
	if end_ == nil then
		end_ = true
	end
	local encode = options.encode or function(x: string)
		return x
	end
	-- Roblox deviation: our Lua regex implementation does not support empty character class
	local endsWith = if options.endsWith then ("[%s]|$"):format(escapeString(options.endsWith or "")) else "$"
	local delimiter = ("[%s]"):format(escapeString(options.delimiter or "/#?"))
	local route = if start then "^" else ""

	-- // Iterate over the tokens and create our regexp string.
	for _, token in tokens do
		if type(token) == "string" then
			route ..= escapeString(encode(token))
		else
			local prefix = escapeString(encode(token.prefix))
			local suffix = escapeString(encode(token.suffix))

			if token.pattern and token.pattern ~= "" then
				if keys then
					table.insert(keys, token)
				end

				if (prefix and prefix ~= "") or (suffix and suffix ~= "") then
					if token.modifier == "+" or token.modifier == "*" then
						local mod = if token.modifier == "*" then "?" else ""
						route ..= ("(?:%s((?:%s)(?:%s%s(?:%s))*)%s)%s"):format(
							prefix,
							token.pattern,
							suffix,
							prefix,
							token.pattern,
							suffix,
							mod
						)
					else
						route ..= ("(?:%s(%s)%s)%s"):format(prefix, token.pattern, suffix, token.modifier)
					end
				else
					route ..= ("(%s)%s"):format(token.pattern, token.modifier)
				end
			else
				route ..= ("(?:%s%s)%s"):format(prefix, suffix, token.modifier)
			end
		end
	end

	if end_ then
		if not strict then
			route ..= ("%s?"):format(delimiter)
		end

		if options.endsWith and options.endsWith ~= "" then
			route ..= ("(?=%s)"):format(endsWith)
		else
			route ..= "$"
		end
	else
		local endToken = tokens[#tokens]
		local isEndDelimited = endToken == nil
		if type(endToken) == "string" then
			isEndDelimited = string.find(delimiter, endToken:sub(-1)) ~= nil
		end

		if not strict then
			route ..= string.format("(?:%s(?=%s))?", delimiter, endsWith)
		end
		if not isEndDelimited then
			route ..= string.format("(?=%s|%s)", delimiter, endsWith)
		end
	end

	return RegExp(route, flags(options))
end

--[[
 * Supported `path-to-regexp` input types.
 ]]
export type Path = string | { string }

--[[
 * Normalize the given path string, returning a regular expression.
 *
 * An empty array can be passed in for the keys, which will hold the
 * placeholder key descriptions. For example, using `/user/:id`, `keys` will
 * contain `[{ name: 'id', delimiter: '/', optional: false, repeat: false }]`.
 ]]
function exports.pathToRegexp(path: Path, keys: { Key }?, options: (TokensToRegexpOptions & ParseOptions)?)
	-- if (path instanceof RegExp) return regexpToRegexp(path, keys);
	if Array.isArray(path) then
		return arrayToRegexp(path :: { string }, keys, options)
	end
	return stringToRegexp(path, keys, options)
end

return exports