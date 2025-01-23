/*
 * Copyright 2011, Oliver Tappe <zooey@hirschkaefer.de>
 * Copyright 2016, Andrew Lindesay <apl@lindesay.co.nz>
 * Distributed under the terms of the MIT License.
 *
 * Source - https://github.com/haiku/haiku/blob/73e180c9b965aaeb034055453e153bc3adf47917/src/kits/package/PackageInfoParser.cpp
 */


#include "PackageInfoParser.h"

#include <ctype.h>
#include <stdint.h>
#include <stdlib.h>

#include <algorithm>
#include <string>

#include <Url.h>

namespace BPackageKit {


BPackageInfo::ParseErrorListener::~ParseErrorListener()
{
}


BPackageInfo::Parser::Parser(ParseErrorListener* listener)
	:
	fListener(listener),
	fPos(NULL)
{
}


status_t
BPackageInfo::Parser::Parse(const BString& packageInfoString,
	BPackageInfo* packageInfo)
{
	if (packageInfo == NULL)
		return B_BAD_VALUE;

	fPos = packageInfoString.String();

	try {
		_Parse(packageInfo);
	} catch (const ParseError& error) {
		if (fListener != NULL) {
			// map error position to line and column
			int line = 1;
			int inLineOffset;
			int32 offset = error.pos - packageInfoString.String();
			int32 newlinePos = packageInfoString.FindLast('\n', offset - 1);
			if (newlinePos < 0)
				inLineOffset = offset;
			else {
				inLineOffset = offset - newlinePos - 1;
				do {
					line++;
					newlinePos = packageInfoString.FindLast('\n',
						newlinePos - 1);
				} while (newlinePos >= 0);
			}

			int column = 0;
			for (int i = 0; i < inLineOffset; i++) {
				column++;
				if (error.pos[i - inLineOffset] == '\t')
					column = (column + 3) / 4 * 4;
			}

			fListener->OnError(error.message, line, column + 1);
		}
		return B_BAD_DATA;
	} catch (const std::bad_alloc& e) {
		if (fListener != NULL)
			fListener->OnError("out of memory", 0, 0);
		return B_NO_MEMORY;
	}

	return B_OK;
}


status_t
BPackageInfo::Parser::ParseVersion(const BString& versionString,
	bool revisionIsOptional, BPackageVersion& _version)
{
	fPos = versionString.String();

	try {
		Token token(TOKEN_STRING, fPos, versionString.Length());
		_ParseVersionValue(token, &_version, revisionIsOptional);
	} catch (const ParseError& error) {
		if (fListener != NULL) {
			int32 offset = error.pos - versionString.String();
			fListener->OnError(error.message, 1, offset);
		}
		return B_BAD_DATA;
	} catch (const std::bad_alloc& e) {
		if (fListener != NULL)
			fListener->OnError("out of memory", 0, 0);
		return B_NO_MEMORY;
	}

	return B_OK;
}


status_t
BPackageInfo::Parser::ParseResolvableExpression(const BString& expressionString,
	BPackageResolvableExpression& _expression)
{
	fPos = expressionString.String();

	try {
		Token token(TOKEN_STRING, fPos, expressionString.Length());
		_ParseResolvableExpression(_NextToken(), _expression, NULL);
	} catch (const ParseError& error) {
		if (fListener != NULL) {
			int32 offset = error.pos - expressionString.String();
			fListener->OnError(error.message, 1, offset);
		}
		return B_BAD_DATA;
	} catch (const std::bad_alloc& e) {
		if (fListener != NULL)
			fListener->OnError("out of memory", 0, 0);
		return B_NO_MEMORY;
	}

	return B_OK;
}


BPackageInfo::Parser::Token
BPackageInfo::Parser::_NextToken()
{
	// Eat any whitespace, comments, or escaped new lines. Also eat ';' -- they
	// have the same function as newlines. We remember the last encountered ';'
	// or '\n' and return it as a token afterwards.
	const char* itemSeparatorPos = NULL;
	bool inComment = false;
	while ((inComment && *fPos != '\0') || isspace(*fPos) || *fPos == ';'
		|| *fPos == '#' || *fPos == '\\') {
		if (*fPos == '#') {
			inComment = true;
		} else if (!inComment && *fPos == '\\') {
			if (fPos[1] != '\n')
				break;
			// ignore escaped line breaks
			fPos++;
		} else if (*fPos == '\n') {
			itemSeparatorPos = fPos;
			inComment = false;
		} else if (!inComment && *fPos == ';')
			itemSeparatorPos = fPos;
		fPos++;
	}

	if (itemSeparatorPos != NULL) {
		return Token(TOKEN_ITEM_SEPARATOR, itemSeparatorPos);
	}

	const char* tokenPos = fPos;
	switch (*fPos) {
		case '\0':
			return Token(TOKEN_EOF, fPos);

		case '{':
			fPos++;
			return Token(TOKEN_OPEN_BRACE, tokenPos);

		case '}':
			fPos++;
			return Token(TOKEN_CLOSE_BRACE, tokenPos);

		case '<':
			fPos++;
			if (*fPos == '=') {
				fPos++;
				return Token(TOKEN_OPERATOR_LESS_EQUAL, tokenPos, 2);
			}
			return Token(TOKEN_OPERATOR_LESS, tokenPos, 1);

		case '=':
			fPos++;
			if (*fPos == '=') {
				fPos++;
				return Token(TOKEN_OPERATOR_EQUAL, tokenPos, 2);
			}
			return Token(TOKEN_OPERATOR_ASSIGN, tokenPos, 1);

		case '!':
			if (fPos[1] == '=') {
				fPos += 2;
				return Token(TOKEN_OPERATOR_NOT_EQUAL, tokenPos, 2);
			}
			break;

		case '>':
			fPos++;
			if (*fPos == '=') {
				fPos++;
				return Token(TOKEN_OPERATOR_GREATER_EQUAL, tokenPos, 2);
			}
			return Token(TOKEN_OPERATOR_GREATER, tokenPos, 1);

		default:
		{
			std::string string;
			char quoteChar = '\0';

			for (; *fPos != '\0'; fPos++) {
				char c = *fPos;
				if (quoteChar != '\0') {
					// within a quoted string segment
					if (c == quoteChar) {
						quoteChar = '\0';
						continue;
					}

					if (c == '\\') {
						// next char is escaped
						c = *++fPos;
						if (c == '\0') {
							throw ParseError("unterminated quoted-string",
								tokenPos);
						}

						if (c == 'n')
							c = '\n';
						else if (c == 't')
							c = '\t';
					}

					string += c;
				} else {
					// unquoted string segment
					switch (c) {
						case '"':
						case '\'':
							// quoted string start
							quoteChar = c;
							continue;

						case '{':
						case '}':
						case '<':
						case '=':
						case '!':
						case '>':
							// a separator character -- this ends the string
							break;

						case '\\':
							// next char is escaped
							c = *++fPos;
							if (c == '\0') {
								throw ParseError("'\\' at end of string",
									tokenPos);
							}
							string += c;
							continue;

						default:
							if (isspace(c))
								break;
							string += c;
							continue;
					}

					break;
				}
			}

			return Token(TOKEN_STRING, tokenPos, fPos - tokenPos,
				string.c_str());
		}
	}

	BString error = BString("unknown token '") << *fPos << "' encountered";
	throw ParseError(error.String(), fPos);
}


void
BPackageInfo::Parser::_RewindTo(const Token& token)
{
	fPos = token.pos;
}


void
BPackageInfo::Parser::_ParseStringValue(BString* value, const char** _tokenPos)
{
	Token string = _NextToken();
	if (string.type != TOKEN_STRING)
		throw ParseError("expected string", string.pos);

	*value = string.text;
	if (_tokenPos != NULL)
		*_tokenPos = string.pos;
}


void
BPackageInfo::Parser::_ParseArchitectureValue(BPackageArchitecture* value)
{
	Token arch = _NextToken();
	if (arch.type == TOKEN_STRING) {
		for (int i = 0; i < B_PACKAGE_ARCHITECTURE_ENUM_COUNT; ++i) {
			if (arch.text.ICompare(BPackageInfo::kArchitectureNames[i]) == 0) {
				*value = (BPackageArchitecture)i;
				return;
			}
		}
	}

	BString error("architecture must be one of: [");
	for (int i = 0; i < B_PACKAGE_ARCHITECTURE_ENUM_COUNT; ++i) {
		if (i > 0)
			error << ",";
		error << BPackageInfo::kArchitectureNames[i];
	}
	error << "]";
	throw ParseError(error, arch.pos);
}


void
BPackageInfo::Parser::_ParseVersionValue(BPackageVersion* value,
	bool revisionIsOptional)
{
	Token word = _NextToken();
	_ParseVersionValue(word, value, revisionIsOptional);
}


/*static*/ void
BPackageInfo::Parser::_ParseVersionValue(Token& word, BPackageVersion* value,
	bool revisionIsOptional)
{
	if (word.type != TOKEN_STRING)
		throw ParseError("expected string (a version)", word.pos);

	// get the revision number
	uint32 revision = 0;
	int32 dashPos = word.text.FindLast('-');
	if (dashPos >= 0) {
		char* end;
		long long number = strtoll(word.text.String() + dashPos + 1, &end,
			0);
		if (*end != '\0' || number < 0 || number > UINT_MAX) {
			throw ParseError("revision must be a number > 0 and < UINT_MAX",
				word.pos + dashPos + 1);
		}

		revision = (uint32)number;
		word.text.Truncate(dashPos);
	}

	if (revision == 0 && !revisionIsOptional) {
		throw ParseError("expected revision number (-<number> suffix)",
			word.pos + word.text.Length());
	}

	// get the pre-release string
	BString preRelease;
	int32 tildePos = word.text.FindLast('~');
	if (tildePos >= 0) {
		word.text.CopyInto(preRelease, tildePos + 1,
			word.text.Length() - tildePos - 1);
		word.text.Truncate(tildePos);

		if (preRelease.IsEmpty()) {
			throw ParseError("invalid empty pre-release string",
				word.pos + tildePos + 1);
		}

		int32 errorPos;
		if (!_IsAlphaNumUnderscore(preRelease, ".", &errorPos)) {
			throw ParseError("invalid character in pre-release string",
				word.pos + tildePos + 1 + errorPos);
		}
	}

	// get major, minor, and micro strings
	BString major;
	BString minor;
	BString micro;
	int32 firstDotPos = word.text.FindFirst('.');
	if (firstDotPos < 0)
		major = word.text;
	else {
		word.text.CopyInto(major, 0, firstDotPos);
		int32 secondDotPos = word.text.FindFirst('.', firstDotPos + 1);
		if (secondDotPos == firstDotPos + 1)
			throw ParseError("expected minor version", word.pos + secondDotPos);

		if (secondDotPos < 0) {
			word.text.CopyInto(minor, firstDotPos + 1, word.text.Length());
		} else {
			word.text.CopyInto(minor, firstDotPos + 1,
				secondDotPos - (firstDotPos + 1));
			word.text.CopyInto(micro, secondDotPos + 1, word.text.Length());

			int32 errorPos;
			if (!_IsAlphaNumUnderscore(micro, ".", &errorPos)) {
				throw ParseError("invalid character in micro version string",
					word.pos + secondDotPos + 1 + errorPos);
			}
		}

		int32 errorPos;
		if (!_IsAlphaNumUnderscore(minor, "", &errorPos)) {
			throw ParseError("invalid character in minor version string",
				word.pos + firstDotPos + 1 + errorPos);
		}
	}

	int32 errorPos;
	if (!_IsAlphaNumUnderscore(major, "", &errorPos)) {
		throw ParseError("invalid character in major version string",
			word.pos + errorPos);
	}

	value->SetTo(major, minor, micro, preRelease, revision);
}


void
BPackageInfo::Parser::_ParseResolvableExpression(const Token& token,
	BPackageResolvableExpression& _value, BString* _basePackage)
{
	if (token.type != TOKEN_STRING) {
		throw ParseError("expected word (a resolvable name)",
			token.pos);
	}

	int32 errorPos;
	if (!_IsValidResolvableName(token.text, &errorPos)) {
		throw ParseError("invalid character in resolvable name",
			token.pos + errorPos);
	}

	BPackageVersion version;
	Token op = _NextToken();
	BPackageResolvableOperator resolvableOperator;
	if (op.type == TOKEN_OPERATOR_LESS
		|| op.type == TOKEN_OPERATOR_LESS_EQUAL
		|| op.type == TOKEN_OPERATOR_EQUAL
		|| op.type == TOKEN_OPERATOR_NOT_EQUAL
		|| op.type == TOKEN_OPERATOR_GREATER_EQUAL
		|| op.type == TOKEN_OPERATOR_GREATER) {
		_ParseVersionValue(&version, true);

		if (_basePackage != NULL) {
			Token base = _NextToken();
			if (base.type == TOKEN_STRING && base.text == "base") {
				if (!_basePackage->IsEmpty()) {
					throw ParseError("multiple packages marked as base package",
						token.pos);
				}

				*_basePackage = token.text;
			} else
				_RewindTo(base);
		}

		resolvableOperator = (BPackageResolvableOperator)
			(op.type - TOKEN_OPERATOR_LESS);
	} else if (op.type == TOKEN_ITEM_SEPARATOR
		|| op.type == TOKEN_CLOSE_BRACE || op.type == TOKEN_EOF) {
		_RewindTo(op);
		resolvableOperator = B_PACKAGE_RESOLVABLE_OP_ENUM_COUNT;
	} else {
		throw ParseError(
			"expected '<', '<=', '==', '!=', '>=', '>', comma or '}'",
			op.pos);
	}

	_value.SetTo(token.text, resolvableOperator, version);
}


void
BPackageInfo::Parser::_ParseList(ListElementParser& elementParser,
	bool allowSingleNonListElement)
{
	Token openBracket = _NextToken();
	if (openBracket.type != TOKEN_OPEN_BRACE) {
		if (!allowSingleNonListElement)
			throw ParseError("expected start of list ('{')", openBracket.pos);

		elementParser(openBracket);
		return;
	}

	while (true) {
		Token token = _NextToken();
		if (token.type == TOKEN_CLOSE_BRACE)
			return;

		if (token.type == TOKEN_ITEM_SEPARATOR)
			continue;

		elementParser(token);
	}
}


void
BPackageInfo::Parser::_ParseStringList(BStringList* value,
	bool requireResolvableName, bool convertToLowerCase,
	StringValidator* stringValidator)
{
	struct StringParser : public ListElementParser {
		BStringList* value;
		bool requireResolvableName;
		bool convertToLowerCase;
		StringValidator* stringValidator;

		StringParser(BStringList* value, bool requireResolvableName,
			bool convertToLowerCase, StringValidator* stringValidator)
			:
			value(value),
			requireResolvableName(requireResolvableName),
			convertToLowerCase(convertToLowerCase),
			stringValidator(stringValidator)
		{
		}

		virtual void operator()(const Token& token)
		{
			if (token.type != TOKEN_STRING)
				throw ParseError("expected string", token.pos);

			if (requireResolvableName) {
				int32 errorPos;
				if (!_IsValidResolvableName(token.text, &errorPos)) {
					throw ParseError("invalid character in resolvable name",
						token.pos + errorPos);
				}
			}

			BString element(token.text);
			if (convertToLowerCase)
				element.ToLower();

			if (stringValidator != NULL)
				stringValidator->Validate(element, token.pos);

			value->Add(element);
		}
	} stringParser(value, requireResolvableName, convertToLowerCase,
		stringValidator);

	_ParseList(stringParser, true);
}


uint32
BPackageInfo::Parser::_ParseFlags()
{
	struct FlagParser : public ListElementParser {
		uint32 flags;

		FlagParser()
			:
			flags(0)
		{
		}

		virtual void operator()(const Token& token)
		{
			if (token.type != TOKEN_STRING)
				throw ParseError("expected word (a flag)", token.pos);

			if (token.text.ICompare("approve_license") == 0)
				flags |= B_PACKAGE_FLAG_APPROVE_LICENSE;
			else if (token.text.ICompare("system_package") == 0)
				flags |= B_PACKAGE_FLAG_SYSTEM_PACKAGE;
			else {
				throw ParseError(
					"expected 'approve_license' or 'system_package'",
					token.pos);
			}
		}
	} flagParser;

	_ParseList(flagParser, true);

	return flagParser.flags;
}


void
BPackageInfo::Parser::_ParseResolvableList(
	BObjectList<BPackageResolvable>* value)
{
	struct ResolvableParser : public ListElementParser {
		Parser& parser;
		BObjectList<BPackageResolvable>* value;

		ResolvableParser(Parser& parser_,
			BObjectList<BPackageResolvable>* value_)
			:
			parser(parser_),
			value(value_)
		{
		}

		virtual void operator()(const Token& token)
		{
			if (token.type != TOKEN_STRING) {
				throw ParseError("expected word (a resolvable name)",
					token.pos);
			}

			int32 errorPos;
			if (!_IsValidResolvableName(token.text, &errorPos)) {
				throw ParseError("invalid character in resolvable name",
					token.pos + errorPos);
			}

			// parse version
			BPackageVersion version;
			Token op = parser._NextToken();
			if (op.type == TOKEN_OPERATOR_ASSIGN) {
				parser._ParseVersionValue(&version, true);
			} else if (op.type == TOKEN_ITEM_SEPARATOR
				|| op.type == TOKEN_CLOSE_BRACE) {
				parser._RewindTo(op);
			} else
				throw ParseError("expected '=', comma or '}'", op.pos);

			// parse compatible version
			BPackageVersion compatibleVersion;
			Token compatible = parser._NextToken();
			if (compatible.type == TOKEN_STRING
				&& (compatible.text == "compat"
					|| compatible.text == "compatible")) {
				op = parser._NextToken();
				if (op.type == TOKEN_OPERATOR_GREATER_EQUAL) {
					parser._ParseVersionValue(&compatibleVersion, true);
				} else
					parser._RewindTo(compatible);
			} else
				parser._RewindTo(compatible);

			value->AddItem(new BPackageResolvable(token.text, version,
				compatibleVersion));
		}
	} resolvableParser(*this, value);

	_ParseList(resolvableParser, false);
}


void
BPackageInfo::Parser::_ParseResolvableExprList(
	BObjectList<BPackageResolvableExpression>* value, BString* _basePackage)
{
	struct ResolvableExpressionParser : public ListElementParser {
		Parser& parser;
		BObjectList<BPackageResolvableExpression>* value;
		BString* basePackage;

		ResolvableExpressionParser(Parser& parser,
			BObjectList<BPackageResolvableExpression>* value,
			BString* basePackage)
			:
			parser(parser),
			value(value),
			basePackage(basePackage)
		{
		}

		virtual void operator()(const Token& token)
		{
			BPackageResolvableExpression expression;
			parser._ParseResolvableExpression(token, expression, basePackage);
			value->AddItem(new BPackageResolvableExpression(expression));
		}
	} resolvableExpressionParser(*this, value, _basePackage);

	_ParseList(resolvableExpressionParser, false);
}


void
BPackageInfo::Parser::_ParseGlobalWritableFileInfos(
	GlobalWritableFileInfoList* infos)
{
	struct GlobalWritableFileInfoParser : public ListElementParser {
		Parser& parser;
		GlobalWritableFileInfoList* infos;

		GlobalWritableFileInfoParser(Parser& parser,
			GlobalWritableFileInfoList* infos)
			:
			parser(parser),
			infos(infos)
		{
		}

		virtual void operator()(const Token& token)
		{
			if (token.type != TOKEN_STRING) {
				throw ParseError("expected string (a file path)",
					token.pos);
			}

			BWritableFileUpdateType updateType
				= B_WRITABLE_FILE_UPDATE_TYPE_ENUM_COUNT;
			bool isDirectory = false;

			Token nextToken = parser._NextToken();
			if (nextToken.type == TOKEN_STRING
				&& nextToken.text == "directory") {
				isDirectory = true;
				nextToken = parser._NextToken();
			}

			if (nextToken.type == TOKEN_STRING) {
				const char* const* end = kWritableFileUpdateTypes
					+ B_WRITABLE_FILE_UPDATE_TYPE_ENUM_COUNT;
				const char* const* found = std::find(kWritableFileUpdateTypes,
					end, nextToken.text);
				if (found == end) {
					throw ParseError(BString("expected an update type"),
						nextToken.pos);
				}
				updateType = (BWritableFileUpdateType)(
					found - kWritableFileUpdateTypes);
			} else if (nextToken.type == TOKEN_ITEM_SEPARATOR
				|| nextToken.type == TOKEN_CLOSE_BRACE) {
				parser._RewindTo(nextToken);
			} else {
				throw ParseError(
					"expected 'included', semicolon, new line or '}'",
					nextToken.pos);
			}

			if (!infos->AddItem(new BGlobalWritableFileInfo(token.text,
					updateType, isDirectory))) {
				throw std::bad_alloc();
			}
		}
	} resolvableExpressionParser(*this, infos);

	_ParseList(resolvableExpressionParser, false);
}


void
BPackageInfo::Parser::_ParseUserSettingsFileInfos(
	UserSettingsFileInfoList* infos)
{
	struct UserSettingsFileInfoParser : public ListElementParser {
		Parser& parser;
		UserSettingsFileInfoList* infos;

		UserSettingsFileInfoParser(Parser& parser,
			UserSettingsFileInfoList* infos)
			:
			parser(parser),
			infos(infos)
		{
		}

		virtual void operator()(const Token& token)
		{
			if (token.type != TOKEN_STRING) {
				throw ParseError("expected string (a settings file path)",
					token.pos);
			}

			BString templatePath;
			bool isDirectory = false;

			Token nextToken = parser._NextToken();
			if (nextToken.type == TOKEN_STRING
				&& nextToken.text == "directory") {
				isDirectory = true;
			} else if (nextToken.type == TOKEN_STRING
				&& nextToken.text == "template") {
				nextToken = parser._NextToken();
				if (nextToken.type != TOKEN_STRING) {
					throw ParseError(
						"expected string (a settings template file path)",
						nextToken.pos);
				}
				templatePath = nextToken.text;
			} else if (nextToken.type == TOKEN_ITEM_SEPARATOR
				|| nextToken.type == TOKEN_CLOSE_BRACE) {
				parser._RewindTo(nextToken);
			} else {
				throw ParseError(
					"expected 'template', semicolon, new line or '}'",
					nextToken.pos);
			}

			if (isDirectory
				? !infos->AddItem(new BUserSettingsFileInfo(token.text, true))
				: !infos->AddItem(new BUserSettingsFileInfo(token.text,
						templatePath))) {
				throw std::bad_alloc();
			}
		}
	} resolvableExpressionParser(*this, infos);

	_ParseList(resolvableExpressionParser, false);
}


void
BPackageInfo::Parser::_ParseUsers(UserList* users)
{
	struct UserParser : public ListElementParser {
		Parser& parser;
		UserList* users;

		UserParser(Parser& parser, UserList* users)
			:
			parser(parser),
			users(users)
		{
		}

		virtual void operator()(const Token& token)
		{
			if (token.type != TOKEN_STRING
				|| !BUser::IsValidUserName(token.text)) {
				throw ParseError("expected a user name", token.pos);
			}

			BString realName;
			BString home;
			BString shell;
			BStringList groups;

			for (;;) {
				Token nextToken = parser._NextToken();
				if (nextToken.type != TOKEN_STRING) {
					parser._RewindTo(nextToken);
					break;
				}

				if (nextToken.text == "real-name") {
					nextToken = parser._NextToken();
					if (nextToken.type != TOKEN_STRING) {
						throw ParseError("expected string (a user real name)",
							nextToken.pos);
					}
					realName = nextToken.text;
				} else if (nextToken.text == "home") {
					nextToken = parser._NextToken();
					if (nextToken.type != TOKEN_STRING) {
						throw ParseError("expected string (a home path)",
							nextToken.pos);
					}
					home = nextToken.text;
				} else if (nextToken.text == "shell") {
					nextToken = parser._NextToken();
					if (nextToken.type != TOKEN_STRING) {
						throw ParseError("expected string (a shell path)",
							nextToken.pos);
					}
					shell = nextToken.text;
				} else if (nextToken.text == "groups") {
					for (;;) {
						nextToken = parser._NextToken();
						if (nextToken.type == TOKEN_STRING
							&& BUser::IsValidUserName(nextToken.text)) {
							if (!groups.Add(nextToken.text))
								throw std::bad_alloc();
						} else if (nextToken.type == TOKEN_ITEM_SEPARATOR
							|| nextToken.type == TOKEN_CLOSE_BRACE) {
							parser._RewindTo(nextToken);
							break;
						} else {
							throw ParseError("expected a group name",
								nextToken.pos);
						}
					}
					break;
				} else {
					throw ParseError(
						"expected 'real-name', 'home', 'shell', or 'groups'",
						nextToken.pos);
				}
			}

			BString templatePath;

			Token nextToken = parser._NextToken();
			if (nextToken.type == TOKEN_STRING
				&& nextToken.text == "template") {
				nextToken = parser._NextToken();
				if (nextToken.type != TOKEN_STRING) {
					throw ParseError(
						"expected string (a settings template file path)",
						nextToken.pos);
				}
				templatePath = nextToken.text;
			} else if (nextToken.type == TOKEN_ITEM_SEPARATOR
				|| nextToken.type == TOKEN_CLOSE_BRACE) {
				parser._RewindTo(nextToken);
			} else {
				throw ParseError(
					"expected 'template', semicolon, new line or '}'",
					nextToken.pos);
			}

			if (!users->AddItem(new BUser(token.text, realName, home, shell,
					groups))) {
				throw std::bad_alloc();
			}
		}
	} resolvableExpressionParser(*this, users);

	_ParseList(resolvableExpressionParser, false);
}


void
BPackageInfo::Parser::_Parse(BPackageInfo* packageInfo)
{
	bool seen[B_PACKAGE_INFO_ENUM_COUNT];
	for (int i = 0; i < B_PACKAGE_INFO_ENUM_COUNT; ++i)
		seen[i] = false;

	const char* const* names = BPackageInfo::kElementNames;

	while (Token t = _NextToken()) {
		if (t.type == TOKEN_ITEM_SEPARATOR)
			continue;

		if (t.type != TOKEN_STRING)
			throw ParseError("expected string (a variable name)", t.pos);

		BPackageInfoAttributeID attribute = B_PACKAGE_INFO_ENUM_COUNT;
		for (int i = 0; i < B_PACKAGE_INFO_ENUM_COUNT; i++) {
			if (names[i] != NULL && t.text.ICompare(names[i]) == 0) {
				attribute = (BPackageInfoAttributeID)i;
				break;
			}
		}

		if (attribute == B_PACKAGE_INFO_ENUM_COUNT) {
			BString error = BString("unknown attribute \"") << t.text << '"';
			throw ParseError(error, t.pos);
		}

		if (seen[attribute]) {
			BString error = BString(names[attribute]) << " already seen!";
			throw ParseError(error, t.pos);
		}

		switch (attribute) {
			case B_PACKAGE_INFO_NAME:
			{
				BString name;
				const char* namePos;
				_ParseStringValue(&name, &namePos);

				int32 errorPos;
				if (!_IsValidResolvableName(name, &errorPos)) {
					throw ParseError("invalid character in package name",
						namePos + errorPos);
				}

				packageInfo->SetName(name);
				break;
			}

			case B_PACKAGE_INFO_SUMMARY:
			{
				BString summary;
				_ParseStringValue(&summary);
				if (summary.FindFirst('\n') >= 0)
					throw ParseError("the summary contains linebreaks", t.pos);
				packageInfo->SetSummary(summary);
				break;
			}

			case B_PACKAGE_INFO_DESCRIPTION:
				_ParseStringValue(&packageInfo->fDescription);
				break;

			case B_PACKAGE_INFO_VENDOR:
				_ParseStringValue(&packageInfo->fVendor);
				break;

			case B_PACKAGE_INFO_PACKAGER:
				_ParseStringValue(&packageInfo->fPackager);
				break;

			case B_PACKAGE_INFO_BASE_PACKAGE:
				_ParseStringValue(&packageInfo->fBasePackage);
				break;

			case B_PACKAGE_INFO_ARCHITECTURE:
				_ParseArchitectureValue(&packageInfo->fArchitecture);
				break;

			case B_PACKAGE_INFO_VERSION:
				_ParseVersionValue(&packageInfo->fVersion, false);
				break;

			case B_PACKAGE_INFO_COPYRIGHTS:
				_ParseStringList(&packageInfo->fCopyrightList);
				break;

			case B_PACKAGE_INFO_LICENSES:
				_ParseStringList(&packageInfo->fLicenseList);
				break;

			case B_PACKAGE_INFO_URLS:
			{
				UrlStringValidator stringValidator;
				_ParseStringList(&packageInfo->fURLList,
					false, false, &stringValidator);
			}
				break;

			case B_PACKAGE_INFO_SOURCE_URLS:
			{
				UrlStringValidator stringValidator;
				_ParseStringList(&packageInfo->fSourceURLList,
					false, false, &stringValidator);
			}
				break;

			case B_PACKAGE_INFO_GLOBAL_WRITABLE_FILES:
				_ParseGlobalWritableFileInfos(
					&packageInfo->fGlobalWritableFileInfos);
				break;

			case B_PACKAGE_INFO_USER_SETTINGS_FILES:
				_ParseUserSettingsFileInfos(
					&packageInfo->fUserSettingsFileInfos);
				break;

			case B_PACKAGE_INFO_USERS:
				_ParseUsers(&packageInfo->fUsers);
				break;

			case B_PACKAGE_INFO_GROUPS:
				_ParseStringList(&packageInfo->fGroups);
				break;

			case B_PACKAGE_INFO_POST_INSTALL_SCRIPTS:
				_ParseStringList(&packageInfo->fPostInstallScripts);
				break;

			case B_PACKAGE_INFO_PROVIDES:
				_ParseResolvableList(&packageInfo->fProvidesList);
				break;

			case B_PACKAGE_INFO_REQUIRES:
				packageInfo->fBasePackage.Truncate(0);
				_ParseResolvableExprList(&packageInfo->fRequiresList,
					&packageInfo->fBasePackage);
				break;

			case B_PACKAGE_INFO_SUPPLEMENTS:
				_ParseResolvableExprList(&packageInfo->fSupplementsList);
				break;

			case B_PACKAGE_INFO_CONFLICTS:
				_ParseResolvableExprList(&packageInfo->fConflictsList);
				break;

			case B_PACKAGE_INFO_FRESHENS:
				_ParseResolvableExprList(&packageInfo->fFreshensList);
				break;

			case B_PACKAGE_INFO_REPLACES:
				_ParseStringList(&packageInfo->fReplacesList, true);
				break;

			case B_PACKAGE_INFO_FLAGS:
				packageInfo->SetFlags(_ParseFlags());
				break;

			default:
				// can never get here
				break;
		}

		seen[attribute] = true;
	}

	// everything up to and including 'provides' is mandatory
	for (int i = 0; i <= B_PACKAGE_INFO_PROVIDES; ++i) {
		if (!seen[i]) {
			BString error = BString(names[i]) << " is not being set anywhere!";
			throw ParseError(error, fPos);
		}
	}
}


/*static*/ inline bool
BPackageInfo::Parser::_IsAlphaNumUnderscore(const BString& string,
	const char* additionalChars, int32* _errorPos)
{
	return _IsAlphaNumUnderscore(string.String(),
		string.String() + string.Length(), additionalChars, _errorPos);
}


/*static*/ inline bool
BPackageInfo::Parser::_IsAlphaNumUnderscore(const char* string,
	const char* additionalChars, int32* _errorPos)
{
	return _IsAlphaNumUnderscore(string, string + strlen(string),
		additionalChars, _errorPos);
}


/*static*/ bool
BPackageInfo::Parser::_IsAlphaNumUnderscore(const char* start, const char* end,
	const char* additionalChars, int32* _errorPos)
{
	for (const char* c = start; c < end; c++) {
		if (!isalnum(*c) && *c != '_' && strchr(additionalChars, *c) == NULL) {
			if (_errorPos != NULL)
				*_errorPos = c - start;
			return false;
		}
	}

	return true;
}


/*static*/ bool
BPackageInfo::Parser::_IsValidResolvableName(const char* string,
	int32* _errorPos)
{
	for (const char* c = string; *c != '\0'; c++) {
		switch (*c) {
			case '-':
			case '/':
			case '<':
			case '>':
			case '=':
			case '!':
				break;
			default:
				if (!isspace(*c))
					continue;
				break;
		}

		if (_errorPos != NULL)
			*_errorPos = c - string;
		return false;
	}
	return true;
}

void
BPackageInfo::Parser::UrlStringValidator::Validate(const BString& urlString,
	const char* pos)
{
	BUrl url(urlString);

	if (!url.IsValid())
		throw ParseError("invalid url", pos);
}


} // namespace BPackageKit
