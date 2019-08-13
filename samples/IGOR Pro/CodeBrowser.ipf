#pragma rtGlobals=3
#pragma version=1.3
#pragma IgorVersion = 6.3.0
#pragma IndependentModule=CodeBrowserModule

#include "CodeBrowser_gui"
#include <Resize Controls>

// Copyright (c) 2019, () byte physics support@byte-physics.de
// All rights reserved.
//
// This source code is licensed under the BSD 3-Clause license found in the
// LICENSE file in the root directory of this source tree.
//
// source: https://github.com/byte-physics/igor-code-browser/blob/6a1497795f606d9d837d4012cbb4bbc481af3683/procedures/CodeBrowser.ipf

Menu "CodeBrowser"
	// CTRL+0 is the keyboard shortcut
	"Open/0", /Q, CodeBrowserModule#CreatePanel()
	"Reset", /Q, CodeBrowserModule#ResetPanel()
End

// Markers for the different listbox elements
StrConstant strConstantMarker = "\\W539"
StrConstant constantMarker    = "\\W534"

Function addDecoratedStructure(text, declWave, lineWave, [parseVariables])
	WAVE/T text
	WAVE/T declWave
	WAVE/D lineWave
	Variable parseVariables
	if(paramIsDefault(parseVariables) | parseVariables != 1)
		parseVariables = 1 // added for debugging
	endif

	variable numLines, idx, numEntries, numMatches
	string procText, reStart, name, StaticKeyword

	Wave/T helpWave = getHelpWave()

	// regexp: match case insensitive (?i) leading spaces don't matter. optional static statement. search for structure name which contains no spaces. followed by an optional space and nearly anything like inline comments
	// help for regex on https://regex101.com/
	reStart = "^(?i)[[:space:]]*((?:static[[:space:]])?)[[:space:]]*structure[[:space:]]+([^[:space:]\/]+)[[:space:]\/]?.*"
	Grep/Q/INDX/E=reStart text
	Wave W_Index
	Duplicate/FREE W_Index wavStructureStart
	KillWaves/Z W_Index
	KillStrings/Z S_fileName
	WaveClear W_Index
	if(!V_Value) // no matches
		return 0
	endif
	numMatches = DimSize(wavStructureStart, 0)

	// optionally analyze structure elements
	if(parseVariables)
		// regexp: match case insensitive endstructure followed by (space or /) and anything else or just a lineend
		// does not match endstructure23 but endstructure//
		Grep/Q/INDX/E="^(?i)[[:space:]]*(?:endstructure(?:[[:space:]]|\/).*)|endstructure$" text
		Wave W_Index
		Duplicate/FREE W_Index wavStructureEnd
		KillWaves/Z W_Index
		KillStrings/Z S_fileName
		WaveClear W_Index
		if(numMatches != DimSize(wavStructureEnd, 0))
			numMatches = 0
			return 0
		endif
	endif

	numEntries = DimSize(declWave, 0)
	Redimension/N=(numEntries + numMatches, -1) declWave, lineWave, helpWave

	for(idx = numEntries; idx < (numEntries + numMatches); idx +=1)
		SplitString/E=reStart text[wavStructureStart[(idx - numEntries)]], StaticKeyword, name
		declWave[idx][0] = createMarkerForType(LowerStr(StaticKeyword) + "structure") // no " " between static and structure needed
		declWave[idx][1] = name

		// optionally parse structure elements
		if(parseVariables)
			Duplicate/FREE/R=[(wavStructureStart[(idx - numEntries)]),(wavStructureEnd[(idx - numEntries)])] text, temp
			declWave[idx][1] += getStructureElements(temp)
			WaveClear temp
		endif

		lineWave[idx] = wavStructureStart[(idx - numEntries)]
	endfor

	WaveClear wavStructureStart, wavStructureEnd
End

/// @brief Return the text of the given procedure as free wave splitted at the EOL
static Function/WAVE getProcedureTextAsWave(module, procedureWithoutModule)
	string module, procedureWithoutModule

	string procText
	variable numLines

	// get procedure code
	procText = getProcedureText("", 0, module, procedureWithoutModule)

#if (IgorVersion() >= 7.0)
	return ListToTextWave(procText, "\r")
#else
	numLines = ItemsInList(procText, "\r")

	if(numLines == 0)
		Make/FREE/N=(numLines)/T wv
		return wv
	endif

	Make/FREE/N=(numLines)/T wv = StringFromList(p, procText, "\r")
	return wv
#endif
End

// add basic html
Function/S AddHTML(context)
	string context

	string line, html, re
	string str0, str1, str2, str3, str4
	variable n, lines

	html = ""
	lines = ItemsInList(context, "\r")
	for(n = 0; n < lines; n += 1)
		line = StringFromList(n, context, "\r")
		re = "\s*([\/]{2,})\s?(.*)"
		SplitString/E=(re) line, str0, str1
		if(V_flag != 2)
			break
		endif
		line = str1
		if(strlen(str0) == 3) // Doxygen comments
			re = "(?i)(.*@param(?:\[(?:in|out)\])?\s+)(\w+)(\s.*)"
			SplitString/E=(re) line, str0, str1, str2
			if(V_flag == 3)
				line  = str0
				line += "<b>" + str1 + "</b> "
				line += str2
			endif
			re = "(?i)(.*)@(\w+)(\s.*)"
			SplitString/E=(re) line, str0, str1, str2
			if(V_flag == 3)
				line  = str0
				line += "<b>@</b><i>" + str1 + "</i>"
				line += str2
			endif
		endif
		html += line + "<br>"
	endfor
	html = RemoveEnding(html, "<br>")
	html = "<code>" + html + "</code>"

	return html
End

// get code of procedure in module
//
// see `DisplayHelpTopic("ProcedureText")`
//
// @param funcName       Name of Function. Leave blank to get full procedure text
// @param linesOfContext line numbers in addition to the function definition. Set to @c 0 to return only the function.
//                       set to @c -1 to return lines before the procedure that are not part of the preceding macro or function
// @param module         independent module
// @param procedure      procedure without module definition
// @return multi-line string with function definition
Function/S getProcedureText(funcName, linesOfContext, module, procedure)
	string funcName, module, procedure
	variable linesOfContext

	if(!isProcGlobal(module))
		debugPrint(procedure + " is not in ProcGlobal")
		procedure = procedure + " [" + module + "]"
	endif

	return ProcedureText(funcName, linesOfContext, procedure)
End

// Returns a list of independent modules
// Includes ProcGlobal but skips all WM modules and the current module in release mode
Function/S getModuleList()
	String moduleList

	moduleList = IndependentModuleList(";")
	moduleList = ListMatch(moduleList, "!WM*", ";") // skip WM modules
	moduleList = ListMatch(moduleList, "!RCP*", ";") // skip WM's Resize Controls modul
	String module = GetIndependentModuleName()

	moduleList = "ProcGlobal;" + SortList(moduleList)

	return moduleList
End

// get help wave: after parsing the function comment is stored here
//
// Return refrence to (text) Wave/T
Function/Wave getHelpWave()
	DFREF dfr = createDFWithAllParents(pkgFolder)
	WAVE/Z/T/SDFR=dfr wv = $helpWave

	if(!WaveExists(wv))
		Make/T/N=(128, 2) dfr:$helpWave/Wave=wv
	endif

	return wv
End

static Structure procedure
	String id
	Variable row
	String name
	String module
	String fullName
Endstructure

/// @brief compile all procedures
Function compile()
	Execute/P/Z/Q "COMPILEPROCEDURES "
End
