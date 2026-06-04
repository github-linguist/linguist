#!/usr/bin/env wolframscript

Begin["WolframLanguageForJupyter`Private`"];

notfound = "configure-jupyter.wls: Jupyter installation on Environment[\"PATH\"] not found.";
isdir = "configure-jupyter.wls: Provided Jupyter binary path is a directory. Please provide the path to the Jupyter binary."
nobin = "configure-jupyter.wls: Provided Jupyter binary path does not exist.";
isdirMath = "configure-jupyter.wls: Provided Wolfram Engine binary path is a directory. Please provide the path to the Wolfram Engine binary."
nobinMath = "configure-jupyter.wls: Provided Wolfram Engine binary path does not exist.";
notadded = "configure-jupyter.wls: An error has occurred. The desired Wolfram Engine is not in \"jupyter kernelspec list.\"";
notremoved = "configure-jupyter.wls: An error has occurred: Wolfram Engine(s) still in \"jupyter kernelspec list.\"";
addconflict = "configure-jupyter.wls: An error has occurred. A Wolfram Engine with the same $VersionNumber of the target Wolfram Engine is in \"jupyter kernelspec list.\" Attempting to overwrite ...";
(* removeconflict = "configure-jupyter.wls: An error has occurred. The Wolfram Engine(s) to be removed is/are not in \"jupyter kernelspec list.\""; *)
removeconflict = "";
nopaclet = "configure-jupyter.wls: WolframLanguageForJupyter paclet source not detected. Are you running the script in the root project directory?";
nolink = "configure-jupyter.wls: Communication with provided Wolfram Engine binary could not be established.";

(*
	Dictionary:
		mathBin/mathBinSession = WolframKernel binary
		kernelspec = Kernel Specification; term used by Jupyter
		notProvidedQ = was a Wolfram Engine Binary explicitly specified?
*)

(* START: Helper symbols  *)

projectHome = If[StringQ[$InputFileName] && $InputFileName != "", DirectoryName[$InputFileName], Directory[]];

(* establishes link with Wolfram Engine at mathBin and evaluates $Version/$VersionNumber *)
(* returns string form *)
getVersionFromKernel[mathBin_String] :=
	Module[{link, res},
		link = 
			LinkLaunch[
				StringJoin[
					{
						"\"",
						mathBin,
						"\" -wstp"
					}
				]
			];
		If[FailureQ[link],
			Return[$Failed];
		];
		(* bleed link *)
		While[LinkReadyQ[link, 0.5], LinkRead[link];];
		LinkWrite[link, Unevaluated[$VersionNumber]];
		res = StringTrim[ToString[LinkRead[link]], "ReturnPacket[" | "]"];
		LinkClose[link];
		If[!StringContainsQ[res, "[" | "]"],
			Return[res];,
			Return[$Failed];
		];
	];

(* determine display name for Jupyter installation from Wolfram Engine $Version/$VersionNumber *)
(* returns {Kernel ID, Display Name} *)
getNames[mathBin_String, notProvidedQ_?BooleanQ] := 
	Module[{version, installDir, (* names, hashedKernelUUID *) versionStr},
		(* if Wolfram Engine binary not provided, just evaluate $Version in the current session *)
		(* otherwise, use MathLink to obtain $Version *)
		If[
			notProvidedQ,
			version = ToString[$VersionNumber];
			installDir = $InstallationDirectory;
			,
			version = Quiet[getVersionFromKernel[mathBin]];
			If[
				FailureQ[version],
				Return[$Failed];
			];
			installDir = mathBin;
		];

		(*

		hashedKernelUUID = StringJoin["wl-script-", Hash[installDir, "SHA", "HexString"]];

		names = StringCases[version, name___ ~~ " for " ~~ ("Mac" | "Microsoft" | "Windows" | "Linux") -> name];
		Return[
			If[Length[names] > 0, 
				{
					ToLowerCase[StringJoin[
						"WolframLanguage-script-",
						StringReplace[First[names], Whitespace -> "-"]
					]],
					StringJoin[
						"Wolfram Language (",
						Capitalize[
							First[names],
							"AllWords"
						],
						") | Script Install"
					]
				}
				,
				{hashedKernelUUID, "Wolfram Language | Script Install"}
			]
		];

		*)

		versionStr = StringTrim[version, "."];
		Return[
			{
				(* Kernel ID *)
				StringJoin["wolframlanguage", versionStr],
				(* Display Name *)
				StringJoin["Wolfram Language ", versionStr]
			}
		];
	];

(* determine symbols related to finding Wolfram Engine and Jupyter installations *)
(* mathBinSession: WolframKernel location for the current session *)
(* fileExt: file extension for executables *)
(* pathSeperator: delimiter for directories on PATH *)
defineGlobalVars[] := 
	Switch[
		$OperatingSystem,
		"Windows",
		mathBinSession = FileNameJoin[{$InstallationDirectory, "wolfram.exe"}];
		fileExt = ".exe";
		pathSeperator = ";";,
		"MacOSX",
		mathBinSession = FileNameJoin[{$InstallationDirectory, "MacOS", "WolframKernel"}];
		fileExt = "";
		pathSeperator = ":";,
		"Unix",
    mathBinSession = FileNameJoin[{$InstallationDirectory, "Executables", "WolframKernel"}];
		fileExt = "";
		pathSeperator = ":";
	];

mathBinSession := (defineGlobalVars[]; mathBinSession);
fileExt := (defineGlobalVars[]; fileExt);
pathSeperator := (defineGlobalVars[]; pathSeperator);

(* a list of directories in PATH *)
splitPath := StringSplit[Environment["PATH"], pathSeperator];

(* restore PATH, if due to a bug, it becomes essentially empty; this is relevant to finding the Jupyter installation *)
(* returns above *)
attemptPathRegeneration[] := If[
		$OperatingSystem === "MacOSX" && FileType["~/.profile"] === File,
		Print["install.wls: Warning: Regenerating PATH ..."];
		SetEnvironment[
			"PATH" -> StringTrim[
						RunProcess[
							$SystemShell,
							"StandardOutput",
							StringJoin[Import["~/.profile", "String"], "\necho $PATH"],
							ProcessEnvironment -> {}
						], 
						"\n"
					]
		];
	];

(* find Jupyter installation path *)
(* returns kernel IDs in Jupyter *)
findJupyterPath[] := 
	SelectFirst[
		splitPath,
		(* check every directory in PATH to see if a Jupyter binary is a member *)
		(FileType[FileNameJoin[{#1, StringJoin["jupyter", fileExt]}]] === File)&
	];

(* get information about installed kernels in Jupyter *)
(* returns kernel IDs in Jupyter *)
getKernels[jupyterPath_String, processEnvironment_] := 
	Module[{json, kernelspecAssoc},
		(* obtain information about "jupyter kernelspec list" in JSON *)
		json = Quiet[ImportString[RunProcess[{jupyterPath, "kernelspec", "list", "--json"}, "StandardOutput", ProcessEnvironment -> processEnvironment], "JSON"]];
		(* transform that JSON information into an Association *)
		kernelspecAssoc = 
			If[
				FailureQ[json],
				Association[],
				Replace[
					json,
					part_List /; AllTrue[part, Head[#1] === Rule &] -> Association @ part, 
					{0, Infinity}
				]
			];
		Return[
			(* if the above process worked, just return the kernel IDs of all the kernelspecs *)
			(* otherwise, return an empty list *)
			If[
				KeyExistsQ[kernelspecAssoc, "kernelspecs"],
				Keys[kernelspecAssoc["kernelspecs"]],
				{}
			]
		];
	];


(* END: Helper symbols  *)

(* main install command *)
(* specs: options \"WolframEngineBinary\" and \"JupyterInstallation\" in an Association, when provided *)
(* removeQ: remove a Jupyter installation or not *)
(* removeAllQ: clear all Jupyter installations or not *)
(* removeQ first, removeAllQ second: "add" is False, False; "remove" is True, False, and "clear" is True, True *)
configureJupyter[specs_Association, removeQ_?BooleanQ, removeAllQ_?BooleanQ] := 
	Module[
		{
			kernelScript,
			retrievedNames, kernelID, displayName,
			notProvidedQ,
			jupyterPath, mathBin,
			fileType,
			processEnvironment,
			baseDir, tempDir,
			wlKernels, (* wlKernelsL(owerCase) *) wlKernelsL,
			commandArgs,
			exitInfo, kernelspecAssoc, kernelspecs,
			conflictMessage, failureMessage
		},

		kernelScript = FileNameJoin[{projectHome, "WolframLanguageForJupyter", "Resources", "KernelForWolframLanguageForJupyter.wl"}];
		(* just check that the REPL script is there *)
		If[
			!(FileType[kernelScript] === File),
			Print[nopaclet];
			Return[$Failed];
		];

		jupyterPath = specs["JupyterInstallation"];
		(* if no Jupyter installation path provided, determine it from PATH *)
		If[
			MissingQ[jupyterPath],
			jupyterPath = findJupyterPath[];
			(* if Jupyter not on PATH, message *)
			If[MissingQ[jupyterPath],
				Print[notfound];
				Return[$Failed];
			];
			jupyterPath = FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}];
		];

		mathBin = 
			Lookup[
				specs,
				"WolframEngineBinary",
				(* if no "WolframEngineBinary" provided, use the session Wolfram Kernel location and set notProvidedQ to True *)
				(notProvidedQ = True; mathBinSession)
			];

		(* check that the Jupyter installation path is a file *)
		If[
			!((fileType = FileType[jupyterPath]) === File),
			Switch[
				fileType,
				Directory,
				Print[isdir];,
				None,
				Print[nobin];
			];
			Return[$Failed];
		];

		{kernelID, displayName} = {"", ""};
		(* if not clearing, check that the Wolfram Engine installation path is a file, and message appropriately *)
		If[
			!(removeQ && removeAllQ),
			If[
				(fileType = FileType[mathBin]) === File,
				(* get the "Kernel ID" and "Display Name" for the new Jupyter kernel *)
				retrievedNames = getNames[mathBin, TrueQ[notProvidedQ]];
				If[FailureQ[retrievedNames], Print[nolink]; Return[$Failed]];
				{kernelID, displayName} = retrievedNames;,
				Switch[
					fileType,
					Directory,
					Print[isdirMath];,
					None,
					Print[nobinMath];
				];
				Return[$Failed];
			];
		];

		(* as an association for 11.3 compatibility *)
			processEnvironment = Association[GetEnvironment[]];
			processEnvironment["PATH"] = StringJoin[Environment["PATH"], pathSeperator, DirectoryName[jupyterPath]];

		(* list of kernels in Jupyter to perform an action on *)
		wlKernels = {kernelID};
		tempDir = "";
		(* if adding, ...*)
		(* otherwise, when removing or clearing, ...*)
		If[
			!removeQ,
			failureMessage = notadded;
			conflictMessage = addconflict;

			(* create staging directory for files needed to register a kernel with Jupyter *)
			tempDir = CreateDirectory[
				FileNameJoin[{
					projectHome,
					CreateUUID[],
					(* removing this would cause every evalution of addKernelToJupyter adds a new kernel with a different uuid *)
					kernelID
				}], CreateIntermediateDirectories -> True
			];

			(* export a JSON file to the staging directory that contains all the relevant information on how to run the kernel *)
			Export[
				FileNameJoin[{tempDir, "kernel.json"}], 
				Association[
					"argv" -> {mathBin, "-script", kernelScript, "{connection_file}", "ScriptInstall" (* , "-noprompt" *)},
					"display_name" -> displayName,
					"language" -> "Wolfram Language"
				]
			];

			(* create a list of arguments that directs Jupyter to install from the staging directory *)
			commandArgs = {jupyterPath, "kernelspec", "install", "--user", tempDir};,
			failureMessage = notremoved;
			conflictMessage = removeconflict;
			(* create a list of arguments that directs Jupyter to remove ... *)
			commandArgs = {jupyterPath, "kernelspec", "remove", "-f",
				If[
					!removeAllQ,
					(* just the specified kernel *)
					kernelID,
					(* all Wolfram Language Jupyter kernels *)
					(* select from all kernel IDs in Jupyter those that match the form used by this install *)
					Sequence @@ (wlKernels = Select[getKernels[jupyterPath, processEnvironment], StringMatchQ[#1, (* ("WolframLanguage-" | "wl-") *) "WolframLanguage" ~~ ___, IgnoreCase -> True] &])
				]
			}
		];
		(* if no kernels to act on, quit *)
		If[Length[wlKernels] == 0, Return[];];
		wlKernelsL = ToLowerCase /@ wlKernels;

		(* for error detection, get a snapshot of kernels before the action is performed *)
		kernelspecs = getKernels[jupyterPath, processEnvironment];
		(* when adding, if there is a kernel with the same id already in Jupyter, it will be replaced; thus, message, but continue *)
		If[Xor[removeQ, SubsetQ[kernelspecs, wlKernelsL]], Print[conflictMessage];];

		(* perform the action *)
		exitInfo = RunProcess[commandArgs, All, ProcessEnvironment -> processEnvironment];
		(* remove temporary directory if it was created *)
		If[StringLength[tempDir] > 0, DeleteDirectory[DirectoryName[tempDir], DeleteContents -> True]];

		(* get list of kernels after the action was performed *)
		kernelspecs = getKernels[jupyterPath, processEnvironment];
		(* message about success with respect to the action that was performed *)
		If[
			!Xor[removeQ, SubsetQ[kernelspecs, wlKernelsL]],
			Print[failureMessage];
			Print["configure-jupyter.wls: See below for the message that Jupyter returned when attempting to add the Wolfram Engine."];
			Print[StringTrim[exitInfo["StandardError"], Whitespace]];
			Return[$Failed];
		];
	];

(* checking RunProcess ..., and messaging appropriately *)
If[
	FailureQ[RunProcess[$SystemShell, All, ""]],
	(* maybe remove *)
	If[
		MemberQ[$CommandLine, "-script"],
		Print["configure-jupyter.wls: Please use -file instead of -script in WolframScript."];
		Quit[];
		,
		Print["configure-jupyter.wls: An unknown error has occurred."];
		attemptPathRegeneration[];
		If[FailureQ[RunProcess[$SystemShell, All, ""]], Quit[]];
	];
];

defineGlobalVars[];

(* maybe remove *)
(* checking PATH ..., and messaging appropriately *)
If[
	Length[splitPath] == 1,
	Print["configure-jupyter.wls: Warning: This script has encountered a very small PATH environment variable."];
	Print["configure-jupyter.wls: Warning: This can occur due to a possible WolframScript bug."];
	attemptPathRegeneration[];
];


(* START: Building usage message *)

templateJupyterPath = StringJoin["\"", FileNameJoin[{"path", "to", "Jupyter-binary"}], "\""];
templateWLPath = StringJoin["\"", FileNameJoin[{"", "absolute", "path", "to", "Wolfram-Engine-binary--not-wolframscript"}], "\""]; 

(* helpMessage = StringJoin[
				"configure-jupyter.wls add [", templateJupyterPath, "]\n",
				"configure-jupyter.wls adds a Wolfram Engine to a Jupyter binary on PATH, or optional provided Jupyter binary path\n",
				"configure-jupyter.wls add ", templateJupyterPath, " ", templateWLPath, "\n",
				"\tadds the provided absolute Wolfram Engine binary path to the provided Jupyter binary path\n",
				"configure-jupyter.wls remove [", templateJupyterPath ,"]\n",
				"\tremoves any Wolfram Engines found on a Jupyter binary on PATH, or optional provided Jupyter binary path"
			]; *)

helpMessage = StringJoin[
				"configure-jupyter.wls add [", templateWLPath, "]\n",
				"\tadds a Wolfram Engine, either attached to the current invocation, or at the provided absolute Wolfram Engine binary path, to a Jupyter binary on PATH\n",
				"configure-jupyter.wls add ", templateWLPath, " ", templateJupyterPath, "\n",
				"\tadds the provided absolute Wolfram Engine binary path to the provided Jupyter binary path\n",
				"configure-jupyter.wls remove [", templateWLPath ,"]\n",
				"\tremoves the Wolfram Engine, either attached to the current invocation, or at the provided absolute Wolfram Engine binary path, from a Jupyter binary on PATH\n",
				"configure-jupyter.wls remove ", templateWLPath, " ", templateJupyterPath, "\n",
				"\tremoves the provided absolute Wolfram Engine binary path from the provided Jupyter binary path\n",
				"configure-jupyter.wls clear [", templateJupyterPath ,"]\n",
				"\tremoves all Wolfram Engines found on a Jupyter binary on PATH, or optional provided Jupyter binary path\n",
				"configure-jupyter.wls build\n",
				"\tbuilds the WolframLanguageForJupyter paclet in the project directory"
			];

(* END: Building usage message *)


(* based off of the script invocation, use configureJupyter or PackPaclet; or display help message *)
If[
	Length[$ScriptCommandLine] < 2 ||
		Length[$ScriptCommandLine] > 4 ||
			$ScriptCommandLine[[2]] === "help",
	Print[helpMessage];
	,
	Switch[
		$ScriptCommandLine[[2]],
		"add" | "Add",
		command = {False, False};,
		"remove" | "Remove",
		command = {True, False};,
		"clear" | "Clear",
		command = {True, True};,
		"build",
		PackPaclet["WolframLanguageForJupyter"];
		Quit[];
		,
		_,
		Print[helpMessage];
	];

	configureJupyter[
		Switch[
			Length[$ScriptCommandLine],
			4,
			Association[
				"WolframEngineBinary" -> $ScriptCommandLine[[3]],
				"JupyterInstallation" -> $ScriptCommandLine[[4]]
			],
			3,
			If[command === {True, True},
				Association["JupyterInstallation" -> $ScriptCommandLine[[3]]],
				Association["WolframEngineBinary" -> $ScriptCommandLine[[3]]]
			],
			2,
			Association[]
		],
		Sequence @@ command
	];
];

End[];
