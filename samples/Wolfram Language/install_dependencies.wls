#!/usr/bin/env wolframscript
(* ::Package:: *)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

print[ a___ ] := WriteString[ "stdout", a, "\n" ];

getInput[ name_String ] :=
    Module[ { val },
        val = Environment[ "INPUT_" <> name ];
        print[ name, "=", val ];
        val
    ];


pacletInstall[ url_String? urlQ ] := Enclose[
    Module[ { installed },
        print[ "Installing paclet from: ", url ];
        installed = PacletInstall[ url, ForceVersionInstall -> True ];
        If[ PacletObjectQ @ installed,
            print[ "Paclet installed to: ", installed[ "Location" ] ],
            print[ "::error:: Paclet installation failed: ", installed ];
            Exit[ 1 ]
        ]
    ],
    Function[
        print[ "::error:: Paclet installation failed: ", url ];
        Exit[ 1 ]
    ]
];


urlQ[ url_String ] := StringQ @ URLParse[ url, "Scheme" ];
urlQ[ ___        ] := False;


toPacletCICDURL[ "latest" ] :=
    Part[
        URLExecute[
            "https://api.github.com/repos/WolframResearch/PacletCICD/releases/latest",
            "RawJSON"
        ],
        "assets",
        1,
        "browser_download_url"
    ];

toPacletCICDURL[ vers_String ] :=
    If[ StringMatchQ[ vers, (DigitCharacter..~~".")...~~(DigitCharacter..) ],
        TemplateApply[
            "https://github.com/WolframResearch/PacletCICD/releases/download/v`1`/Wolfram__PacletCICD-`1`.paclet",
            { vers }
        ],
        TemplateApply[
            "https://github.com/WolframResearch/PacletCICD/archive/refs/heads/`1`.zip",
            vers
        ]
    ];


pacVer = getInput[ "PACLET_CICD_VERSION" ];
pacURL = toPacletCICDURL @ pacVer;

If[ ! StringQ @ pacURL,
    Print[ "Could not determine URL for specified version" ];
    Exit[ 1 ]
];

pacletInstall @ pacURL;

(* :!CodeAnalysis::EndBlock:: *)