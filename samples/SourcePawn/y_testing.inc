/**--------------------------------------------------------------------------**\
					====================================
					 y_testing - Run unit tests easilly 
					====================================
Description:
	Runs any functions named as tests when the Testing_Run function is called.
Legal:
	Version: MPL 1.1
	
	The contents of this file are subject to the Mozilla Public License Version 
	1.1 (the "License"); you may not use this file except in compliance with 
	the License. You may obtain a copy of the License at 
	http://www.mozilla.org/MPL/
	
	Software distributed under the License is distributed on an "AS IS" basis,
	WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
	for the specific language governing rights and limitations under the
	License.
	
	The Original Code is the YSI testing include.
	
	The Initial Developer of the Original Code is Alex "Y_Less" Cole.
	Portions created by the Initial Developer are Copyright (C) 2011
	the Initial Developer. All Rights Reserved.
	
	Contributors:
		ZeeX, koolk, JoeBullet/Google63, g_aSlice/Slice
	
	Thanks:
		JoeBullet/Google63 - Handy arbitrary ASM jump code using SCTRL.
		ZeeX - Very productive conversations.
		koolk - IsPlayerinAreaEx code.
		TheAlpha - Danish translation.
		breadfish - German translation.
		Fireburn - Dutch translation.
		yom - French translation.
		50p - Polish translation.
		Zamaroht - Spanish translation.
		Dracoblue, sintax, mabako, Xtreme, other coders - Producing other modes
			for me to strive to better.
		Pixels^ - Running XScripters where the idea was born.
		Matite - Pestering me to release it and using it.
	
	Very special thanks to:
		Thiadmer - PAWN, whose limits continue to amaze me!
		Kye/Kalcor - SA:MP.
		SA:MP Team past, present and future - SA:MP.
	
Version:
	1.0
Changelog:
	16/02/12:
		Added better error reporting (less verbose passes).
		Removed "Tests:".
		Added player requiring tests.
	25/10/10:
		Integrated in to YSI.
	06/08/10:
		First version
</remarks>
\**--------------------------------------------------------------------------**/

/*#if defined _inc_a_samp || defined _samp_included || defined _inc_y_version
	#error y_testing must be the VERY FIRST include in your mode if being used.
#endif

// Now we redefined "native" to do something clever!
// "IsPlayerAttachedObjectSlotUsed" is an issue for us...

// "y_renative" allows native functions to be declared without constantly trying
// to redefine it.
#include "internal\y_renative"

#define native%0(%1); _YSI_NO_TAG_NATIVE:_YSI_RE_NATIVE<%0@|||%1|||%0>_YSI_RE_STOCK<%0|||%1|||_:%0@>

//stock%0(%1){printf(_:_YSI_NO_SQUARE:"native "#%0" called");return _:%0@(%1);}

#define _YSI_RE_STOCK<%0|||%1|||%2> stock%0(%1){print(_:_YSI_NO_SQUARE:"native "#%0" called");return %2(%1);}

#define _YSI_NO_SQUARE:%0);%1(%2[%3]%4);} _YSI_NO_SQUARE:%0);%1(%2%4);}

//#define _YSI_NO_TAG_NATIVE:_YSI_RE_NATIVE<%0:%1|||%2|||%3:%4>%5{%6;%7:%8:%9} _YSI_RE_NATIVE<%1|||%2|||%4>%5{%6;return%8:%9}
#define _YSI_NO_TAG_NATIVE:_YSI_RE_NATIVE<%0:%1|||%2|||%3:%4>_YSI_RE_STOCK<%5:%6|||%7|||_:%8:%9> _YSI_RE_NATIVE<%1|||%2|||%4>_YSI_RE_STOCK<%5:%6|||%7|||%8:%9>*/

#include <a_samp>

#if defined YSI_TESTS
	#if defined INCLUDE_TESTS
		#error Incompatible testing options (YSI_TESTS + INCLUDE_TESTS)
	#endif
	#if !defined RUN_TESTS
		#define RUN_TESTS
	#endif
#endif

#if defined INCLUDE_TESTS
	#define RUN_TESTS
	#define Debug_PrintT va_printf
#elseif defined RUN_TESTS
	#define _AUTO_RUN_TESTS
	#define Debug_PrintT va_printf
#else
	#if _DEBUG > 0 || defined _YSI_SPECIAL_DEBUG
		#define RUN_TESTS
		#define _AUTO_RUN_TESTS
		//#if defined _YSI_SPECIAL_DEBUG
		//	#define TP printf
		//	//#define TC(%0); %0
		#define Debug_PrintT va_printf
		//#else
		//	//#define TP P:0
		//	//#define TC C:0
		//#endif
	#else
		#define Debug_PrintT(%0);
	//	#define Tests:%1() stock bool:Tests_@%1()
	//	#define Test:%1() stock bool:Test_@%1()
	//	#define TestInit:%1() stock Init_@%1()
	//	#define TestClose:%1() stock Shut_@%1()
	#endif
#endif

#define Y_TESTING_DIALOG_ID (0x7974) // "yt"

#define TEST_TRUE(%0)  Testing_Test(!!%0)
#define TEST_FALSE(%0) Testing_Test(!%0)
#define TEST_NULL(%0)  Testing_Test(0 == %0)
#define TEST_NOT_NULL(%0)  Testing_Test(0 != %0)
#define TEST_N(%0,%1)  Testing_Test(%1 == %0)

#define TEST_TRUE_EX(%0,%2)  Testing_Test(bool:(%0), (%2))
#define TEST_FALSE_EX(%0,%2) Testing_Test(!(%0), (%2))
#define TEST_NULL_EX(%0,%2)  Testing_Test((%0) == 0, (%2))
#define TEST_N_EX(%0,%1,%2)  Testing_Test((%0) == (%1), (%2))

#define TEST_MSG "\2\2\2\2\2"
#define TEST_FAILED "FAIL:"
#define TEST_PASSED "PASS!"
//#define TEST_FAILED "*** Test Failed:"
//#define TEST_PASSED "*** Test Passed:"

#define _Y_TESTEQ(%0) "\"%0\"")
#define _Y_TESTDQ:_Y_TESTEQ(%0"%1"%2) _Y_TESTDQ:_Y_TESTEQ(%0\x22;%1\x22;%2)
//#define _Y_TESTCB:_Y_TESTDQ:_Y_TESTEQ(%0)%1) _Y_TESTCB:_Y_TESTDQ:_Y_TESTEQ(%0\x29;%1)
#define _Y_TESTCB:_Y_TESTDQ:_Y_TESTEQ(%0)%1) _Y_TESTCB:_Y_TESTDQ:_Y_TESTEQ(%0\x29;%1)
#define _Y_TESTOB:_Y_TESTCB:_Y_TESTDQ:_Y_TESTEQ(%0(%1) _Y_TESTOB:_Y_TESTCB:_Y_TESTDQ:_Y_TESTEQ(%0\x28;%1)

#define ASSERT(%0) Testing_Test(%0,TEST_MSG" %s", _Y_TESTOB:_Y_TESTCB:_Y_TESTDQ:_Y_TESTEQ(%0)
#define ASSERT_TRUE(%0) ASSERT(!!%0) //Testing_Test(!!%0,__name)
#define ASSERT_FALSE(%0) ASSERT(!%0) //Testing_Test(!%0,__name)
#define ASSERT_NULL(%0) ASSERT(0 == %0) //Testing_Test(0==%0,__name)
#define ASSERT_N:%1(%0) ASSERT(%1 == %0) //Testing_Test(%1==%0,__name)

#define ASK(%0) Testing_Ask(playerid,%0)

static stock
	YSI_g_sFailMessage[512],
	YSI_g_sPlayer = cellmax,
	bool:YSI_g_sAsked,
	YSI_g_sTests,
	YSI_g_sFails;

#if defined RUN_TESTS
	#define Test:%1() forward bool:Tezt_@%1(); public bool:Tezt_@%1() for(new string:__name[]=#%1,bool:__once=(printf("*** Test %s start", __name) || TRUE);__once;__once=(printf(" ") && FALSE))
	//#define Tests:%1() forward bool:Test_@%1(); public bool:Test_@%1() for(new string:__name[]=#%1,bool:__once=(printf("*** Test %s start", __name) || TRUE);__once;__once=(printf(" ") && FALSE))
	#define TestInit:%1() forward Init_@%1(); public Init_@%1() for(new string:__name[]=#%1,bool:__once=TRUE;__once;__once=(printf(" ", __name) && FALSE))
	#define TestClose:%1() forward Shut_@%1(); public Shut_@%1() for(new string:__name[]=#%1,bool:__once=TRUE;__once;__once=(printf(" ", __name) && FALSE))
	
	#define PTest:%1(%2) forward bool:Tezp_@%1(%2); public bool:Tezp_@%1(%2) for(new string:__name[]=#%1,bool:__once=(printf("*** Player Test %s start", __name) || TRUE);__once;__once=(printf(" ") && FALSE))
	//#define Tests:%1() forward bool:Test_@%1(); public bool:Test_@%1() for(new string:__name[]=#%1,bool:__once=(printf("*** Test %s start", __name) || TRUE);__once;__once=(printf(" ") && FALSE))
	#define PTestInit:%1(%2) forward Inip_@%1(%2); public Inip_@%1(%2) for(new string:__name[]=#%1,bool:__once=TRUE;__once;__once=(printf(" ", __name) && FALSE))
	#define PTestClose:%1(%2) forward Shup_@%1(%2); public Shup_@%1(%2) for(new string:__name[]=#%1,bool:__once=TRUE;__once;__once=(printf(" ", __name) && FALSE))
#else
	#define Test:%1() stock bool:Test_@%1()
	#define TestInit:%1() stock Init_@%1()
	#define TestClose:%1() stock Shut_@%1()
	
	#define PTest:%1(%2) stock bool:Test_@%1(%2)
	#define PTestInit:%1(%2) stock Init_@%1(%2)
	#define PTestClose:%1(%2) stock Shut_@%1(%2)
#endif

// These all need to come AFTER the types are defined in case the have tests.
#include "internal\y_version"

#include "y_amx"
#include "internal\y_shortfunc"
#include "y_debug"

#include "internal\y_natives"

#include "y_va"

/**--------------------------------------------------------------------------**\
<summary>Testing_Ask</summary>
<param name="playerid">Player to ask a question to.</param>
<param name="str[]">What to ask.</param>
<param name="va_args<>">Additional data.</param>
<returns>
	-
</returns>
<remarks>
	Calls a dialog to ask the player if the given test actually passed.
</remarks>
\**--------------------------------------------------------------------------**/

stock Testing_Ask(playerid, str[] = "", va_args<>)
{
	va_format(YSI_g_sFailMessage, sizeof (YSI_g_sFailMessage), str, va_start<2>);
	ShowPlayerDialog(playerid, Y_TESTING_DIALOG_ID, DIALOG_STYLE_MSGBOX, "Did the test pass?", YSI_g_sFailMessage, "Yes", "No");
	YSI_g_sAsked = true;
}

stock Testing_Test(bool:x, str[] = "", va_args<>)
{
	P:3("Testing_Test called: %i, \"%s\"", x, str);
	++YSI_g_sTests;
	if (!x)
	{
		++YSI_g_sFails;
		if (numargs() == 2)
		{
			P:T(TEST_FAILED " %s", va_start<1>);
			//printf("*** Test failed: %s", str);
		}
		else
		{
			if (str[0] == '\2')
			{
				strunpack(str, !TEST_FAILED, 6);
				str[5] = ' ';
			}
			P:T(str, va_start<2>);
		}
	}
	#if defined TEST_SHOW_PASSES
		else /*if (numargs() == 2)
		{
			P:T(TEST_PASSED " %s", va_start<1>);
		}
		else
		{
			P:T(str, va_start<2>);
			if (str[0] == '\2')
			{
				strunpack(str, !TEST_PASSED, 17);
				str[16] = ' ';
			}
			printf(str);
			P:T(str, va_start<2>);*/
		{
			P:T(TEST_PASSED, va_start<2>);
		}
	#endif
}

/**--------------------------------------------------------------------------**\
<summary>Testing_Run</summary>
<param name="&tests">Number of tests run.</param>
<param name="&fails">Number of tests which failed.</param>
<param name="buffer[33]">The name of the first test which failed.</param>
<returns>
	Wether all tests were sucessful or not.
</returns>
<remarks>
	-

native Testing_Run(&tests, &fails, buffer[33] = "");

</remarks>
\**--------------------------------------------------------------------------**/

stock bool:Testing_Run(&tests, &fails, lastfail[33] = "", bool:p = false)
{
	P:3("bool:Testing_Run called: %i, %i, \"%s\", %i", tests, fails, lastfail, p);
	#pragma unused p, lastfail
	#if defined RUN_TESTS
		P:2("Testing_Run() called");
		new
			idx,
			buffer[32];
		while ((idx = AMX_GetPublicName(idx, buffer, "Tezt_@")))
		{
			strunpack(buffer, buffer);
			//++YSI_g_sTests;
			// Call the setup function if there is one.
			buffer[0] = 'I';
			buffer[1] = 'n';
			buffer[2] = 'i';
			buffer[3] = 't';
			CallLocalFunction(buffer, "");
			// Call the test.
			buffer[0] = 'T';
			buffer[1] = 'e';
			buffer[2] = 'z';
			buffer[3] = 't';
			fails = YSI_g_sFails;
			P:5("Testing_Run(): Calling %s", buffer[6]);
			CallLocalFunction(buffer, "");
			#if !defined TEST_SHOW_PASSES
				if (YSI_g_sFails == fails)
				{
					printf(TEST_PASSED);
					printf(" ");
				}
			#endif
			/*if (YSI_g_sFails != fails)
			{
				if (YSI_g_sFails)
				{
					++YSI_g_sFails;
				}
				else
				{
					fails = 0;
					// Copy the string over.
					while ((lastfail[fails] = buffer[fails + 6])) ++fails;
					YSI_g_sFails = 1;
				}
				//C:1(if (p) printf("*** Test failed: %s", buffer[fails + 6]););
			}*/
			// Call the shutdown function if there is one.
			buffer[0] = 'S';
			buffer[1] = 'h';
			buffer[2] = 'u';
			buffer[3] = 't';
			CallLocalFunction(buffer, "");
		}
		tests = YSI_g_sTests;
		fails = YSI_g_sFails;
		return fails == 0;
	#else
		#pragma unused tests, fails, lastfail
		return true;
	#endif
}

/**--------------------------------------------------------------------------**\
<summary>Testing_Player</summary>
<param name="playerid">Player to test on.</param>
<param name="&idx">Next test to run.</param>
<param name="&tests">Number of tests run.</param>
<param name="&fails">Number of tests which failed.</param>
<param name="buffer[33]">The name of the first test which failed.</param>
<returns>
	Wether all tests were sucessful or not.
</returns>
<remarks>
	-

native Testing_Run(&tests, &fails, buffer[33] = "");

</remarks>
\**--------------------------------------------------------------------------**/

stock bool:Testing_Player(playerid, &idx, &tests, &fails, lastfail[33] = "", bool:p = false)
{
	P:3("bool:Testing_Player called: %i, %i, %i, %i, \"%s\", %i", playerid, idx, tests, fails, lastfail, p);
	#pragma unused p
	#if defined RUN_TESTS
		P:2("Testing_Player() called");
		new
			//idx,
			buffer[32];
		//while ((idx = AMX_GetPublicName(idx, buffer, "Tezt_@")))
		if ((idx = AMX_GetPublicName(idx, buffer, "Tezp_@")))
		{
			strunpack(buffer, buffer);
			//++YSI_g_sTests;
			// Call the setup function if there is one.
			buffer[0] = 'I';
			buffer[1] = 'n';
			buffer[2] = 'i';
			buffer[3] = 'p';
			CallLocalFunction(buffer, "");
			// Call the test.
			buffer[0] = 'T';
			buffer[1] = 'e';
			buffer[2] = 'z';
			buffer[3] = 'p';
			fails = YSI_g_sFails;
			P:5("Testing_Player(): Calling %s", buffer[6]);
			CallLocalFunction(buffer, "");
			#if !defined TEST_SHOW_PASSES
				if (YSI_g_sFails == fails)
				{
					printf(TEST_PASSED);
					printf(" ");
				}
			#endif
			/*if (YSI_g_sFails != fails)
			{
				if (YSI_g_sFails)
				{
					++YSI_g_sFails;
				}
				else
				{
					fails = 0;
					// Copy the string over.
					while ((lastfail[fails] = buffer[fails + 6])) ++fails;
					YSI_g_sFails = 1;
				}
				//C:1(if (p) printf("*** Test failed: %s", buffer[fails + 6]););
			}*/
			// Call the shutdown function if there is one.
			//buffer[0] = 'S';
			//buffer[1] = 'h';
			//buffer[2] = 'u';
			//buffer[3] = 'p';
			//CallLocalFunction(buffer, "");
		}
		tests = YSI_g_sTests;
		fails = YSI_g_sFails;
		return fails == 0;
	#else
		#pragma unused tests, fails, lastfail
		return true;
	#endif
}

#if defined RUN_TESTS
	/*#define Test:%1() forward bool:Tezt_@%1(); public bool:Tezt_@%1() for(new string:__name[]=#%1,bool:__once=(printf("*** Test %s start", __name) || TRUE);__once;__once=(printf(" ") && FALSE))
	//#define Tests:%1() forward bool:Test_@%1(); public bool:Test_@%1() for(new string:__name[]=#%1,bool:__once=(printf("*** Test %s start", __name) || TRUE);__once;__once=(printf(" ") && FALSE))
	#define TestInit:%1() forward Init_@%1(); public Init_@%1() for(new string:__name[]=#%1,bool:__once=TRUE;__once;__once=(printf(" ", __name) && FALSE))
	#define TestClose:%1() forward Shut_@%1(); public Shut_@%1() for(new string:__name[]=#%1,bool:__once=TRUE;__once;__once=(printf(" ", __name) && FALSE))
	
	#define PTest:%1(%2) forward bool:Tezp_@%1(%2); public bool:Tezp_@%1(%2) for(new string:__name[]=#%1,bool:__once=(printf("*** Player Test %s start", __name) || TRUE);__once;__once=(printf(" ") && FALSE))
	//#define Tests:%1() forward bool:Test_@%1(); public bool:Test_@%1() for(new string:__name[]=#%1,bool:__once=(printf("*** Test %s start", __name) || TRUE);__once;__once=(printf(" ") && FALSE))
	#define PTestInit:%1(%2) forward Inip_@%1(%2); public Inip_@%1(%2) for(new string:__name[]=#%1,bool:__once=TRUE;__once;__once=(printf(" ", __name) && FALSE))
	#define PTestClose:%1(%2) forward Shup_@%1(%2); public Shup_@%1(%2) for(new string:__name[]=#%1,bool:__once=TRUE;__once;__once=(printf(" ", __name) && FALSE))*/
	
	#if defined _AUTO_RUN_TESTS
		#if !defined FILTERSCRIPT
			// Hook main in gamemodes.
			main()
			{
				// Disable error messages (as we're likely to generate them).
				state ysi_debug : off;
				CallLocalFunction("Testing_main", "");
				new
					tests,
					fails;
				printf(" ");
				printf("  ||===================||  ");
				printf("  || STARTING TESTS... ||  ");
				printf("  ||===================||  ");
				printf(" ");
				Testing_Run(tests, fails, _, true);
				printf("*** Tests: %d, Fails: %d", tests, fails);
				if (!fails)
				{
					printf(" ");
					printf("  ||===================||  ");
					printf("  || ALL TESTS PASSED! ||  ");
					printf("  ||===================||  ");
					printf(" ");
				}
				state ysi_debug : on;
			}
			
			#define main forward Testing_main(); public Testing_main
			
			Testing_Next(playerid)
			{
				new
					buffer[32];
				for ( ; ; )
				{
					new
						fails = YSI_g_sFails;
					// Get the last test (nicely fails for cellmax).
					if ((YSI_g_sPlayer = AMX_GetPublicName(YSI_g_sPlayer, buffer, "Tezp_@")))
					{
						strunpack(buffer, buffer);
						// Call the shutdown function if there is one.
						buffer[0] = 'S';
						buffer[1] = 'h';
						buffer[2] = 'u';
						buffer[3] = 'p';
						CallLocalFunction(buffer, "i", playerid);
					}
					// Get the new test, but don't store the index.
					if (AMX_GetPublicName(YSI_g_sPlayer, buffer, "Tezp_@"))
					{
						YSI_g_sAsked = false;
						//++YSI_g_sTests;
						strunpack(buffer, buffer);
						// Call the setup function if there is one.
						buffer[0] = 'I';
						buffer[1] = 'n';
						buffer[2] = 'i';
						buffer[3] = 'p';
						CallLocalFunction(buffer, "i", playerid);
						// Call the test.
						buffer[0] = 'T';
						buffer[1] = 'e';
						buffer[2] = 'z';
						buffer[3] = 'p';
						P:5("Testing_Next(): Calling %s", buffer[6]);
						CallLocalFunction(buffer, "i", playerid);
					}
					else
					{
						YSI_g_sAsked = true;
						// No more tests.
						printf("*** Tests: %d, Fails: %d", YSI_g_sTests, YSI_g_sFails);
						if (!YSI_g_sFails)
						{
							printf(" ");
							printf("  ||==========================||  ");
							printf("  || ALL PLAYER TESTS PASSED! ||  ");
							printf("  ||==========================||  ");
							printf(" ");
						}
					}
					// If the test needs a player but doesn't ask them anything
					// then we can't wait for "OnDialogResponse" to run the next
					// one.
					if (YSI_g_sAsked)
					{
						break;
					}
					else if (fails == YSI_g_sFails)
					{
						printf(TEST_PASSED);
					}
				}
				//while (!YSI_g_sAsked);
			}
			
			public OnDialogResponse(playerid, dialogid, response, listitem, inputtext[])
			{
				if (dialogid == Y_TESTING_DIALOG_ID)
				{
					++YSI_g_sTests;
					if (response)
					{
						// Pass.
						printf(TEST_PASSED);
					}
					else
					{
						// Fail.
						printf(TEST_FAILED " %s", YSI_g_sFailMessage);
						++YSI_g_sFails;
					}
					Testing_Next(playerid);
					return 1;
				}
				#if defined Testing_OnDialogResponse
					return Testing_OnDialogResponse(playerid, dialogid, response, listitem, inputtext);
				#else
					return 1;
				#endif
			}
			
			#if defined _ALS_OnDialogResponse
				#undef OnDialogResponse
			#else
				#define _ALS_OnDialogResponse
			#endif
			#define OnDialogResponse Testing_OnDialogResponse
			#if defined Testing_OnDialogResponse
				forward Testing_OnDialogResponse(playerid, dialogid, response, listitem, inputtext[]);
			#endif
			
			public OnPlayerSpawn(playerid)
			{
				if (YSI_g_sPlayer == cellmax && !IsPlayerNPC(playerid))
				{
					printf(" ");
					printf("  ||==========================||  ");
					printf("  || STARTING PLAYER TESTS... ||  ");
					printf("  ||==========================||  ");
					printf(" ");
					YSI_g_sTests = 0;
					YSI_g_sFails = 0;
					Testing_Next(playerid);
				}
				#if defined Testing_OnPlayerSpawn
					return Testing_OnPlayerSpawn(playerid);
				#else
					return 1;
				#endif
			}
			
			#if defined _ALS_OnPlayerSpawn
				#undef OnPlayerSpawn
			#else
				#define _ALS_OnPlayerSpawn
			#endif
			#define OnPlayerSpawn Testing_OnPlayerSpawn
			#if defined Testing_OnPlayerSpawn
				forward Testing_OnPlayerSpawn(playerid);
			#endif
		#endif
	#else
		Testing_RunAll()
		{
			// Disable error messages (as we're likely to generate them).
			state ysi_debug : off;
			new
				tests,
				fails;
			printf(" ");
			printf("  ||===================||  ");
			printf("  || STARTING TESTS... ||  ");
			printf("  ||===================||  ");
			printf(" ");
			Testing_Run(tests, fails, _, true);
			printf("*** Tests: %d, Fails: %d", tests, fails);
			if (!fails)
			{
				printf(" ");
				printf("  ||===================||  ");
				printf("  || ALL TESTS PASSED! ||  ");
				printf("  ||===================||  ");
				printf(" ");
			}
			state ysi_debug : on;
		}
	#endif
#else
	//#define Tests:%1() stock bool:Tests_@%1()
	/*#define Test:%1() stock bool:Test_@%1()
	#define TestInit:%1() stock Init_@%1()
	#define TestClose:%1() stock Shut_@%1()
	
	#define PTest:%1(%2) stock bool:Test_@%1(%2)
	#define PTestInit:%1(%2) stock Init_@%1(%2)
	#define PTestClose:%1(%2) stock Shut_@%1(%2)*/
#endif
