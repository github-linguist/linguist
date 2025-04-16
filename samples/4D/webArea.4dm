//%attributes = {"invisible":true}
  // ----------------------------------------------------
  // Project method : webArea
  // ID[400C94A87A6E4C049664710A5F9E71F9]
  // Created 10-1-2020 by Vincent de Lachaux
  // ----------------------------------------------------
  // Description:
  //
  // ----------------------------------------------------
  // Declarations
C_OBJECT:C1216($0)
C_TEXT:C284($1)
C_OBJECT:C1216($2)

C_TEXT:C284($t)
C_OBJECT:C1216($o)

If (False:C215)
	C_OBJECT:C1216(webArea ;$0)
	C_TEXT:C284(webArea ;$1)
	C_OBJECT:C1216(webArea ;$2)
End if 

  // ----------------------------------------------------
If (This:C1470[""]=Null:C1517)  // Constructor
	
	If (Count parameters:C259>=1)
		
		$t:=String:C10($1)
		
	End if 
	
	$o:=New object:C1471(\
		"";"webArea";\
		"name";$t;\
		"url";"";\
		"coordinates";Null:C1517;\
		"windowCoordinates";Null:C1517;\
		"getCoordinates";Formula:C1597(widget ("getCoordinates"));\
		"hide";Formula:C1597(OBJECT SET VISIBLE:C603(*;This:C1470.name;False:C215));\
		"moveHorizontally";Formula:C1597(widget ("setCoordinates";New object:C1471("left";$1)));\
		"moveVertically";Formula:C1597(widget ("setCoordinates";New object:C1471("top";$1)));\
		"resizeHorizontally";Formula:C1597(widget ("setCoordinates";New object:C1471("right";$1)));\
		"resizeVertically";Formula:C1597(widget ("setCoordinates";New object:C1471("bottom";$1)));\
		"setVisible";Formula:C1597(OBJECT SET VISIBLE:C603(*;This:C1470.name;Bool:C1537($1)));\
		"show";Formula:C1597(OBJECT SET VISIBLE:C603(*;This:C1470.name;True:C214));\
		"visible";Formula:C1597(OBJECT Get visible:C1075(*;This:C1470.name));\
		"back";Formula:C1597(WA OPEN BACK URL:C1021(*;This:C1470.name));\
		"content";Formula:C1597(WA Get page content:C1038(*;This:C1470.name));\
		"forward";Formula:C1597(WA OPEN FORWARD URL:C1022(*;This:C1470.name));\
		"init";Formula:C1597(webArea ("init"));\
		"isLoaded";Formula:C1597(WA Get current URL:C1025(*;This:C1470.name)=This:C1470.url);\
		"lastFiltered";Formula:C1597(WA Get last filtered URL:C1035(*;This:C1470.name));\
		"openURL";Formula:C1597(webArea ("openURL";New object:C1471("url";$1)));\
		"refresh";Formula:C1597(webArea ("openURL";New object:C1471("url";$1)));\
		"setContent";Formula:C1597(webArea ("setContent";New object:C1471("content";String:C10($1);"base";$2)));\
		"title";Formula:C1597(WA Get page title:C1036(*;This:C1470.name))\
		)
	
	If (Is Windows:C1573)
		
		$o.openURL()  // previously load about:blank
		
	End if 
	
Else 
	
	$o:=This:C1470
	
	Case of 
			
			  //______________________________________________________
		: ($o=Null:C1517)
			
			ASSERT:C1129(False:C215;"OOPS, this method must be called from a member method")
			
			  //______________________________________________________
		: ($1="init")
			
			ARRAY TEXT:C222($tTxt_filters;0x0000)
			ARRAY BOOLEAN:C223($tBoo_allow;0x0000)
			
			  // All are forbidden
			APPEND TO ARRAY:C911($tTxt_filters;"*")  // All
			APPEND TO ARRAY:C911($tBoo_allow;False:C215)  // Forbidden
			
			  // Allow WA SET PAGE CONTENT
			APPEND TO ARRAY:C911($tTxt_filters;"file*")
			APPEND TO ARRAY:C911($tBoo_allow;True:C214)  // to allow including HTML files
			
			WA SET URL FILTERS:C1030(*;$o.name;$tTxt_filters;$tBoo_allow)
			
			WA SET PREFERENCE:C1041(*;$o.name;WA enable Java applets:K62:3;False:C215)
			WA SET PREFERENCE:C1041(*;$o.name;WA enable JavaScript:K62:4;True:C214)
			WA SET PREFERENCE:C1041(*;$o.name;WA enable plugins:K62:5;False:C215)  //
			
			  // Active the contextual menu in debug mode
			WA SET PREFERENCE:C1041(*;$o.name;WA enable contextual menu:K62:6;Not:C34(Is compiled mode:C492) | (Structure file:C489=Structure file:C489(*)))
			WA SET PREFERENCE:C1041(*;$o.name;WA enable Web inspector:K62:7;True:C214)
			
			  //______________________________________________________
		: ($1="openURL")
			
			If (Value type:C1509($2.url)=Is object:K8:27)  // File
				
				$t:="file:///"+$2.url.path
				
			Else 
				
				$t:=String:C10($2.url)
				
			End if 
			
			Case of 
					
					  //……………………………………………………………………………………………………
				: (Length:C16($t)=0)
					
					$t:="about:blank"
					
					  //……………………………………………………………………………………………………
				: ($t="internal")  // Current database server
					
					$t:="127.0.0.1:"+String:C10(WEB Get server info:C1531.options.webPortID)
					
					  //……………………………………………………………………………………………………
				: ($t="localhost")\
					 & Is macOS:C1572  //#TURN_AROUND - In some cases, using "localhost" we get the error -30 "Server unreachable"
					
					$t:="127.0.0.1"
					
					  //……………………………………………………………………………………………………
			End case 
			
			$o.url:=$t
			
			WA OPEN URL:C1020(*;$o.name;$o.url)
			
			  //______________________________________________________
		: ($1="setContent")
			
			WA SET PAGE CONTENT:C1037(*;$o.name;$2.content;Choose:C955($2.base=Null:C1517;"/";String:C10($2.base)))
			
			  //______________________________________________________
		Else 
			
			ASSERT:C1129(False:C215;"Unknown entry point: \""+$1+"\"")
			
			  //______________________________________________________
	End case 
End if 

  // ----------------------------------------------------
  // Return
$0:=$o

  // ----------------------------------------------------
  // End