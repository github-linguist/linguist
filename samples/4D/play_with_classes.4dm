//%attributes = {}
  // ----------------------------------------------------
  // Project method : object
  // ID[5543995AC5E24988A39E024B39541A8F]
  // Created 13-9-2019 by Vincent de Lachaux
  // ----------------------------------------------------
  // Description:
  //
  // ----------------------------------------------------
  // Declarations
C_OBJECT:C1216($0)
C_TEXT:C284($1)
C_TEXT:C284($2)

C_TEXT:C284($t)
C_OBJECT:C1216($o)
C_COLLECTION:C1488($c)

If (False:C215)
	C_OBJECT:C1216(class ;$0)
	C_TEXT:C284(class ;$1)
	C_TEXT:C284(class ;$2)
End if 

  // ----------------------------------------------------
If (This:C1470[""]=Null:C1517)  // Constructor
	
	If (Count parameters:C259>=1)
		
		$t:=String:C10($1)
		
		If (Count parameters:C259>=2)
			
			  // With parameter
			$o:=Formula from string:C1601($t+"(\""+String:C10($2)+"\")").call()
			
		Else 
			
			$o:=Formula from string:C1601($t).call()
			
		End if 
		
		$o.constructor:=Formula:C1597(class ("constructor"))
		
	End if 
	
Else 
	
	Case of 
			
			  //______________________________________________________
		: ($1="constructor")  // Returns collection of properties and functions
			
			$o:=New object:C1471(\
				"properties";New collection:C1472;\
				"functions";New collection:C1472;\
				"fomulas";New collection:C1472)
			
			$c:=New collection:C1472
			
			For each ($t;This:C1470)
				
				$c.push($t)
				
			End for each 
			
			$c:=$c.orderBy(ck ascending:K85:9)
			
			For each ($t;$c)
				
				Case of 
						
						  //______________________________________________________
					: ($t=$1)
						
						  //______________________________________________________
					: (Value type:C1509(This:C1470[$t])=Is object:K8:27)
						
						  //If (JSON Stringify(This[$t])="\"[object Formula]\"")
						If (This:C1470[$t].source#Null:C1517)
							
							$o.functions.push($t)
							$o.fomulas.push(This:C1470[$t].source)
							
						Else 
							
							$o.properties.push($t)
							
						End if 
						
						  //______________________________________________________
					Else 
						
						$o.properties.push($t)
						
						  //______________________________________________________
				End case 
			End for each 
			
			$o.functions:=$o.functions.orderBy(ck ascending:K85:9)
			$o.properties:=$o.properties.orderBy(ck ascending:K85:9)
			
			  //________________________________________
		: (This:C1470=Null:C1517)
			
			ASSERT:C1129(False:C215;"OOPS, this method must be called from a member method")
			
			  //______________________________________________________
		Else 
			
			$o:=This:C1470[$1].call()
			
			  //______________________________________________________
	End case 
End if 

  // ----------------------------------------------------
  // Return
$0:=$o

  // ----------------------------------------------------
  // End