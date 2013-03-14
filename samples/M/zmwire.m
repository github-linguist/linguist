zmwire ; M/Wire Protocol for M Systems (eg GT.M, Cache)
 ;
 ; ----------------------------------------------------------------------------
 ; | M/Wire                                                                   |
 ; | Copyright (c) 2004-11 M/Gateway Developments Ltd,                        |
 ; | Reigate, Surrey UK.                                                      |
 ; | All rights reserved.                                                     |
 ; |                                                                          |
 ; | http://www.mgateway.com                                                  |
 ; | Email: rtweed@mgateway.com                                               |
 ; |                                                                          |
 ; | This program is free software: you can redistribute it and/or modify     |
 ; | it under the terms of the GNU Affero General Public License as           |
 ; | published by the Free Software Foundation, either version 3 of the       |
 ; | License, or (at your option) any later version.                          |
 ; |                                                                          |
 ; | This program is distributed in the hope that it will be useful,          |
 ; | but WITHOUT ANY WARRANTY; without even the implied warranty of           |
 ; | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            |
 ; | GNU Affero General Public License for more details.                      |
 ; |                                                                          |
 ; | You should have received a copy of the GNU Affero General Public License |
 ; | along with this program.  If not, see <http://www.gnu.org/licenses/>.    |
 ; ----------------------------------------------------------------------------
 ;
 QUIT
 ;
 ; By default this server code runs on port 6330
 ;
 ; For GT.M systems it is invoked via xinetd:
 ;   Edit /etc/services and add the line:
 ; 
 ;    mwire  6330/tcp  # Service for M/Wire Protocol
 ;
 ;   Copy the file mwire to /etc/xinetd.d/mwire
 ;   Copy the file zmwire to /usr/local/gtm/zmwire and change 
 ;    its permissions to executable (eg 755)
 ;
 ;   These files may be edited to change the paths or port number
 ;   Restart xinetd using: sudo /etc/init.d/xinetd restart
 ;
 ;   On GT.M systems you must also have installed MGWSI or m_apache
 ;     in order to provide the MD5 hashing function for passwords
 ;     Alternatively substitute the MD5 callout to the MD5 function of your choice
 ;
 ; For Cache systems, it is invoked via the M/Wire Daemon routine
 ;   which should be running as a jobbed process:
 ;
 ;     job start^zmwireDaemon
 ;
 ;   You can change the port number by simply editing the line
 ; 
 ;      port+1^zmwireDaemon
 ;
 ;    Stop the Daemon process using ^RESJOB and restart it.
 ;
mwireVersion
 ;;Build 22
 ;
mwireDate
 ;;06 July 2011
 ;
version
 ;
 s output="+M/Wire "_$p($t(mwireVersion+1),";;",2,2000)_crlf
 w output
 QUIT
 ;
build
 ;
 n crlf,response
 ;
 s crlf=$c(13,10)
 s response="*3"_crlf
 w response
 i $g(^zewd("trace"))=1 d trace("build: "_response_" sent")
 ;
 s response=$p($t(mwireVersion+1),";;",2,2000)
 s response="$"_$l(response)_crlf_response_crlf
 w response
 i $g(^zewd("trace"))=1 d trace("build: "_response_" sent")
 ;
 s response=$p($t(mwireDate+1),";;",2,2000)
 s response="$"_$l(response)_crlf_response_crlf
 w response
 i $g(^zewd("trace"))=1 d trace("build: "_response_" sent")
 ;
 s response=$zv
 s response="$"_$l(response)_crlf_response_crlf
 w response
 i $g(^zewd("trace"))=1 d trace("build: "_response_" sent")
 QUIT
 ;
command ;
 n authNeeded,c,crlf,input,output
 ;
 d cleardown
 i $zv["GT.M" s $zint="d monitoroutput"
 s ^zmwire("connected",$j)=""
 l +^zmwire("connected",$j)
 s crlf=$c(13,10)
 s authNeeded=0
 s role="user"
 i $d(^zmwire("auth")) s authNeeded=1
loop
 i $g(^zmwire("relink"))=1,'$d(^zmwire("relink","process",$j)) s ok=$$relinkRoutines()
 r *c
 i $c(c)="*" d
 . s input=$$multiBulkRequest()
 . i $g(^zewd("trace"))=1 d trace($j_": "_$h_": mwire input: "_input)
 e  d
 . r input s input=$c(c)_input 
 . i $g(^zewd("trace"))=1 d trace($h_": mwire input: "_input)
 . i input'="" d
 . . i $zv["GT.M" s input=$e(input,1,$l(input)-2)
 . . i $zv["Cache" s input=$e(input,1,$l(input)-1)
 ;i input="PING" w "+PONG"_crlf g loop
 i input="PING" s output="+PONG"_crlf w output g loop
 i $d(^zmwire("monitor","listener")) d log(input)
 i input="" g loop
 i input="EXIT" g halt
 i input="QUIT" g quit
 i input="HALT" g halt
 ;
 i authNeeded,$e(input,1,4)'="AUTH" s output="-Authentication required"_crlf w output g loop
 i 'authNeeded,$e(input,1,4)="AUTH" s output="-Authentication ignored"_crlf w output g loop
 i $e(input,1,5)="AUTH " d auth($e(input,6,$l(input))) g loop
 i 'authNeeded!(role="admin"),$e(input,1,12)="SETPASSWORD " d setpassword($e(input,13,$l(input))) g loop
 ;
 s input=$$utfConvert(input)
 i $e(input,1,4)="SET " d set($e(input,5,$l(input))) g loop
 i $e(input,1,10)="GETGLOBAL " d getGlobal($e(input,11,$l(input))) g loop
 i $e(input,1,14)="GETJSONSTRING " d getJSON($e(input,15,$l(input))) g loop
 i $e(input,1,14)="SETJSONSTRING " d setJSON($e(input,15,$l(input))) g loop
 i $e(input,1,15)="RUNTRANSACTION " d runTransaction($e(input,16,$l(input))) g loop
 i $e(input,1,4)="GET " d get($e(input,5,$l(input))) g loop
 i $e(input,1,7)="INCRBY " d incrby($e(input,8,$l(input))) g loop
 i $e(input,1,7)="DECRBY " d decrby($e(input,8,$l(input))) g loop
 i $e(input,1,14)="NEXTSUBSCRIPT " d nextSubscript($e(input,15,$l(input)),1) g loop
 i $e(input,1,18)="PREVIOUSSUBSCRIPT " d nextSubscript($e(input,19,$l(input)),-1) g loop
 i $e(input,1,5)="KILL " d kill($e(input,6,$l(input))) g loop
 i $e(input,1,4)="DEL " d kill($e(input,5,$l(input))) g loop
 i $e(input,1,5)="DATA " d data($e(input,6,$l(input))) g loop
 i $e(input,1,7)="EXISTS " d data($e(input,8,$l(input))) g loop
 i $e(input,1,5)="INCR " d incr($e(input,6,$l(input))) g loop
 i $e(input,1,5)="DECR " d decr($e(input,6,$l(input))) g loop
 i $e(input,1,11)="COPYGLOBAL " d copy($e(input,12,$l(input))) g loop
 i $e(input,1,5)="LOCK " d lock($e(input,6,$l(input))) g loop
 i $e(input,1,7)="UNLOCK " d unlock($e(input,8,$l(input))) g loop
 i $e(input,1,6)="ORDER " d order($e(input,7,$l(input))) g loop
 i $e(input,1,5)="NEXT " d order($e(input,6,$l(input))) g loop
 i $e(input,1,9)="ORDERALL " d orderall($e(input,10,$l(input))) g loop
 i $e(input,1,11)="GETGLOBALS2" d getGlobals() g loop
 i $e(input,1,10)="GETGLOBALS" d getGlobalList() g loop
 i $e(input,1,9)="MULTIGET " d multiGet($e(input,10,$l(input))) g loop
 i $e(input,1,11)="GETALLSUBS " d orderall($e(input,12,$l(input))) g loop
 i $e(input,1,14)="GETSUBSCRIPTS " d getAllSubscripts($e(input,15,$l(input))) g loop
 i $e(input,1,9)="REVORDER " d reverseorder($e(input,10,$l(input))) g loop
 i $e(input,1,9)="PREVIOUS " d reverseorder($e(input,10,$l(input))) g loop
 i $e(input,1,6)="QUERY " d query($e(input,7,$l(input))) g loop
 i $e(input,1,9)="QUERYGET " d queryget($e(input,10,$l(input))) g loop
 i $e(input,1,10)="MERGEFROM " d mergefrom($e(input,11,$l(input))) g loop
 i $e(input,1,11)="GETSUBTREE " d mergefrom($e(input,12,$l(input))) g loop
 i $e(input,1,8)="MERGETO " d mergeto($e(input,9,$l(input))) g loop
 i $e(input,1,11)="SETSUBTREE " d mergeto($e(input,12,$l(input))) g loop
 i $e(input,1,9)="FUNCTION " d function($e(input,10,$l(input))) g loop
 i $e(input,1,8)="EXECUTE " d function($e(input,9,$l(input))) g loop
 i $e(input,1,6)="TSTART" d tstart g loop
 i $e(input,1,7)="TCOMMIT" d tcommit g loop
 i $e(input,1,9)="TROLLBACK" d trollback g loop
 i $e(input,1,5)="MDATE" d mdate g loop
 i $e(input,1,9)="PROCESSID" d processid g loop
 i $e(input,1,7)="VERSION" d version g loop
 i $e(input,1,8)="GETBUILD" d build g loop
 i $e(input,1,8)="MVERSION" d zv g loop
 i $e(input,1,4)="INFO" d info g loop
 i $e(input,1,7)="MONITOR" d monitor g loop
 s output="-"_input_" not recognized"_crlf w output
 g loop
 ;
multiBulkRequest()
 ;
 n buff,c,i,input,j,len,noOfCommands,param,space
 ;
 s noOfCommands=""
 f  d  q:c=13
 . r *c q:c=13
 . ;d trace("0: "_$c(c))
 . s noOfCommands=noOfCommands_$c(c)
 r *x
 ;d trace("1: "_$c(x))
 ;
 f i=1:1:noOfCommands d
 . ;d trace("i="_i)
 . s len=""
 . f  d  q:c=13
 . . r *c
 . . ;d trace("2: "_$c(c))
 . . i $c(c)="$",len="" q
 . . q:c=13
 . . s len=len_$c(c)
 . r *c
 . ;d trace("3: "_$c(c))
 . s input=""
 . i len=0 d
 . . s param(i)=""
 . . r *c
 . . ;d trace("4: "_$c(c))
 . e  d
 . . r input#len
 . . ;d trace("input="_input)
 . . s param(i)=input
 . . ;d trace("param "_i_" = "_input)
 . . r *c,*c
 ;
 s param(1)=$zconvert(param(1),"U")
 ;QUIT "PING"
 i param(1)="PING" QUIT param(1)
 ;s no=$increment(^rob) m ^rob(no)=param
 i param(1)="SET" QUIT param(1)_" "_param(2)_" "_$l(param(3))_crlf_param(3)
 i param(1)="SETJSONSTRING" QUIT param(1)_" "_param(2)_crlf_param(3)_crlf_param(4)
 i param(1)="COPYGLOBAL"!(param(1)="GETSUBSCRIPTS") d  QUIT input
 . s space="",input=""
 . f i=1:1:noOfCommands d
 . . s input=input_space_param(i)
 . . i space="" s space=" " q
 . . i space=" " s space=$c(1)
 i param(1)="EXECUTE" d  QUIT input
 . ;d trace("Execute: param(3)="_param(3))
 . s param(3)=$$replaceAll(param(3),"\""","""""")
 . i $e(param(3),1)="[" s input=param(1)_" "_param(2)_"("_$e(param(3),2,$l(param(3))-1)_")" q
 . s input=param(1)_" "_param(2)
 ;
 s space="",input=""
 f i=1:1:noOfCommands d
 . s input=input_space_param(i)
 . s space=" " 
 ;
 QUIT input
 ;
halt
 k ^zmwire("connected",$j)
 HALT
 ;
quit
 ;
 i '$d(^zmwire("monitor","listener",$j)) g halt
 k ^zmwire("monitor","listener",$j)
 g loop
 ;
cleardown
 ;
 n ignore,pid
 ;
 s pid=""
 f  s pid=$o(^zmwire("connected",pid)) q:pid=""  d
 . i pid=$j q
 . s ignore=1
 . l +^zmwire("connected",pid):0 e  s ignore=0
 . i ignore d
 . . l -^zmwire("connected",pid)
 . . k ^zmwire("connected",pid)
 . . k ^zmwire("monitor","listener",pid)
 . . k ^zmwire("monitor","output",pid)
 s pid=""
 f  s pid=$o(^zmwire("monitor","output",pid)) q:pid=""  d
 . i pid=$j q
 . s ignore=1
 . l +^zmwire("connected",pid):0 e  s ignore=0
 . l -^zmwire("connected",pid)
 . i ignore d
 . . k ^zmwire("monitor","output",pid)
 QUIT
 ;
monitor
 ;
 i $zv'["GT.M" w "-Command unavailable"_crlf QUIT
 n quit
 ;
 s ^zmwire("monitor","listener",$j)=""
 s output="+OK"_crlf w output
 f  h 1 r quit:0  i $e(quit,1,4)="QUIT" q
 k ^zmwire("monitor","listener",$j)
 s output="+OK"_crlf w output
 QUIT
 ;
log(input)
 ;
 i $zv'["GT.M" QUIT
 ;
 QUIT:'$d(^zmwire("monitor","listener"))
 ;
 n dev,inputr,io,lineNo,pid
 ;
 i input["AUTH" QUIT
 i input["QUIT" QUIT
 i input["EXIT" QUIT
 i input["HALT" QUIT
 s inputr=$re(input)
 i $e(inputr,1,2)'=$c(10,13) s input=input_crlf
 s pid=""
 f  s pid=$o(^zmwire("monitor","listener",pid)) q:pid=""  d
 . i pid=$j q
 . s lineNo=$o(^zmwire("monitor","output",pid,""),-1)+1
 . s ^zmwire("monitor","output",pid,lineNo)=input
 . ;zsy "mupip intrpt "_pid_" >/dev/null"
 . zsy "kill -USR1 "_pid
 ;
 QUIT
 ;
monitoroutput
 ;
 n lineNo
 ;
 s lineNo=""
 f  s lineNo=$o(^zmwire("monitor","output",$j,lineNo)) q:lineNo=""  d
 . w ^zmwire("monitor","output",$j,lineNo)
 . k ^zmwire("monitor","output",$j,lineNo)
 QUIT
 ;
logger(command,initialise)
 ;
 n tot,count
 ;
 i $g(initialise) k ^mwireLogger
 s tot=$increment(^mwireLogger)
 s count=$increment(^mwireLogger(command))
 ;
 QUIT
 ;
info
 ;
 n count,ignore,pid,response
 ;
 s response="m_wire_version:"_$p($t(mwireVersion+1),";;",2,2000)_crlf
 s pid="",count=0
 f  s pid=$o(^zmwire("connected",pid)) q:pid=""  d
 . s ignore=1
 . i pid=$j d
 . . s ignore=0
 . e  d
 . . l +^zmwire("connected",pid):0 e  s ignore=0
 . i ignore d
 . . l -^zmwire("connected",pid)
 . . k ^zmwire("connected",pid)
 . . k ^zmwire("monitor","listener",pid)
 . e  d
 . . s count=count+1
 s response=response_"connected_clients:"_count ;_crlf
 s output="$"_$l(response)_crlf_response_crlf
 w output
 QUIT
 ;
auth(input)
 ;
 n pass
 s pass=$$MD5(input)
 i $d(^zmwire("auth",pass)) d
 . s authNeeded=0
 . s role=^zmwire("auth",pass)
 . s output="+OK"_crlf
 . w output
 e  d
 . s output="-Invalid password"_crlf
 . w output
 QUIT
 ;
setpassword(input)
 ;
 ; SETPASSWORD secret
 ; +OK <set as role=user>
 ;
 ; SETPASSWORD secret admin
 ; +OK
 ;
 n pass,newrole
 ;
 i $d(^zmwire("auth")),role'="admin" s output="-Invalid command"_crlf w output QUIT
 i $$stripSpaces(input)="" s output="-Invalid command"_crlf w output QUIT
 s newrole="user"
 i input[" " d
 . s newrole=$p(input," ",2)
 . s input=$p(input," ",1)
 i '$d(^zmwire("auth")) s newrole="admin"
 i newrole'="user",newrole'="admin" s output="-Invalid role"_crlf w output QUIT
 ;
 s pass=$$MD5(input)
 s ^zmwire("auth",pass)=newrole
 s output="+OK"_crlf
 w output
 QUIT
 ;
getGloRef(input)
 ;
 n gloName,gloRef,nb,subs
 ;
 s gloRef=input
 i $e(gloRef,1)'="^" s gloRef="^"_gloRef
 i $e(gloRef,$l(gloRef))="]" s gloRef=$e(gloRef,1,$l(gloRef)-1)
 s gloName=$p(gloRef,"[",1)
 s nb=$l(gloRef,"[")+2
 s subs=$p(gloRef,"[",2,nb)
 i subs="" QUIT gloName
 QUIT gloName_"("_subs_")"
 ;
set(input)
 ;
 n c123,data,gloName,gloRef,i,inputr,json,len,nb,nsp,ok,quot,subs,x
 ;
 ; SET myglobal["1","xx yy",3] 5
 ; hello
 ; +OK
 ; SET myGlo 5
 ; hello
 ; +OK
 ;
 ;n n
 ;s n=$increment(^rob("set"))
 ;s ^rob("set",n)=input
 i input[crlf d
 . s data=$p(input,crlf,2,$l(input))
 . s input=$p(input,crlf,1)
 s nsp=$l(input," ")
 s len=$p(input," ",nsp)
 i len'=0,+len=0 s output="-Data length was not specified"_crlf w output QUIT
 s gloRef=$p(input," ",1,nsp-1)
 i $e(gloRef,1)'="^" s gloRef="^"_gloRef
 i $e(gloRef,$l(gloRef))="]" s gloRef=$e(gloRef,1,$l(gloRef)-1)
 s gloName=$p(gloRef,"[",1)
 s nb=$l(gloRef,"[")+2
 s subs=$p(gloRef,"[",2,nb)
 ; Process Javascript escaping
 i subs'="" s subs=subs_")"
 s quot=""""
 i subs'="" f i=1:1:$l(subs)-2 d
 . s c123=$e(subs,i,i+2)
 . i $e(c123,1)'="\" q
 . i $e(c123,2)="\" q
 . i $e(c123,2)=quot d  q
 . . i $e(c123,3)="," q
 . . i $e(c123,3)=")" q
 . . s subs=$e(subs,1,i-1)_quot_quot_$e(subs,i+2,$l(subs))
 i subs["\\" d
 . s subs=$$replaceAll(subs,"\\",$c(5))
 . s subs=$$replaceAll(subs,$c(5),"\")
 s gloRef=gloName
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 i subs'="" s gloRef=gloRef_"("_subs
 i '$d(data)  d
 . s data=$$readChars(len)
 . r ok 
 . i $d(^zmwire("monitor","listener")) d log(data)
 i data["""" s data=$$replaceAll(data,"""","""""")
 i data="zmwire_null_value" s data=""
 s x="s "_gloRef_"="""_data_""""
 i $g(^zewd("trace"))=1 d trace("SET x = "_x)
 s $zt=$$zt()
 x x
 s $zt=""
 s json="{""ok"":true}"
 s output="$"_$l(json)_crlf_json_crlf
 w output
 i $g(^zewd("trace"))=1 d trace("set: ok:true sent")
 i $g(^mwire("logger"))=1 d logger("set")
 QUIT
 ;
getGlobalList()
 ;
 n arrString,comma,count,glo,gloRef,list,response,x
 ;
 i $zv["GT.M" d
 . s x="^%"
 . i $d(@x) s list(x)=""
 . f  s x=$order(@x) q:x=""  s list(x)=""
 . ;
 e  d
 . d getGlobalList^MDBMCache(.list)
 ;
 ;s count=0,glo=""
 ;f  s glo=$o(list(glo)) q:glo=""  s count=count+1
 ;s response="*"_count_crlf
 ;w response
 ;i $g(^zewd("trace"))=1 d trace("getGlobalList: "_response_" sent") 
 ;s glo=""
 ;f  s glo=$o(list(glo)) q:glo=""  d
 ;. s gloRef=$e(glo,2,$l(glo))
 ;. s response="$"_$l(gloRef)_crlf_gloRef_crlf
 ;. w response
 ;. i $g(^zewd("trace"))=1 d trace("getGlobalList: "_response_" sent") 
 s arrString="["
 s glo="",comma=""
 f  s glo=$o(list(glo)) q:glo=""  d
 . s gloRef=$e(glo,2,$l(glo))
 . s arrString=arrString_comma_""""_gloRef_""""
 . s comma=","
 s arrString=arrString_"]"
 s response="$"_$l(arrString)_crlf_arrString_crlf
 w response
 i $g(^zewd("trace"))=1 d trace("getGlobalList: "_response_" sent")  ;
 QUIT
 ;
getGlobals()
 ;
 n arrString,comma,count,glo,gloRef,list,response,x
 ;
 i $zv["GT.M" d
 . s x="^%"
 . i $d(@x) s list(x)=""
 . f  s x=$order(@x) q:x=""  s list(x)=""
 . ;
 e  d
 . d getGlobalList^MDBMCache(.list)
 ;
 s count=0,glo=""
 f  s glo=$o(list(glo)) q:glo=""  s count=count+1
 s response="*"_count_crlf
 w response
 i $g(^zewd("trace"))=1 d trace("getGlobalList: "_response_" sent") 
 s glo=""
 f  s glo=$o(list(glo)) q:glo=""  d
 . s gloRef=$e(glo,2,$l(glo))
 . s response="$"_$l(gloRef)_crlf_gloRef_crlf
 . w response
 . i $g(^zewd("trace"))=1 d trace("getGlobalList: "_response_" sent") 
 QUIT
 ;
get(input)
 ;
 n data,exists,gloRef,response,x
 ;
 ; GET myglobal["1","xx yy",3]
 ; $6
 ; foobar
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s x="s exists=$d("_gloRef_")"
 s $zt=$$zt()
 x x
 s $zt=""
 i exists'=1,exists'=11 s output="$-1"_crlf w output QUIT
 s x="s data="_gloRef
 s $zt=$$zt()
 x x
 s $zt=""
 ;
 s response="$"_$l(data)_crlf_data_crlf
 ;
 w response
 i $g(^mwire("logger"))=1 d logger("get")
 QUIT
 ;
getGlobal(input)
 ;
 n data,exists,gloRef,json,response,x
 ;
 ; GET myglobal["1","xx yy",3]
 ; $6
 ; foobar
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s x="s exists=$d("_gloRef_"),"
 ;x x
 s x=x_"data=$g("_gloRef_")"
 s $zt=$$zt()
 x x
 s $zt=""
 s json="{""value"":"""_data_""",""dataStatus"":"_exists_"}"
 ;
 s response="$"_$l(json)_crlf_json_crlf
 ;
 w response
 i $g(^zewd("trace"))=1 d trace("getGlobal: response="_response)
 i $g(^mwire("logger"))=1 d logger("getGlobal")
 QUIT
 ;
multiGet(input)
 ;
 n comma,dataStatus,error,exists,globalName,i,json,props,ref,response,subs,value
 ;
 ; MULTIGET myglobal[
 ;  {globalName:'xxx', subscripts:["x1","y1"]},
 ;  {globalName:'xxx', subscripts:["x2",""]},
 ;  {globalName:'xxx', subscripts:["x2","y2"]}
 ;]
 ;
 ;
 s error=$$parseJSON(input,.props,1)
 i error'="" s output="-"_error_crlf w output QUIT
 ;
 s stop=0,error="",json="[",comma=""
 f i=1:1 q:'$d(props(i))  d
 . s dataStatus=0,value="",error=""
 . s globalName=$g(props(i,"globalName"))
 . i globalName="" s globalName=$g(props(i,"GlobalName"))
 . i globalName="" d
 . . s error="globalName not defined"
 . e  d
 . . i $e(globalName,1)'="^" s globalName="^"_globalName
 . . s ref="s exists=$d("_globalName
 . . s subs=""
 . . i $d(props(i,"subscripts")) d
 . . . n comma,j,stop,sub
 . . . s subs="(",comma="",stop=0
 . . . f j=1:1 q:'$d(props(i,"subscripts",j))  d  q:stop
 . . . . s sub=props(i,"subscripts",j)
 . . . . i sub="" s error="Subscript "_j_" is null",stop=1 q
 . . . . s subs=subs_comma_""""_props(i,"subscripts",j)_"""",comma=","
 . . . i error="" s subs=subs_")"
 . . i error="" d
 . . . s ref=ref_subs_")"
 . . . s $zt=$$zt()
 . . . x ref
 . . . s $zt=""
 . . . i exists d
 . . . . s ref="s value=$g("_globalName
 . . . . s ref=ref_subs_")"
 . . . . s $zt=$$zt()
 . . . . x ref
 . . . . s $zt=""
 . s json=json_comma_"{""value"":"""_value_""",""dataStatus"":"_exists_",""error"":"""_error_"""}"
 . s comma=","
 s json=json_"]"
 ;
 s response="$"_$l(json)_crlf_json_crlf
 ;
 w response
 i $g(^zewd("trace"))=1 d trace("multiGet: response="_response)
 QUIT
 ;
kill(input)
 ;
 n i,glo,gloRef,len,nsp,p1,p2,x
 ;
 ; KILL myglobal["1","xx yy",3]
 ; +OK
 ;
 s glo=input
 s p1=$p(glo,"[",1)
 s p2=$p(glo,"[",2,2000)
 s p2=$e(p2,1,$l(p2)-1)
 s glo=p1_"("_p2_")"
 i glo["()" d
 . s len=$l(glo)
 . i $e(glo,len-1,len)="()" s glo=$e(glo,1,len-2)
 e  d
 . ; Process Javascript escaping
 . n c123,quot
 . s quot=""""
 . f i=1:1:$l(glo)-2 d
 . . s c123=$e(glo,i,i+2)
 . . i $e(c123,1)'="\" q
 . . i $e(c123,2)="\" q
 . . i $e(c123,2)=quot d  q
 . . . i $e(c123,3)="," q
 . . . i $e(c123,3)=")" q
 . . . s glo=$e(glo,1,i-1)_quot_quot_$e(glo,i+2,$l(glo))
 . i glo["\\" d
 . . s glo=$$replaceAll(glo,"\\",$c(5))
 . . s glo=$$replaceAll(glo,$c(5),"\")
 ;
 i glo'["zmwire" s glo(glo)=""

 s x="k ^"_glo
 s $zt=$$zt()
 x x
 s $zt=""
 s response="+ok"_crlf
 i $g(^%zewd("trace"))=1 d trace("kill: response="_response)
 w response
 i $g(^mwire("logger"))=1 d logger("kill")
 QUIT
 ;
data(input)
 ;
 n data,gloRef,x
 ;
 ; DATA myglobal["1","xx yy",3]
 ; :10
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s x="s data=$d("_gloRef_")"
 s $zt=$$zt()
 x x
 s $zt=""
 i $g(^zewd("trace"))=1 d trace("input="_input_"; data="_data)
 s output=":"_data_crlf
 w output
 i $g(^mwire("logger"))=1 d logger("data")
 QUIT
 ;
runTransaction(input)
 ;
 n error,globalName,i,json,props,ref,result,stop,subscripts
 ;
 s error=$$parseJSON(input,.props,1)
 i error'="" s output="-"_error_crlf w output QUIT
 ;
 s stop=0,error=""
 f i=1:1 q:'$d(props(i))  d  q:stop
 . s method=$g(props(i,"method"))
 . i method="" s stop=1,error="Missing method in JSON transaction document at step "_i q
 . i method="setJSON" d  q:stop
 . . n json
 . . m json=props(i,"json")
 . . i '$d(json) s stop=1,error="Missing JSON document in JSON transaction document at step "_i q
 . . s globalName=$g(props(i,"globalName"))
 . . i globalName="" s globalName=$g(props(i,"GlobalName"))
 . . i globalName="" s stop=1,error="Missing Global name in JSON transaction document at step "_i q
 . . i $e(globalName,1)'="^" s globalName="^"_globalName
 . . s ref="m "_globalName
 . . i $d(props(i,"subscripts")) d
 . . . n comma,j
 . . . s ref=ref_"(",comma=""
 . . . f j=1:1 q:'$d(props(i,"subscripts",j))  d
 . . . . s ref=ref_comma_""""_props(i,"subscripts",j)_"""",comma=","
 . . . s ref=ref_")"
 . . s ref=ref_"=json"
 . . x ref
 . i method="setGlobal" d  q:stop
 . . n value
 . . s globalName=$g(props(i,"globalName"))
 . . i globalName="" s globalName=$g(props(i,"GlobalName"))
 . . i globalName="" s stop=1,error="Missing Global name in JSON transaction document at step "_i q
 . . i $e(globalName,1)'="^" s globalName="^"_globalName
 . . s ref="s "_globalName
 . . i $d(props(i,"subscripts")) d
 . . . n comma,j
 . . . s ref=ref_"(",comma=""
 . . . f j=1:1 q:'$d(props(i,"subscripts",j))  d
 . . . . s ref=ref_comma_""""_props(i,"subscripts",j)_"""",comma=","
 . . . s value=$g(props(i,"value"))
 . . . i value="zmwire_null_value" s value=""
 . . . s ref=ref_")="""_value_""""
 . . x ref
 . i method="kill" d  q:stop
 . . s globalName=$g(props(i,"globalName"))
 . . i globalName="" s globalName=$g(props(i,"GlobalName"))
 . . i globalName="" s stop=1,error="Missing Global name in JSON transaction document at step "_i q
 . . i $e(globalName,1)'="^" s globalName="^"_globalName
 . . s ref="k "_globalName
 . . i $d(props(i,"subscripts")) d
 . . . n comma,j
 . . . s ref=ref_"(",comma=""
 . . . f j=1:1 q:'$d(props(i,"subscripts",j))  d
 . . . . s ref=ref_comma_""""_props(i,"subscripts",j)_"""",comma=","
 . . . s ref=ref_")"
 . . x ref
 ;
 i error'="" s output="-"_error_crlf w output QUIT
 s response="+ok"_crlf
 w response
 i $g(^zewd("trace"))=1 d trace("transaction: response="_response)
 QUIT
 ;
setJSON(input)
 ;
 n arr,del,error,flrc,inputr,gloRef,inputr,json,nb,nsp,props,ref,response,subs
 ;
 ; SETJSONSTRING myglobal["1","xx yy",3] CRLF {"a":123} CRLF 1
 ; +ok
 ;
 s flrc=$c(10,13)
 s gloRef=$p(input,crlf,1)
 s input=$p(input,crlf,2,10000)
 s inputr=$re(input)
 s del=$p(inputr,flrc,1),del=$re(del)
 s inputr=$p(inputr,flrc,2,10000) ; in case it contains crlfs
 s json=$re(inputr)
 i $zv["GT.M" d
 . s json=$$unEscape(json)
 e  d
 . s json=$$unEscape^MDBMCache(json)
 ;
 i $e(gloRef,1)'="^" s gloRef="^"_gloRef
 i $e(gloRef,$l(gloRef))="]" s gloRef=$e(gloRef,1,$l(gloRef)-1)
 s gloName=$p(gloRef,"[",1)
 s nb=$l(gloRef,"[")+2
 s subs=$p(gloRef,"[",2,nb)
 s gloRef=gloName
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 i subs'="" s gloRef=gloRef_"("_subs_")"
 s error=$$parseJSON(json,.props,1)
 i error'="" s output="-Invalid JSON in setJSON: "_json_crlf w output QUIT
 ;
 s ref=""
 i del s ref="k "_gloRef_" "
 ;
 s ref=ref_"m "_gloRef_"=props"
 x ref
 s response="+OK"_crlf
 w response
 i $g(^zewd("trace"))=1 d trace("setJSON: response="_response)
 i $g(^mwire("logger"))=1 d logger("setJSON")
 ;
 QUIT
 ;
getJSON(input)
 ;
 n arr,inputr,gloRef,json,nb,nsp,ref,response,subs
 ;
 ; GETJSONSTRING myglobal["1","xx yy",3]
 ; $5
 ; {x:1}
 ;
 s gloRef=input
 i $e(gloRef,1)'="^" s gloRef="^"_gloRef
 i $e(gloRef,$l(gloRef))="]" s gloRef=$e(gloRef,1,$l(gloRef)-1)
 s gloName=$p(gloRef,"[",1)
 s nb=$l(gloRef,"[")+2
 s subs=$p(gloRef,"[",2,nb)
 s gloRef=gloName
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 i subs'="" s gloRef=gloRef_"("_subs_")"
 s ref="m arr="_gloRef
 x ref
 i '$d(arr) d
 . s response="$-1"_crlf
 e  d
 . s json=$$arrayToJSON("arr")
 . s response="$"_$l(json)_crlf_json_crlf
 i $g(^zewd("trace"))=1 d trace("getJSON: response="_response)
 ;
 w response
 ;
 i $g(^mwire("logger"))=1 d logger("getJSON")
 QUIT
 ;
incr(input)
 ;
 n data,gloRef,x
 ;
 ; INCR myglobal["1","xx yy",3]
 ; :4
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s x="s data=$increment("_gloRef_")"
 s $zt=$$zt()
 x x
 s $zt=""
 s output=":"_data_crlf
 w output
 i $g(^mwire("logger"))=1 d logger("incr")
 QUIT
 ;
incrby(input)
 ;
 n by,data,gloName,gloRef,inputr,len,nb,nsp,ok,subs,x
 ;
 ; INCRBY myglobal["1","xx yy",3] 3
 ; :7
 ;
 s inputr=$re(input)
 s by=$re($p(inputr," ",1))
 s nsp=$l(input," ")+2
 s gloRef=$p(inputr," ",2,nsp)
 s gloRef=$re(gloRef)
 i $e(gloRef,1)'="^" s gloRef="^"_gloRef
 i $e(gloRef,$l(gloRef))="]" s gloRef=$e(gloRef,1,$l(gloRef)-1)
 s gloName=$p(gloRef,"[",1)
 s nb=$l(gloRef,"[")+2
 s subs=$p(gloRef,"[",2,nb)
 s gloRef=gloName
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 i subs'="" s gloRef=gloRef_"("_subs_")"
 s x="s data=$increment("_gloRef_","_by_")"
 s $zt=$$zt() x x
 s $zt=""
 s output=":"_data_crlf
 w output
 i $g(^mwire("logger"))=1 d logger("incrbr")
 QUIT
 ;
function(input)
 ;
 n data,func,x
 ;
 ; FUNCTION label^rou("1","xx yy")
 ; $5
 ; hello
 ;
 s func=input
 i func["^",$e(func,1,2)'="$$" s func="$$"_func
 i func["class(",$e(func,1,2)'="##" s func="##"_func
 s x="s data="_func
 s $zt=$$zt()
 x x
 s $zt=""
 s output="$"_$l(data)_crlf_data_crlf
 w output
 i $g(^mwire("logger"))=1 d logger("function")
 QUIT
 ;
decr(input)
 ;
 n data,gloRef,x
 ;
 ; DECR myglobal["1","xx yy",3]
 ; :3
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s x="s data=$increment("_gloRef_",-1)"
 s $zt=$$zt() x x
 s $zt=""
 s output=":"_data_crlf
 w output
 i $g(^mwire("logger"))=1 d logger("decr")
 QUIT
 ;
decrby(input)
 ;
 n by,data,gloName,gloRef,inputr,nb,nsp,ok,subs,x
 ;
 ; DECRBY myglobal["1","xx yy",3] 3
 ; :4
 ;
 s inputr=$re(input)
 s by=$re($p(inputr," ",1))
 s nsp=$l(input," ")+2
 s gloRef=$p(inputr," ",2,nsp)
 s gloRef=$re(gloRef)
 i $e(gloRef,1)'="^" s gloRef="^"_gloRef
 i $e(gloRef,$l(gloRef))="]" s gloRef=$e(gloRef,1,$l(gloRef)-1)
 s gloName=$p(gloRef,"[",1)
 s nb=$l(gloRef,"[")+2
 s subs=$p(gloRef,"[",2,nb)
 s gloRef=gloName
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 i subs'="" s gloRef=gloRef_"("_subs_")"
 s x="s data=$increment("_gloRef_",-"_by_")"
 s $zt=$$zt()
 x x
 s $zt=""
 s output=":"_data_crlf
 w output
 i $g(^mwire("logger"))=1 d logger("decrby")
 QUIT
 ;
nextSubscript(input,direction)
 ;
 n data,gloRef,response,subscript,x,value
 ;
 ; NEXTSUBSCRIPT myglobal["1","xx yy",""]
 ; +abc
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s x="s subscript=$o("_gloRef_",direction)"
 s $zt=$$zt()
 x x
 s $zt=""
 s value="",data=0
 i subscript'="" d
 . s value=$g(^(subscript))
 . s data=$d(^(subscript))
 ;
 s response="{""subscriptValue"":"""_subscript_""","
 s response=response_"""dataStatus"":"_data_","
 s response=response_"""dataValue"":"""_value_"""}"
 ;
 s response="$"_$l(response)_crlf_response_crlf
 i $g(^zewd("trace"))=1 d trace("nextsubscript: response="_response)
 w response
 i $g(^mwire("logger"))=1 d logger("nextsubscript")
 ;
 QUIT
 ;
order(input)
 ;
 n data,gloRef,x
 ;
 ; ORDER myglobal["1","xx yy",""]
 ; +abc
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s x="s data=$o("_gloRef_")"
 s $zt=$$zt()
 x x
 s $zt=""
 i data="" s output="$-1"_crlf w output QUIT
 s output="$"_$l(data)_crlf_data_crlf
 w output
 i $g(^mwire("logger"))=1 d logger("order")
 QUIT
 ;
reverseorder(input)
 ;
 n data,gloRef,x
 ;
 ; REVORDER myglobal["1","xx yy",""]
 ; +abc
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s x="s data=$o("_gloRef_",-1)"
 s $zt=$$zt()
 x x
 s $zt=""
 i data="" s output="$-1"_crlf w output QUIT
 s output="$"_$l(data)_crlf_data_crlf
 w output
 i $g(^mwire("logger"))=1 d logger("reverseorder")
 QUIT
 ;
query(input)
 ;
 n data,gloRef,nb,p1,p2,x
 ;
 ; QUERY myglobal["1","xx yy"]
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s x="s data=$q("_gloRef_")"
 s $zt=$$zt()
 ;d trace("query x="_x)
 x x
 i data="" s output="$-1"_crlf w output QUIT
 s data=$e(data,2,$l(data))
 ;d trace("data="_data)
 s p1=$p(data,"(",1)
 s nb=$l(data,"(")+2
 ;d trace("nb="_nb)
 s p2=$p(data,"(",2,nb)
 ;d trace("1 p2="_p2)
 s p2=$e(p2,1,$l(p2)-1)
 ;d trace("2 p2="_p2)
 s data=p1_"["_p2_"]"
 s output="$"_$l(data)_crlf_data_crlf
 w output
 s $zt=""
 i $g(^mwire("logger"))=1 d logger("query")
 QUIT
 ;
queryget(input)
 ;
 n data,gloRef,nb,odata,p1,p2,value,x
 ;
 ; QUERYGET myglobal["1","xx yy"]
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s x="s data=$q("_gloRef_")"
 s $zt=$$zt()
 x x
 i data="" s output="$-1"_crlf w output QUIT
 s odata=data
 s output="*2"_crlf
 w output
 s data=$e(data,2,$l(data))
 s p1=$p(data,"(",1)
 s nb=$l(data,"(")+2
 s p2=$p(data,"(",2,nb)
 s p2=$e(p2,1,$l(p2)-1)
 s data=p1_"["_p2_"]"
 s output="$"_$l(data)_crlf_data_crlf
 w output
 s value=@odata
 s output="$"_$l(value)_crlf_value_crlf
 w output
 s $zt=""
 i $g(^mwire("logger"))=1 d logger("queryget")
 QUIT
 ;
lock(input)
 ;
 n gloName,gloRef,inputr,nb,nsp,ok,subs,time,x
 ;
 ; LOCK myglobal["1","xxyy"] 5
 ; +OK
 ;
 s inputr=$re(input)
 s time=$re($p(inputr," ",1))
 i time?1N.N d
 . s nsp=$l(input," ")+2
 . s gloRef=$p(inputr," ",2,nsp)
 e  d
 . s time=5
 . s gloRef=inputr
 s gloRef=$re(gloRef)
 i $e(gloRef,1)'="^" s gloRef="^"_gloRef
 i $e(gloRef,$l(gloRef))="]" s gloRef=$e(gloRef,1,$l(gloRef)-1)
 s gloName=$p(gloRef,"[",1)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s nb=$l(gloRef,"[")+2
 s subs=$p(gloRef,"[",2,nb)
 s gloRef=gloName
 i subs'="" s gloRef=gloRef_"("_subs_")"
 s x="s ok=1 l +"_gloRef_":"_time_" e  s ok=0"
 s $zt=$$zt()
 x x
 s $zt=""
 s output=":"_ok_crlf
 w output
 QUIT
 ;
unlock(input)
 ;
 n gloRef,x
 ;
 ; UNLOCK myglobal["1","xxyy"]
 ; +OK
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 s x="l -"_gloRef
 s $zt=$$zt()
 x x
 s $zt=""
 s output="+OK"_crlf
 w output
 QUIT
 ;
getAllSubscripts(input)
 ;
 n comma,data,exists,from,i,gloRef,len,numericEnd,rec,ref
 n stop,subscripts,subs,subs1,to,x
 ;
 ; GETSUBSCRIPTS myglobal["1","xx yy"] fromValue toValue
 ; 
 s gloRef=$p(input,$c(1),1)
 s gloRef=$$getGloRef(gloRef)
 s from=$p(input,$c(1),2)
 i from="zz-null" s from=""
 s to=$p(input,$c(1),3)
 i to="zz-null" s to=""
 s numericEnd=$$numeric(to)
 ;d trace("to="_to_": numeric = "_numericEnd)
 ;
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 i $e(gloRef,$l(gloRef))=")" d
 . s x="s exists=$d("_gloRef_")"
 . s gloRef=$e(gloRef,1,$l(gloRef)-1)_","
 e  d
 . s x="s exists=$d("_gloRef_")"
 . s gloRef=gloRef_"("
 s $zt=$$zt()
 x x
 s $zt=""
 i 'exists!(exists=1) s output="$2"_crlf_"[]"_crlf w output QUIT
 ;
 s subs=from
 s subs1=subs i subs1["""" s subs1=$$replaceAll(subs1,"""","""""")
 i from'="" d
 . s x="s subs1=$o("_gloRef_""""_subs1_"""),-1)"
 . ;d trace("1 x="_x)
 . x x
 s x="s subs=$o("_gloRef_""""_subs1_"""))"
 x x
 s len=3+$l(subs)
 s comma=",",stop=0
 i subs'="" d
 . f  s subs=$o(^(subs)) q:stop  d
 . . i subs="" s stop=1 q
 . . i to'="" d  q:stop
 . . . i numericEnd d
 . . . . ;d trace("numeric: subs="_subs_": to="_to)
 . . . . i $$numeric(subs),subs>to s stop=1
 . . . e  d
 . . . . i subs]to s stop=1
 . . s len=len+$l(comma)+2+$l(subs)
 ;d trace("3 len="_len)
 s len=len+1
 s response="$"_len_crlf
 w response
 ;
 s x="s subs=$o("_gloRef_""""_subs1_"""))"
 ;d trace("4 x="_x)
 ;d trace("xx to="_to)
 x x
 s response="["""_subs_""""
 w response
 i subs'="" d
 . s stop=0
 . f  s subs=$o(^(subs)) q:stop  d
 . . i subs="" s stop=1 q
 . . i to'="" d  q:stop
 . . . i numericEnd d
 . . . . ;d trace("numeric: subs="_subs_": to="_to)
 . . . . i $$numeric(subs),subs>to s stop=1
 . . . e  d
 . . . . i subs]to s stop=1
 . . s response=comma_""""_subs_""""
 . . w response
 ;d trace("5 response="_response)
 s response="]"_crlf
 w response
 i $g(^mwire("logger"))=1 d logger("getallsubscripts")
 ;
 QUIT
 ;
orderall(input)
 ;
 n data,exists,i,gloRef,rec,subs,subs1,x
 ;
 ; ORDERALL myglobal["1","xx yy"] 
 ; *6
 ; $2
 ; aa
 ; $5
 ; hello
 ; $2
 ; bb
 ; $5
 ; world
 ; $3
 ; bba
 ; $-1
 ; 
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 i $e(gloRef,$l(gloRef))=")" d
 . s x="s exists=$d("_gloRef_")"
 . s gloRef=$e(gloRef,1,$l(gloRef)-1)_","
 e  d
 . s x="s exists=$d("_gloRef_")"
 . s gloRef=gloRef_"("
 s $zt=$$zt()
 x x
 i 'exists s output="$-1"_crlf w output QUIT
 ;
 s subs="",rec=0
 k ^CacheTempEWD($j)
 f  d  q:subs=""
 . s subs1=subs i subs1["""" s subs1=$$replaceAll(subs1,"""","""""")
 . s x="s subs=$o("_gloRef_""""_subs1_"""))"
 . x x
 . i subs="" q
 . s rec=rec+1
 . s ^CacheTempEWD($j,rec)="$"_$l(subs)_crlf_subs_crlf
 . s x="s exists=$d("_gloRef_""""_subs_"""))"
 . x x
 . i exists=1!(exists=11) d
 . . s x="s data="_gloRef_""""_subs_""")"
 . . x x
 . . s rec=rec+1
 . . s ^CacheTempEWD($j,rec)="$"_$l(data)_crlf_data_crlf
 . e  d
 . . s rec=rec+1
 . . s ^CacheTempEWD($j,rec)="$-1"_crlf
 s $zt=""
 s output="*"_rec_crlf
 w output
 f i=1:1:rec w ^CacheTempEWD($j,i)
 k ^CacheTempEWD($j)
 i $g(^mwire("logger"))=1 d logger("orderall")
 QUIT
 ;
mergefrom(input)
 ;
 n data,gloRef,i,params,resp,start,x
 ;
 ; MERGEFROM myglobal["1","a"]
 ; *6
 ; $1
 ; 1  <keys>
 ; $5
 ; hello  <data>
 ; $9
 ; 1,"a\"aa"  <keys> note escaping
 ; $5
 ; world
 ; $8
 ; 2,"cccc"
 ; $3
 ; foo
 ;
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 k ^CacheTempEWD($j)
 s x="m ^CacheTempEWD($j)="_gloRef
 s $zt=$$zt()
 x x
 s $zt=""
 s x=$q(^CacheTempEWD($j,""))
 i x="" d  QUIT
 . s output="*-1"_crlf 
 . w output 
 . k ^CacheTempEWD($j)
 f i=1:1 s x=$q(@x) q:x=""
 i $d(^CacheTempEWD($j))=1!($d(^CacheTempEWD($j))=11) s i=i+1
 s output="*"_(i*2)_crlf
 w output
 i $d(^CacheTempEWD($j))=1!($d(^CacheTempEWD($j))=11) d
 . s output="$-1"_crlf
 . w output
 . s output="$"_$l(^CacheTempEWD($j))_crlf_^CacheTempEWD($j)_crlf
 . w output
 s x=$q(^CacheTempEWD($j,""))
 s start="^CacheTempEWD("_$j_","
 s params=$p(x,start,2,2000)
 s params=$e(params,1,$l(params)-1)
 s resp=params
 s output="$"_$l(resp)_crlf_resp_crlf
 w output
 s data=@x
 s output="$"_$l(data)_crlf_data_crlf  
 w output
 f i=1:1 s x=$q(@x) q:x=""  d
 . s params=$p(x,start,2,2000)
 . s params=$e(params,1,$l(params)-1)
 . s resp=params
 . s output="$"_$l(resp)_crlf_resp_crlf  
 . w output
 . s data=@x
 . i data="" d
 . . s output="$-1"_crlf
 . . w output
 . e  d
 . . s output="$"_$l(data)_crlf_data_crlf  
 . . w output
 k ^CacheTempEWD($j)
 QUIT
 ;
mergeto(input)
 ;
 n data,dataLength,error,gloRef,i,key,keyLength,noOfRecs,x
 ;
 ; MERGETO myglobal["1","a"]
 ; *6
 ; $1
 ; 1   <keys>
 ; $5
 ; hello  <data>
 ; $7
 ; 1,"aaa"  <keys>
 ; $5
 ; world   <data>
 ; $8
 ; 2,"cccc"
 ; $3
 ; foo
 ; +OK
 ;   note $-1 for key length means no key - data put at top level
 ;
 s $zt=$$zt()
 s gloRef=$$getGloRef(input)
 i gloRef["^zmwire" s output="-No access allowed to this global"_crlf w output QUIT
 i $e(gloRef,$l(gloRef))=")" s gloRef=$e(gloRef,1,$l(gloRef)-1)
 ;
 r noOfRecs
 i $e(noOfRecs,1)'="*" s output="-Invalid: expected number of records"_crlf w output QUIT
 s noOfRecs=+$e(noOfRecs,2,$l(noOfRecs))
 i noOfRecs'?1N.N s output="-Invalid format for number of records"_crlf w output QUIT
 i noOfRecs=0 QUIT "+OK"_crlf QUIT
 i (noOfRecs#2)=1 s output="-Invalid: no of records must be an even number"_crlf w output QUIT
 s noOfRecs=noOfRecs/2
 k ^CacheTempEWD($j)
 s error=""
 k ^CacheTempEWD($j)
 f i=1:1:noOfRecs d  q:error'=""
 . r keyLength
 . i $e(keyLength,1)'="$" s error="Invalid record "_i_": record length" q
 . s keyLength=+$e(keyLength,2,$l(keyLength))
 . i keyLength=-1 d
 . . s key=""
 . e  d
 . . i keyLength'?1N.N s error="Invalid record "_i_": bad format for record length" q
 . . i keyLength=0 s error="Invalid record "_i_": record length cannot be zero" q
 . . s key=$$readChars(keyLength)
 . . r ok d log(key)
 . . ;r key#keyLength,ok d log(key)
 . . i key["\""" s key=$$replaceAll(key,"\""","""""")
 . i error'="" q
 . r dataLength
 . i $e(dataLength,1)'="$" s error="Invalid record "_i_": expected data length" q
 . s dataLength=+$e(dataLength,2,$l(dataLength))
 . i dataLength'=-1,dataLength'?1N.N s error="Invalid record "_i_": bad format for data length" q
 . i dataLength=-1 d
 . . s data=""
 . e  d
 . . s data=$$readChars(dataLength)
 . . r ok
 . . d log(data)
 . . ;r data#dataLength,ok d log(data)
 . . i data["""" s data=$$replaceAll(data,"""","""""")
 . i key="" d
 . . n gloRef1
 . . s gloRef1=gloRef
 . . i gloRef["(" s gloRef1=gloRef1_")"
 . . s x="s "_gloRef1_"="""_data_""""
 . e  d
 . . n gloRef1
 . . s gloRef1=gloRef
 . . i gloRef'["(" d
 . . . s gloRef1=gloRef1_"("
 . . e  d
 . . . s gloRef1=gloRef1_","
 . . s x="s "_gloRef1_key_")="""_data_""""
 . s ^CacheTempEWD($j,i)=x
 i error'="" s output="-"_error_crlf w output QUIT
 f i=1:1:noOfRecs d
 . s x=^CacheTempEWD($j,i)
 . x x
 k ^CacheTempEWD($j)
 s output="+OK"_crlf
 w output
 s $zt=""
 QUIT
 ;
copy(input)
 ;
 n fromGlo,killToFirst,p2,response,toGlo,x
 ;
 ; COPY fromGlobal["1","a"] toGlobal["x"] 1
 i $g(^%zewd("trace"))=1 d trace("copy: input="_input)
 s $zt=$$zt()
 s fromGlo=$p(input,$c(1),1)
 s toGlo=$p(input,$c(1),2)
 s killToFirst=$p(input,$c(1),3)
 s p2=$p(fromGlo,"[",2,2000)
 s p2=$e(p2,1,$l(p2)-1)
 i p2'="" s p2="("_p2_")"
 s fromGlo="^"_$p(fromGlo,"[",1)_p2
 s p2=$p(toGlo,"[",2,2000)
 s p2=$e(p2,1,$l(p2)-1)
 i p2'="" s p2="("_p2_")"
 s toGlo="^"_$p(toGlo,"[",1)_p2
 s x=""
 i killToFirst s x="k "_toGlo_" "
 s x=x_"m "_toGlo_"="_fromGlo
 x x
 ;
 i $g(^%zewd("trace"))=1 d trace("x="_x)
 s response="+ok"_crlf
 i $g(^%zewd("trace"))=1 d trace("copy: response="_response)
 w response
 s $zt=""
 i $g(^mwire("logger"))=1 d logger("copy")
 ;
 QUIT
 ;
mdate
 ;
 n date,day,time
 ;
 s date=$h
 s day=+date
 s output="*2"_crlf_"$"_$l(day)_crlf_day_crlf
 w output
 s time=$p(date,",",2)
 s output="$"_$l(time)_crlf_time_crlf
 w output
 QUIT
 ;
tstart
 s $zt=$$zt()
 TSTART
 s output="+OK"_crlf
 w output
 s $zt=""
 QUIT
 ;
tcommit
 s $zt=$$zt()
 TCOMMIT
 s output="+OK"_crlf
 w output
 s $zt=""
 QUIT
 ;
trollback
 s $zt=$$zt()
 TROLLBACK
 s output="+OK"_crlf
 w output
 s $zt=""
 QUIT
 ;
zv
 s output="+"_$zv_crlf
 w output
 QUIT
 ;
zt()
 i $zv["GT.M" QUIT "g executeError^zmwire"
 QUIT "executeError^zmwire"
 ;
processid
 s output=":"_$j_crlf
 w output
 QUIT
 ;
ping
 s output="+PONG"_crlf
 w output
 QUIT
 ;
executeError
 s output="-Invalid Command"_crlf
 w output
 g loop
 ;
replaceAll(InText,FromStr,ToStr) ; Replace all occurrences of a substring
 ;
 n p
 ;
 s p=InText
 i ToStr[FromStr d  QUIT p
 . n i,stop,tempText,tempTo
 . s stop=0
 . f i=0:1:255 d  q:stop
 . . q:InText[$c(i)
 . . q:FromStr[$c(i)
 . . q:ToStr[$c(i)
 . . s stop=1
 . s tempTo=$c(i)
 . s tempText=$$replaceAll(InText,FromStr,tempTo)
 . s p=$$replaceAll(tempText,tempTo,ToStr)
 f  q:p'[FromStr  S p=$$replace(p,FromStr,ToStr)
 QUIT p
 ;
replace(InText,FromStr,ToStr) ; replace old with new in string
 ;
 n np,p1,p2
 ;
 i InText'[FromStr q InText
 s np=$l(InText,FromStr)+1
 s p1=$p(InText,FromStr,1),p2=$p(InText,FromStr,2,np)
 QUIT p1_ToStr_p2
 ;
readChars(length)
 ;
 n data,i,x
 ;
 s data=""
 f i=1:1:length r *x s data=data_$c(x)
 QUIT data
 ;
stripSpaces(string)
 i $zv["Cache" QUIT $$stripSpaces^MDBMCache(string)
 ;
 s string=$$stripLeadingSpaces(string)
 QUIT $$stripTrailingSpaces(string)
 ;
stripLeadingSpaces(string)
 ;
 n i
 ;
 f i=1:1:$l(string) QUIT:$e(string,i)'=" "
 QUIT $e(string,i,$l(string))
 ;
stripTrailingSpaces(string)
 ;
 n i,spaces,new
 ;
 s spaces=$$makeString(" ",100)
 s new=string_spaces
 QUIT $p(new,spaces,1)
 ;
makeString(char,len) ; create a string of len characters
 ;
 n str
 ;
 s str="",$p(str,char,len+1)=""
 QUIT str
 ;
MD5(string)
 ;
 ; n hash
 ;
 i $zv["Cache" QUIT $$MD5^MDBMCache(string)
 ;
 QUIT $$MD5^%ZMGWSIS(string,1,1)
 ;
unEscape(string)
 ;
 n buf,outstring,p1,p2,hex,asc
 ;
 s buf=string
 s outstring=""
 f  q:buf'["%"  d
 . s p1=$p(buf,"%",1)
 . s outstring=outstring_p1
 . s p2=$p(buf,"%",2,50000)
 . i $e(p2)="u" s buf=$e(p2,6,9999),hex=$e(p2,2,5),outstring=outstring_$c($$hex2Ascii(hex)-1264) q
 . s hex=$e(p2,1,2)
 . s buf=$e(p2,3,$l(p2))
 . s asc=$$hex2Ascii(hex)
 . s outstring=outstring_$c(asc)
 QUIT (outstring_buf)
 ;
hex2Ascii(string)
 ;
 n asc,c,conv,err,i,n,power
 ;
 s string=$zconvert(string,"U")
 s asc=0
 f i=0:1:9 S conv(i)=i
 s conv("A")=10
 s conv("B")=11
 s conv("C")=12
 s conv("D")=13
 s conv("E")=14
 s conv("F")=15
 s n=-1,err=0
 f i=$l(string):-1:1 d  q:err
 . s n=n+1
 . s power=16**n
 . s c=$e(string,i)
 . i '$d(conv(c)) s err=1 q
 . s asc=asc+(conv(c)*power)
 i err QUIT "-1"
 QUIT asc
 ;
ts()
 s last=$g(^zmwire("lastts"))
 n io,p,resp
 s io=$io
 s p="time"
 o p:(COMMAND="date +%s%N":READONLY)::"PIPE"
 u p
 r resp q:$ZEOF
 c p
 u io
 s ^zmwire("lastts")=resp
 QUIT ((resp-last)/1000000000)
 ;
relinkRoutines()
 n list,rou,xrou
 i $g(^zewd("trace"))=1 d trace("Process "_$j_": Relinking...")
 s rou=""
 f  s rou=$view("RTNNEXT",rou) q:rou=""  d
 . i rou="zmwire" q
 . i rou="%zewdGTMRuntime" q
 . i rou="%zewdPHP" q
 . i rou="MDB" q
 . i rou="ewdWLewdmgrrelink" q
 . i rou="%ZMGWSI" q
 . i rou="%ZMGWSIS" q
 . i rou="GTM$DMOD" q
 . s xrou=rou
 . i $e(xrou,1)="%" s xrou="_"_$e(xrou,2,$l(xrou))
 . zl xrou
 . i $g(^zewd("trace"))=1 d trace("relinked "_rou)
 s ^zmwire("relink","process",$j)=""
 i $g(^zewd("trace"))=1 d trace("Process "_$j_": Relinking complete")
 QUIT ""
 ;
relink ;
 s ^zmwire("relink")=1
 k ^zmwire("relink","process")
 QUIT
 ;
arrayToJSON(name)
 n a,buff,c,json,subscripts
 i '$d(@name) QUIT "[]"
 s json=$$walkArray("",name)
 ; Encode UTF-8 characters
 s buff=""
 f i=1:1:$l(json) d
 . s c=$e(json,i)
 . s a=$a(c)
 . i a>160 d
 . . i a<192 d
 . . . s buff=buff_$c(194)_c
 . . e  d
 . . . s buff=buff_$c(195)_$c(a-64)
 . e  d
 . . s buff=buff_c
 QUIT buff
 ;
walkArray(json,name,subscripts)
 ;
 n allNumeric,arrComma,brace,comma,count,cr,dd,i,no,numsub,dblquot,quot
 n ref,sub,subNo,subscripts1,type,valquot,value,xref,zobj
 ;
 s cr=$c(13,10),comma=","
 s (quot,dblquot,valquot)=""""
 s dd=$d(@name)
 i dd=1!(dd=11) d  i dd=1 QUIT json
 . s value=@name
 . i value'[">" q
 . s json=$$walkArray(json,value,.subscripts)
 s ref=name_"("
 s no=$o(subscripts(""),-1)
 i no>0 f i=1:1:no d
 . i subscripts(i)[quot s subscripts(i)=$$replaceAll(subscripts(i),quot,quot_quot)
 . i subscripts(i)?."-"1N.N s quot=""
 . s ref=ref_quot_subscripts(i)_quot_","
 . s quot=dblquot
 s ref=ref_"sub)"
 s sub="",numsub=0,subNo=0,count=0
 s allNumeric=1
 f  s sub=$o(@ref) q:sub=""  d  q:'allNumeric
 . i sub'?1N.N s allNumeric=0
 . s count=count+1
 . i sub'=count s allNumeric=0
 ;i allNumeric,count=1 s allNumeric=0
 s allNumeric=0
 i allNumeric d
 . s json=json_"["
 e  d
 . s json=json_"{"
 s sub=""
 f  s sub=$o(@ref) q:sub=""  d
 . s subscripts(no+1)=sub
 . s subNo=subNo+1
 . s dd=$d(@ref)
 . i dd=1 d
 . . s value=@ref
 . . ;i value["\" s value=$$replaceAll(value,"\","\\")
 . . s value=$$removeControlChars(value)
 . . i 'allNumeric d
 . . . ;i sub["\",sub'["\\",sub'["\"""  s sub=$$replaceAll(sub,"\","\\")
 . . . i sub["\" s sub=$$replaceAll(sub,"\","\\")
 . . . s sub=$$removeControlChars(sub)
 . . . s json=json_""""_sub_""":"
 . . s type="literal"
 . . i $$numeric(value) s type="numeric"
 . . i value="true"!(value="false") s type="boolean"
 . . i $e(value,1)="{",$e(value,$l(value))="}" s type="variable"
 . . i type="literal" d
 . . . ;i value[quot s value=$$replaceAll(value,quot,"\"_quot)
 . . . ;i value["\",value'["\\",value'["\"""  s value=$$replaceAll(value,"\","\\")
 . . . i value["\" s value=$$replaceAll(value,"\","\\")
 . . . i value[quot s value=$$replaceAll(value,quot,"\""")
 . . . s value=valquot_value_valquot
 . . d
 . . . s json=json_value_","
 . k subscripts1
 . m subscripts1=subscripts
 . i dd>9 d
 . . n subx
 . . ;i sub?1N.N d
 . . ;. i subNo=1 d
 . . ;. . s numsub=1
 . . ;. . s json=$e(json,1,$l(json)-1)_"["
 . . ;e  d
 . . ;. s json=json_""""_sub_""":"
 . . ;i sub["\",sub'["\\",sub'["\""" s sub=$$replaceAll(sub,"\","\\")
 . . s subx=sub
 . . i subx["\" s subx=$$replaceAll(sub,"\","\\")
 . . i subx[quot s subx=$$replaceAll(subx,quot,"\""")
 . . s subx=$$removeControlChars(subx)
 . . s json=json_""""_subx_""":"
 . . s json=$$walkArray(json,name,.subscripts1)
 . . d
 . . . s json=json_","
 ;
 s json=$e(json,1,$l(json)-1)
 i allNumeric d
 . s json=json_"]"
 e  d
 . s json=json_"}"
 QUIT json ; exit!
 ;
trace(text,clear) ; trace  ;
 n i
 s text=$g(text)
 i $g(clear)=1 k ^%zewdTrace
 s i=$increment(^%zewdTrace)
 s ^%zewdTrace(i)=text
 QUIT
 ;
parseJSON(jsonString,propertiesArray,mode)
 ;
 n array,arrRef,buff,c,error
 ;
 k propertiesArray
 s error=""
 s buff=$g(jsonString)
 s buff=$$replaceAll(buff,"\""","\'")
 s arrRef="array"
 s c=$e(buff,1)
 s buff=$e(buff,2,$l(buff))
 d
 . i c="{" d  q
 . . n prefix
 . . s prefix="""zobj1"""
 . . i $g(mode)=1 s prefix=""
 . . s error=$$parseJSONObject(.buff,prefix)
 . . q:error
 . . i buff'="" s error=1
 . i c="[" d  q
 . . n prefix
 . . s prefix=1
 . . i $g(mode)=1 s prefix=""
 . . s error=$$parseJSONArray(.buff,prefix)
 . . q:error
 . . i buff'="" s error=1
 . s error=1
 i error=1 QUIT "Invalid JSON"
 m propertiesArray=array
 QUIT ""
 ;
parseJSONObject(buff,subs)
 n c,error,name,stop,subs2,value,x
 s stop=0,name="",error=""
 f  d  q:stop
 . s c=$e(buff,1)
 . i c="" s error=1,stop=1 q
 . s buff=$e(buff,2,$l(buff))
 . i c="[" s error=1,stop=1 q
 . i c="}" d  q
 . . s stop=1
 . i c=":" d  q
 . . n subs2
 . . s value=$$getJSONValue(.buff)
 . . d  q:stop
 . . . i value="" q
 . . . i $e(value,1)="""",$e(value,$l(value))="""" q
 . . . i value="true"!(value="false") s value=""""_value_"""" q
 . . . i $$numeric(value) q
 . . . s error=1,stop=1
 . . i value="",$e(buff,1)="{" d  q
 . . . i $e(name,1)'="""",$e(name,$l(name))'="""" s name=""""_name_""""
 . . . s subs2=subs
 . . . i subs'="" s subs2=subs2_","
 . . . s subs2=subs2_name
 . . . i $g(mode)="" s subs2=subs2_",""zobj1"""
 . . . s buff=$e(buff,2,$l(buff))
 . . . s error=$$parseJSONObject(.buff,subs2)
 . . . i error=1 s stop=1 q
 . . i value="",$e(buff,1)="[" d  q
 . . . ;s subs2=subs_","""_name_""",""1"""
 . . . i $e(name,1)'="""",$e(name,$l(name))'="""" s name=""""_name_""""
 . . . s subs2=subs
 . . . i subs'="" s subs2=subs2_","
 . . . s subs2=subs2_name
 . . . s buff=$e(buff,2,$l(buff))
 . . . s error=$$parseJSONArray(.buff,subs2)
 . . . i error=1 s stop=1 q
 . . i $e(name,1)="""",$e(name,$l(name))'="""" s error=1,stop=1 q
 . . i $e(name,1)'="""",$e(name,$l(name))="""" s error=1,stop=1 q
 . . i $e(name,1)'="""",$e(name,$l(name))'="""" s name=""""_name_""""
 . . s subs2=subs
 . . i subs'="" s subs2=subs2_","
 . . s subs2=subs2_name
 . . i value["\'" s value=$$replaceAll(value,"\'","""""")
 . . s x="s "_arrRef_"("_subs2_")="_value
 . . x x
 . i c="," s name="" q
 . s name=name_c q
 QUIT error
 ;
parseJSONArray(buff,subs)
 n c,error,name,no,stop,subs2,value,x
 s stop=0,name="",no=0,error=""
 f  d  q:stop
 . s c=$e(buff,1)
 . i c="" s error=1,stop=1 q
 . s buff=$e(buff,2,$l(buff))
 . i c=":" s error=1,stop=1 q
 . i c="]" d  q
 . . s stop=1
 . . i name="" q
 . . s no=no+1
 . . s subs2=subs
 . . i subs'="" s subs2=subs2_","
 . . s subs2=subs2_no
 . . s x="s "_arrRef_"("_subs2_")="_name
 . . x x
 . i c="[" d  q
 . . s no=no+1
 . . s subs2=subs
 . . i subs'="" s subs2=subs2_","
 . . s subs2=subs2_no
 . . ;s buff=$e(buff,2,$l(buff))
 . . s error=$$parseJSONArray(.buff,subs2)
 . . i error=1 s stop=1 q
 . i c="{" d  q
 . . s no=no+1
 . . s subs2=subs
 . . i subs'="" s subs2=subs2_","
 . . s subs2=subs2_no
 . . i $g(mode)="" s subs2=subs2_",""zobj1"""
 . . ;s buff=$e(buff,2,$l(buff))
 . . s error=$$parseJSONObject(.buff,subs2)
 . . i error=1 s stop=1 q
 . s subs2=subs
 . i subs'="" s subs2=subs2_","
 . s subs2=subs2_""""_name_""""
 . i c="," d  q
 . . i name="" q
 . . d  q:stop
 . . . i $e(name,1)="""",$e(name,$l(name))="""" q
 . . . ;i value="true"!(value="false") s value=""""_value_"""" q
 . . . i $$numeric(name) q
 . . . s error=1,stop=1
 . . s no=no+1
 . . s subs2=subs
 . . i subs'="" s subs2=subs2_","
 . . s subs2=subs2_""""_no_""""
 . . s x="s "_arrRef_"("_subs2_")="_name
 . . x x
 . . s name=""
 . s name=name_c q
 QUIT error
 ;
getJSONValue(buff)
 n c,isLiteral,lc,stop,value
 s stop=0,value="",isLiteral=0,lc=""
 f  d  q:stop  q:buff=""
 . s c=$e(buff,1)
 . i value="",c="""" s isLiteral=1
 . i 'isLiteral,c="[" s stop=1 q
 . i 'isLiteral,c="{" s stop=1 q
 . i c="}" d  q:stop
 . . i isLiteral,lc'="""" q
 . . s stop=1
 . i c="," d  q:stop
 . . i isLiteral,lc'="""" q
 . . s stop=1
 . s buff=$e(buff,2,$l(buff))
 . s value=value_c
 . s lc=c
 QUIT value
 ;
numeric(value)
 i $e(value,1)=0,$l(value)>1 QUIT 0
 i $e(value,1,2)="-0",$l(value)>2,$e(value,1,3)'="-0." QUIT 0
 i value?1N.N QUIT 1
 i value?1"-"1N.N QUIT 1
 i value?1N.N1"."1N.N QUIT 1
 i value?1"-"1N.N1"."1N.N QUIT 1
 i value?1"."1N.N QUIT 1
 i value?1"-."1N.N QUIT 1
 QUIT 0
 ;
removeControlChars(string)
 n c,i,newString
 s newString=""
 f i=1:1:$l(string) d
 . s c=$e(string,i)
 . i $a(c)<32 s c="~"
 . s newString=newString_c
 QUIT newString
 ;
utfConvert(input)
 ; Unescape UTF-8 characters
 i input[$c(195) d
 . n buf,c1,i,no,p
 . s buf=$p(input,$c(195),1)
 . s no=$l(input,$c(195))
 . f i=2:1:no d
 . . s p=$p(input,$c(195),i)
 . . s c1=$e(p,1)
 . . s c1=$c($a(c1)+64)
 . . s buf=buf_c1_$e(p,2,$l(p))
 . s input=buf
 s input=$tr(input,$c(194),"")
 QUIT input
 ;

