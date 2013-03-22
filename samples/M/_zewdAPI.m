%zewdAPI	; Enterprise Web Developer run-time functions and user APIs
 ;
 ; Product: Enterprise Web Developer (Build 944)
 ; Build Date: Fri, 23 Nov 2012 17:15:06
 ; 
 ; ----------------------------------------------------------------------------
 ; | Enterprise Web Developer for GT.M and m_apache                           |
 ; | Copyright (c) 2004-12 M/Gateway Developments Ltd,                        |
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
 ;QUIT
 ;
 ;
version() ;
 QUIT "Enterprise Web Developer (Build "_$$getVersion^%zewdCompiler()_")"
 ;
date() ;
 QUIT $$getDate^%zewdCompiler()
 ;
compilePage(app,page,mode,technology,outputPath,multilingual,maxLines)
 d compilePage^%zewdCompiler($g(app),$g(page),$g(mode),$g(technology),$g(outputPath),$g(multilingual),$g(maxLines))
 QUIT
 ;
compileAll(app,mode,technology,outputPath,multilingual,templatePageName,maxLines)
 d compileAll^%zewdCompiler($g(app),$g(mode),$g(technology),$g(outputPath),$g(multilingual),$g(templatePageName),$g(maxLines))
 QUIT
 ;
autoTranslate(app,language,verbose)
 d autoTranslate^%zewdMgr($g(app),$g(language),$g(verbose))
 ;
startSession(page,requestArray,serverArray,sessionArray,filesArray) ;
 ;
 QUIT $$startSession^%zewdPHP(page,.requestArray,.serverArray,.sessionArray,.filesArray)
 ;
closeSession(requestArray) ;
 ;
 QUIT $$closeSession^%zewdPHP(.requestArray)
 ;
saveSession(sessionArray) ;
 ;
 d saveSession^%zewdPHP(.sessionArray)
 QUIT
 ;
endOfPage(sessionArray)
 ;
 d endOfPage^%zewdPHP(.sessionArray)
 QUIT
 ;
prePageScript(sessid)
 QUIT $$prePageScript^%zewdPHP(sessid)
 ;
releaseLock(sessid)
 d releaseLock^%zewdPHP(sessid)
 QUIT
 ;
tokeniseURL(url,sessid)
 QUIT $$tokeniseURL^%zewdCompiler16($g(url),$g(sessid))
 ;
getSessid(token)
 ;
 i token="" QUIT ""
 i $$isTokenExpired(token) QUIT ""
 QUIT $p(^%zewdSession("tokens",token),"~",1)
 ;
initialiseSession(sessid)
 k ^%zewdSession("session",sessid)
 QUIT
 ;
deleteSession(sessid)
 ;
 d deleteSession^%zewdPHP(sessid)
 ;
 QUIT
 ;
changeApp(appName,sessid)
 i $g(appName)="" QUIT
 i $g(sessid)="" QUIT
 d setSessionValue("ewd_appName",appName,sessid)
 QUIT
 ;
setRedirect(toPage,sessid,app)
 i $g(app)'="" d
 . d setSessionValue("ewd_appName",app,sessid)
 d setJump(toPage,sessid,$g(app))
 QUIT
 ;
setJump(toPage,sessid,app)
 ;
 n token
 ;
 i $g(app)'="",$e(sessid,1,4)="csp:" d
 . n path
 . s path=$$getRootURL("csp")
 . i $e(path,$l(path))'="/" s path=path_"/"
 . s toPage=path_app_"/"_toPage
 d setSessionValue("ewd_jump",toPage,sessid)
 d setSessionValue("ewd_nextPage",toPage,sessid)
 QUIT:$e(sessid,1,4)="csp:"
 s token=$$setNextPageToken(toPage,sessid)
 d setSessionValue("ewd_pageToken",token,sessid)
 QUIT
 ;
setNextPageToken(nextPage,sessid)
 ;
 n token,length
 ;
 s length=$$getSessionValue("ewd_sessid_length",sessid)
 i length="" s length=30
 i $g(sessid)="" s sessid=0
 f  s token=$$makeTokenString(length) q:'$d(^%zewdSession("nextPageTokens",sessid,token))
 i $g(^zewd("trace"))=1 d trace^%zewdAPI("setNextPageToken^%zewdAPI: sessid="_sessid_"; token="_token_"; nextPage="_nextPage)
 s ^%zewdSession("nextPageTokens",sessid,token,$$zcvt(nextPage,"l"))=""
 QUIT token
 ; 
isNextPageTokenValid(token,sessid,page)
 QUIT $$isNextPageTokenValid^%zewdCompiler13(token,sessid,page)
 ;
isCSP(sessid)
 QUIT $e(sessid,1,4)="csp:"
 ;
normaliseTextValue(text)
 s text=$$replaceAll(text,"&#39;","'")
 QUIT $$zcvt(text,"o","HTML")
 ;
writeLine(line,technology)
 i technology="node" d
 . s ^CacheTempBuffer($j,$increment(^CacheTempBuffer($j)))=line
 e  d
 . w line
 QUIT
 ;
displayOptions(fieldName,listName,escape)
 ;d displayOptions^%zewdCompiler13($g(fieldName),$g(listName),$g(escape))
 n codeValue,%d,i,line,name,nnvp,nvp,pos,technology,textValue,value
 ;
 s technology=$$getSessionValue^%zewdAPI("ewd.technology",sessid)
 s fieldName=$tr(fieldName,".","_")
 s listName=$tr(listName,".","_")
 i 0
 e  d
 . s escape=+$g(escape)
 . s pos=""
 . f  s pos=$o(^%zewdSession("session",sessid,"ewd_list",listName,pos)) q:pos=""  d
 . . k %d,textValue,codeValue,codeValueEsc,textValueEsc
 . . s %d=^%zewdSession("session",sessid,"ewd_list",listName,pos)
 . . s textValue=$p(%d,$c(1),1)
 . . ;
 . . s textValueEsc=textValue
 . . i $g(^zewd("xssEncoding")) d
 . . . s textValueEsc=$$htmlOutputEncode^%zewdAPI2(textValueEsc)
 . . e  d 
 . . . s textValueEsc=$$replaceAll(textValueEsc,"&#39;","'")
 . . . i escape s textValueEsc=$$zcvt(textValue,"o","HTML")
 . . ;
 . . s codeValue=$p(%d,$c(1),2)
 . . i codeValue="" s codeValue=textValue
 . . s codeValueEsc=codeValue
 . . i $g(^zewd("xssEncoding")) d
 . . . s codeValueEsc=$$htmlOutputEncode^%zewdAPI2(codeValueEsc)
 . . e  d 
 . . . s codeValueEsc=$$replaceAll(codeValueEsc,"&#39;","'")
 . . . i escape s codeValueEsc=$$zcvt(codeValue,"o","HTML")
 . . s line="<option value='"_codeValueEsc_"'"
 . . d writeLine(line,technology)
 . . i $e(fieldName,1)'="$" d
 . . . n fn
 . . . s fn=$tr(fieldName,"_",".")
 . . . i $$getSessionValue(fn,sessid)=codeValue d  q
 . . . . s line=" selected='selected'"
 . . . . d writeLine(line,technology)
 . . . i $d(^%zewdSession("session",sessid,"ewd_selected",fieldName,codeValue)) d  q
 . . . . s line=" selected='selected'"
 . . . . d writeLine(line,technology)
 . . i $e(fieldName,1)="$" d
 . . . n fieldValue
 . . . s fieldValue=$e(fieldName,2,$l(fieldName))
 . . . s fieldValue=$g(@fieldValue)
 . . . i fieldValue=codeValue d
 . . . . s line=" selected='selected'"
 . . . . d writeLine(line,technology)
 . . s nnvp=$l(%d,$c(1))
 . . f i=3:1:nnvp d
 . . . s nvp=$p(%d,$c(1),i)
 . . . i nvp="" q
 . . . s name=$p(nvp,$c(3),1)
 . . . s value=$p(nvp,$c(3),2)
 . . . s line=" "_name_"='"_value_"'"
 . . . d writeLine(line,technology)
 . . s line=">"_textValueEsc_"</option>"_$c(13,10)
 . . d writeLine(line,technology)
 QUIT
 ;
displayTextArea(fieldName)
 d displayTextArea^%zewdCompiler13($g(fieldName))
 QUIT
 ;
mCSPReq2(fields)
 ;
 n i,noOfFields,field,type
 s noOfFields=$l(fields,"`")
 f i=1:1:noOfFields d
 . s field=$p(fields,"`",i)
 . q:field=""
 . s type=$p(field,"|",2)
 . S field=$P(field,"|",1)
 . d mergeCSPRequestToSession(field,type)
 d mergeCSPRequestToSession("ewd_pressed","hidden")
 QUIT
 ;
mCSPReq(fieldName,type)
 d mergeCSPRequestToSession(fieldName,type)
 QUIT
 ;
mergeCSPRequestToSession(fieldName,type)
 d mergeCSPRequestToSession^%zewdCompiler16($g(fieldName),$g(type))
 QUIT
 ;
 ; note - textarea data storage can be queried using SQL with the following construct
 ; 
 ; listAttributeFL {type=%Library.String ; sqllisttype=subnode}
 ;
displayText(textID,reviewMode,sessid)
	QUIT $$displayText^%zewdCompiler13($g(textID),$g(reviewMode),$g(sessid))
	;
systemMessage(text,type,sessid,appName,langCode)
 n textid,fragments,outputText,error,technology,translationMode,typex
 ;
 ;d trace^%zewdAPI("systemMessage : text="_text_" ; type="_type_" ; sessid="_sessid)
 i $g(text)="" QUIT ""
 ; manual API or where sessid not known
 i $g(sessid)="" QUIT $$systemMessage^%zewdCompiler5(text,$g(type),$g(appName),$g(langCode))
 s translationMode=+$$getSessionValue^%zewdAPI("ewd_translationMode",sessid)
 ;d trace^%zewdAPI("ewd_translationMode="_translationMode)
 i 'translationMode QUIT text
 s appName=$$getSessionValue^%zewdAPI("ewd_appName",sessid)
 ;d trace^%zewdAPI("appName="_appName)
 s typex=type ; avoid Cache bug !
 i $$getPhraseIndex^%zewdCompiler5(text)="" QUIT ""
 i '$$isTextPreviouslyFound^%zewdCompiler5(text,appName,"","",.textid,,,type) d
 . s textid=$$addTextToIndex^%zewdCompiler5(text,appName,"","",.fragments,.outputText,typex)
 s error=$$displayText(textid,0,sessid)
 QUIT error
 ;
errorMessage(text,sessid)
 QUIT $$systemMessage(text,"error",sessid)
 ;
 ; ============================================================================
 ;   User API Methods
 ; ============================================================================
 ;
isCSPPage(docOID)
 ;
 n docName
 ;
 s docName=$$getDocumentName^%zewdDOM(docOID)
 QUIT $$bypassMode^%zewdCompiler(docName)
 ;
getSessionValue(name,sessid)
 ;
 n %zt,return,technology,value
 ;
 s name=$$stripSpaces(name)
 i $g(name)="" QUIT ""
 i $g(sessid)="" QUIT ""
 i name["." d  QUIT value
 . n np,obj,prop
 . i name["_" s name=$p(name,"_",1)_"."_$p(name,"_",2,200)
 . s np=$l(name,".")
 . s obj=$p(name,".",1,np-1)
 . s prop=$p(name,".",np)
 . s value=$$getSessionObject(obj,prop,sessid)
 ;s $zt="extcErr"
 ;i $r(100)<10 i '$$$licensed("DOM",,,,,,,,,,) d setWarning("You do not have a current eXtc License",sessid)
 ;i $$isTemp(name) d  QUIT value
 i $e(name,1,4)="tmp_" d  QUIT value
 . s value=$g(zewdSession(name))
 . i value="",$g(^%zewdSession("session",sessid,"ewd_technology"))="gtm" s value=$g(sessionArray(name))
 QUIT $g(^%zewdSession("session",sessid,name))
 ;
setWLDSymbol(name,sessid)
 ;
 ;  ------------------------------------------------------
 ;  Duplicate copy for performance: see also %zewdPHP!
 ;  ------------------------------------------------------
 ;
 n wldAppName,wldName,wldSessid,%zzname
 ;
 QUIT:$zv["GT.M"
 QUIT
 ;
extcErr
 ;
 n mess
 s mess="eXtc does not appear to have been installed or is unavailable in the "_$$namespace()_" namespace where your application is attempting to run.  Your application will be unable to run correctly"
 d setWarning(mess,sessid)
 s $zt=%zt
 QUIT ""
 ;
valueErr ;
 s $zt=$g(%zt)
 QUIT ""
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
exportCustomTags(tagList,filepath)
 QUIT $$exportCustomTags^%zewdCompiler16(.tagList,$g(filepath))
 ;
exportAllCustomTags(filepath)
 QUIT $$exportAllCustomTags^%zewdCompiler16($g(filepath))
 ;
importCustomTags(filePath)
 QUIT $$importCustomTags^%zewdForm($g(filePath))
 ;
setSessionValue(name,value,sessid)
 ;
 s name=$$stripSpaces(name)
 i $g(name)="" QUIT
 i $g(sessid)="" QUIT
 i name["." d  QUIT
 . n np,obj,prop
 . i name["_" s name=$p(name,"_",1)_"."_$p(name,"_",2,200)
 . s np=$l(name,".")
 . s obj=$p(name,".",1,np-1)
 . s prop=$p(name,".",np)
 . d setSessionObject(obj,prop,value,sessid)
 s value=$g(value)
 i $e(name,1,4)="tmp_" s zewdSession(name)=value QUIT
 s ^%zewdSession("session",sessid,name)=value
 QUIT
 ;
allowJSONAccess(sessionName,access,sessid)
 ; access="r|rw"
 s ^%zewdSession("jsonAccess",sessid,sessionName)=access
 QUIT
 ;
disallowJSONAccess(sessionName,sessid)
 k ^%zewdSession("jsonAccess",sessid,sessionName)
 QUIT
 ;
JSONAccess(sessionName,sessid)
 QUIT $g(^%zewdSession("jsonAccess",sessid,sessionName))
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
isTemp(name)
 QUIT $e(name,1,4)="tmp_"
 ;
 ;
existsInSession(name,sessid)
 QUIT $$existsInSession^%zewdCompiler13($g(name),$g(sessid))
 ;
existsInSessionArray(name,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)
 QUIT $$existsInSessionArray^%zewdCompiler13($g(name),$g(p1),$g(p2),$g(p3),$g(p4),$g(p5),$g(p6),$g(p7),$g(p8),$g(p9),$g(p10),$g(p11))
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
clearSessionArray(arrayName,sessid)
 s arrayName=$$stripSpaces(arrayName)
 i $g(sessid)="" QUIT
 i $g(arrayName)="" QUIT
 s arrayName=$tr(arrayName,".","_")
 ;i $$isTemp(arrayName) k zewdSession(arrayName) QUIT
 i $e(arrayName,1,4)="tmp_" k zewdSession(arrayName) QUIT
 k ^%zewdSession("session",sessid,arrayName)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
setSessionArray(arrayName,itemName,itemValue,sessid)
 ;
 s arrayName=$$stripSpaces(arrayName)
 QUIT:$g(arrayName)=""
 QUIT:$g(itemName)=""
 QUIT:$g(sessid)=""
 s arrayName=$tr(arrayName,".","_")
 i $$isTemp(arrayName) s zewdSession(arrayName,itemName)=itemValue QUIT
 s ^%zewdSession("session",sessid,arrayName,itemName)=itemValue
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
getSessionArray(arrayName,sessid,array,clearArray)
 ;
 s arrayName=$$stripSpaces(arrayName)
 QUIT:$g(arrayName)=""
 s arrayName=$tr(arrayName,".","_")
 QUIT:$g(sessid)=""
 set $zt="getSessionArrayErr"
 i $g(clearArray)=1 k array
 i $$isTemp(arrayName) m array=zewdSession(arrayName) QUIT
 m array=^%zewdSession("session",sessid,arrayName)
 QUIT
 ;
getSessionArrayErr ; --- Come here if error occurred in 'getSessionArray' ---
 set $zt=""
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
addToSession(name,sessid)
 s name=$$stripSpaces(name)
 QUIT:$g(sessid)=""
 QUIT:$g(name)=""
 s name=$tr(name,".","_")
 i $$isTemp(name) m zewdSession(name)=@name QUIT
 m ^%zewdSession("session",sessid,name)=@name
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
mergeToSession(name,sessid)
 QUIT:$g(sessid)=""
 QUIT:$g(name)=""
 d addToSession(name,sessid)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
mergeGlobalToSession(globalName,sessionName,sessid)
 d mergeGlobalToSession^%zewdCompiler13($g(globalName),$g(sessionName),$g(sessid))
 QUIT
 ;
mergeGlobalFromSession(globalName,sessionName,sessid)
 d mergeGlobalFromSession^%zewdCompiler13($g(globalName),$g(sessionName),$g(sessid))
 QUIT
 ;
mergeArrayToSession(array,sessionName,sessid)
 QUIT:$g(sessid)=""
 QUIT:$g(sessionName)=""
 s sessionName=$tr(sessionName,".","_")
 ;i $$isTemp(sessionName) m zewdSession(sessionName)=array QUIT
 i $e(sessionName,1,4)="tmp_" m zewdSession(sessionName)=array QUIT
 m ^%zewdSession("session",sessid,sessionName)=array
 QUIT
 ;
mergeArrayToSessionObject(array,sessionName,sessid)
 d mergeArrayToSessionObject^%zewdCompiler16(.array,$g(sessionName),$g(sessid))
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
mergeArrayFromSession(array,sessionName,sessid)
 QUIT:$g(sessid)=""
 QUIT:$g(sessionName)=""
 s sessionName=$tr(sessionName,".","_")
 ;i $$isTemp(sessionName) m array=zewdSession(sessionName) QUIT
 i $e(sessionName,1,4)="tmp_" m array=zewdSession(sessionName) QUIT
 m array=^%zewdSession("session",sessid,sessionName)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
mergeFromSession(name,sessid)
 QUIT:$g(sessid)=""
 QUIT:$g(name)=""
 s name=$tr(name,".","_")
 i $$isTemp(name) m @name=zewdSession(name)
 m @name=^%zewdSession("session",sessid,name)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
deleteFromSession(name,sessid)
 QUIT:$g(sessid)=""
 QUIT:$g(name)=""
 i name["." d  QUIT
 . n np,obj,prop
 . s np=$l(name,".")
 . s obj=$p(name,".",1,np-1)
 . s prop=$p(name,".",np)
 . d deleteFromSessionObject(obj,prop,sessid)
 ;i $$isTemp(name) k zewdSession(name) QUIT
 i $e(name,1,4)="tmp_" k zewdSession(name) QUIT
 k ^%zewdSession("session",sessid,name)
 QUIT
 ;
sessionNameExists(name,sessid)
 QUIT:$g(sessid)=""
 QUIT:$g(name)=""
 s name=$tr(name,".","_")
 i $$isTemp(name) QUIT $d(zewdSession(name))
 QUIT $d(^%zewdSession("session",sessid,name))
 ;
getSessionArrayValue(arrayName,subscript,sessid,exists)
 QUIT $$getSessionArrayValue^%zewdCompiler13($g(arrayName),$g(subscript),$g(sessid),.exists)
 ;
sessionArrayValueExists(arrayName,subscript,sessid)
 QUIT $$sessionArrayValueExists^%zewdCompiler13($g(arrayName),$g(subscript),$g(sessid))
 ;
deleteSessionArrayValue(arrayName,subscript,sessid)
 d deleteSessionArrayValue^%zewdCompiler13($g(arrayName),$g(subscript),$g(sessid))
 QUIT
 ;
 ; Objects
 ;
setSessionObject(objectName,propertyName,propertyValue,sessid)
 ;d setSessionObject^%zewdCompiler13($g(objectName),$g(propertyName),$g(propertyValue),$g(sessid))
 ;QUIT
	;
	n comma,i,np,p,sessionArray,x
	;
	i $g(objectName)="" QUIT
	i $g(propertyName)="" QUIT
	;i $g(propertyValue)="" QUIT
	i $g(sessid)="" QUIT
    s np=$l(objectName,".")
    ;s objectName=$$replace(objectName,".","_")
    i objectName["." s objectName=$p(objectName,".",1)_"_"_$p(objectName,".",2,2000)
    i np=1 d  QUIT
	. i $e(objectName,1,3)="tmp" s zewdSession(objectName_"_"_propertyName)=propertyValue  q
	. s ^%zewdSession("session",sessid,(objectName_"_"_propertyName))=propertyValue
    ;
    f i=1:1:np-1 s p(i)=$p(objectName,".",i)
    s comma=","
    i $e(objectName,1,4)="tmp_" d
    . s x="s zewdSession(",comma=""
	e  d
    . s x="s ^%zewdSession(""session"","""_sessid_""""
    i propertyValue["""" d
    . s propertyValue=$$replaceAll^%zewdAPI(propertyValue,"""",$c(4))
    . s propertyValue=$$replaceAll^%zewdAPI(propertyValue,$c(4),"""""")
    f i=1:1:np-1 s x=x_comma_""""_p(i)_"""",comma=","
    s x=x_","""_propertyName_""")="""_propertyValue_""""
    x x
    QUIT
 ;
getSessionObject(objectName,propertyName,sessid)
    ;
    n i,np,p,value,x
    ;
    i $g(sessid)="" QUIT ""
    s value=""
    s np=$l(objectName,".")
    i objectName[".",objectName'["_" s objectName=$p(objectName,".",1)_"_"_$p(objectName,".",2,2000)
    ;s objectName=$$replace(objectName,".","_")
    i np=1 QUIT $g(^%zewdSession("session",sessid,(objectName_"_"_propertyName)))
    ;
    f i=1:1:np-1 s p(i)=$p(objectName,".",i)
    s x="s value=$g(^%zewdSession(""session"","""_sessid_""""
    f i=1:1:np-1 s x=x_","""_p(i)_""""
    s x=x_","""_propertyName_"""))"
    x x
    QUIT value
    ;
deleteFromSessionObject(objectName,propertyName,sessid)
 d deleteFromSessionObject^%zewdCompiler13($g(objectName),$g(propertyName),$g(sessid))
 QUIT
 ;
sessionObjectPropertyExists(objectName,propertyName,sessid)
 QUIT $$sessionObjectPropertyExists^%zewdCompiler13($g(objectName),$g(propertyName),$g(sessid))
 ;
deleteSessionObject(objectName,sessid)
 n obj
 s obj=objectName
 i obj["." s obj=$tr(obj,".","_")
 i obj'["_" s obj=obj_"_"
 d clearSessionByPrefix(obj,$g(sessid))
 ;d deleteSessionObject^%zewdCompiler13($g(objectName),$g(sessid))
 QUIT
 ;
copyObjectToSession(oref,objectName,sessid)
 d copyObjectToSession^%zewdCompiler13($g(oref),$g(objectName),$g(sessid))
 QUIT
 ;
copyResultSetToSession(oref,objectName,sessid)
 d copyResultSetToSession^%zewdCompiler13($g(oref),$g(objectName),$g(sessid))
 QUIT
 ;
getResultSetValue(resultSetName,index,propertyName,sessid)
 QUIT $$getResultSetValue^%zewdCompiler13($g(resultSetName),$g(index),$g(propertyName),$g(sessid))
 ;
addToResultSet(sessionName,propertyName,value,sessid)
 d addToResultSet^%zewdCompiler13($g(sessionName),$g(propertyName),$g(value),$g(sessid))
 QUIT
 ;
mergeRecordArrayToResultSet(sessionName,recordArray,sessid)
 d mergeRecordArrayToResultSet^%zewdCompiler13($g(sessionName),.recordArray,$g(sessid))
 QUIT
 ;
JSONToSessionObject(objectName,jsonString,sessid)
 d JSONToSessionObject^%zewdCompiler13($g(objectName),$g(jsonString),$g(sessid))
 QUIT
 ;
sessionObjectToJSON(objectName,sessid)
 QUIT $$sessionObjectToJSON^%zewdCompiler13($g(objectName),$g(sessid))
 ;
objectGlobalToJSON(objectName)
 QUIT $$objectGlobalToJSON^%zewdCompiler13($g(objectName))
 ;
saveJSON(objectName,jsonString)
 QUIT $$saveJSON^%zewdCompiler13($g(objectName),$g(jsonString))
 ;
getJSON(objectName,addRefCol)
 QUIT $$getJSON^%zewdCompiler13($g(objectName),$g(addRefCol))
 ;
setJSONValue(JSONName,objectName,sessid)
 d setJSONValue^%zewdCompiler16($g(JSONName),$g(objectName),$g(sessid))
 d allowJSONAccess(objectName,"r",sessid)
 QUIT
 ;
convertToJSON(arrayName,isExtJS)
 n dojo
 i '$d(@arrayName) QUIT ""
 s dojo=""
 i $g(isExtJS)=1 s dojo=2
 QUIT $$walkArray^%zewdCompiler13("",arrayName,dojo)
 ;
mergeToJSObject(sessionObject,JSObject)
 QUIT $$mergeToJSObject^%zewdCompiler13($g(sessionObject),$g(JSObject),$g(sessid))
 ;
 ; Javascript objects
 ;
getJavascriptObjectBlock(objectName,docName,textArray)
 QUIT $$getJavascriptObjectBlock^%zewdCompiler13($g(objectName),$g(docName),.textArray)
 ;
replaceJavascriptObject(objectName,newFunctionText,docName)
 QUIT $$replaceJavascriptObject^%zewdCompiler13($g(objectName),$g(newFunctionText),$g(docName))
 ;
replaceJavascriptObjectBody(functionName,newBody,docName)
 QUIT $$replaceJavascriptObjectBody^%zewdCompiler13($g(functionName),$g(newBody),$g(docName))
 ;
getJavascriptObjectBody(functionName,docName)
 QUIT $$getJavascriptObjectBody^%zewdCompiler13($g(functionName),$g(docName))
 ;
getJavascriptObject(objectName,docName,eOID)
 QUIT $$getJavascriptObject^%zewdCompiler13($g(objectName),$g(docName),$g(eOID))
 ;
javascriptObjectExists(objectName,docName)
 QUIT $$javascriptObjectExists^%zewdCompiler13($g(objectName),$g(docName))
 ;
getLastJavascriptTag(docName,textArray)
 QUIT $$getLastJavascriptTag^%zewdCompiler13($g(docName),.textArray)
 ;
addJavascriptObject(docName,jsText)
 QUIT $$addJavascriptObject^%zewdCompiler13($g(docName),.jsText)
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
setSessionValues(nvArray,sessid)
 ;
 QUIT:$g(sessid)=""
 n name,no,value
 s name=""
 f  s name=$o(nvArray(name)) q:name=""  d
 . d deleteFromSession(name,sessid)
 . d clearSelected(name,sessid)
 . s value=$g(nvArray(name))
 . d setSessionValue(name,value,sessid)
 . s no=""
 . f  s no=$o(nvArray(name,no)) q:no=""  d
 . . s value=nvArray(name,no)
 . . d addToSelected(name,value,sessid)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
getSessionValues(prefix,nvArray,sessid)
 ;
 n len,name,no,value
 QUIT:$g(sessid)=""
 QUIT:$g(prefix)=""
 set $zt="getSessionValuesErr"
 s len=$l(prefix)
 k nvArray
 s name=prefix
 f  s name=$o(^%zewdSession("session",sessid,name)) q:name=""  q:$e(name,1,len)'=prefix  d
 . d setNVArray(name,.nvArray,sessid)
 s name=prefix,no=0
 f  s name=$o(^%zewdSession("session",sessid,"ewd_selected",name)) q:name=""  q:$e(name,1,len)'=prefix  d
 . s value=""
 . f  s value=$o(^%zewdSession("session",sessid,"ewd_selected",name,value)) q:value=""  d
 . . s no=no+1
 . . s nvArray(name,no)=value
 QUIT
 ;
getSessionValuesErr ; --- Come here if error occurred in 'getSessionValues' ---
 set $zt=""
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
getSessionValuesByPrefix(prefix,sessid)
 ;
 n len,name
 QUIT:$g(sessid)=""
 QUIT:$g(prefix)=""
 s prefix=$tr(prefix,".","_")
 set $zt="getSessionValuesByPrefixErr"
 s len=$l(prefix)
 s name=prefix
 f  s name=$o(^%zewdSession("session",sessid,name)) q:name=""  q:$e(name,1,len)'=prefix  d
 . i name?1A.AN m @name=^%zewdSession("session",sessid,name)
 QUIT
 ;
getSessionValuesByPrefixErr
 set $zt=""
 QUIT
 ;
setNVArray(name,nvArray,sessid)
 n selected,value,no
 s nvArray(name)=$$getSessionValue(name,sessid)
 QUIT
 ;
clearSessionByPrefix(prefix,sessid)
 d clearSessionByPrefix^%zewdCompiler20($g(prefix),$g(sessid))
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
 ; HTML Form-specific APIs
 ;
getTextValue(fieldName,sessid)
 QUIT $$getSessionValue(fieldName,sessid)
 ;
setTextValue(fieldName,value,sessid)
 d setSessionValue(fieldName,value,sessid)
 QUIT
 ;
getPasswordValue(fieldName,sessid)
 QUIT $$getSessionValue(fieldName,sessid)
 ;
getHiddenValue(fieldName,sessid)
 QUIT $$getSessionValue(fieldName,sessid)
 ;
setHiddenValue(fieldName,value,sessid)
 d setSessionValue(fieldName,value,sessid)
 ;
getRadioValue(fieldName,sessid)
 QUIT $$getSessionValue(fieldName,sessid)
 ;
setRadioOn(fieldName,value,sessid)
 d setSessionValue(fieldName,value,sessid)
 QUIT
 ;
isRadionOn(fieldName,value,sessid)
 QUIT $$getRadioValue(fieldName,sessid)=value
 ;
isCheckboxOn(fieldName,value,sessid)
 QUIT $$isSelected(fieldName,value,sessid)
 ;
getCheckboxValues(fieldName,selectedValueArray,sessid)
 d mergeFromSelected(fieldName,.selectedValueArray,sessid)
 QUIT
 ;
initialiseCheckbox(fieldName,sessid)
 d clearSelected(fieldName,sessid)
 QUIT
 ;
setCheckboxOn(fieldName,value,sessid)
 d addToSelected(fieldName,value,sessid)
 QUIT
 ;
setCheckboxOff(fieldName,value,sessid)
 d removeFromSelected(fieldName,value,sessid)
 ;
setCheckboxValues(fieldName,selectedValueArray,sessid)
 ;
 ; array format : array(checkboxValue)=checkboxValue
 ; eg selected("red")="red"
 ;
 d mergeToSelected(fieldName,.selectedValueArray,sessid)
 QUIT
 ;
getSelectValue(fieldName,sessid,nullify)
 ;
 n value
 ;
 s value=$$getSessionValue(fieldName,sessid)
 i $a(value)=160 s value=""
 QUIT value
 ;
setSelectValue(fieldName,value,sessid)
 d setSessionValue(fieldName,value,sessid)
 ;
isSelectOn(fieldName,value,sessid)
 QUIT $$getSelectValue(fieldName,sessid)=value
 ;
isMultipleSelectOn(fieldName,value,sessid)
 QUIT $$isSelected(fieldName,value,sessid)
 ;
getMultipleSelectValues(fieldName,selectedValueArray,sessid)
 n value
 d mergeFromSelected(fieldName,.selectedValueArray,sessid)
 s value=$$getSessionValue^%zewdAPI(fieldName,sessid)
 i value'="" s selectedValueArray(value)=value
 QUIT
 ;
initialiseMultipleSelect(fieldName,sessid)
 d clearSelected(fieldName,sessid)
 QUIT
 ;
setMultipleSelectOn(fieldName,value,sessid)
 d addToSelected(fieldName,value,sessid)
 QUIT
 ;
setMultipleSelectOff(fieldName,value,sessid)
 d removeFromSelected(fieldName,value,sessid)
 ;
setMultipleSelectValues(fieldName,selectedValueArray,sessid)
 ;
 ; array format : array(checkboxValue)=checkboxValue
 ; eg selected("red")="red"
 ;
 d mergeToSelected(fieldName,.selectedValueArray,sessid)
 QUIT
 ;
getTextArea(fieldName,textArray,sessid)
 d mergeFromTextArea(fieldName,.textArray,sessid)
 QUIT
 ;
setFieldError(fieldName,sessid)
 ;
 n errors
 s errors(fieldName)=$$getSessionValue("ewd_errorClass",sessid)
 d mergeArrayToSession^%zewdAPI(.errors,"ewd_errorFields",sessid)
 d setSessionValue^%zewdAPI("ewd_hasErrors",1,sessid)
 QUIT
 ;
setErrorClasses()
 QUIT $$setErrorClasses^%zewdUtilities()
 ;
getRequestValue(fieldName,sessid)
 n value,zt
 i $g(fieldName)="" QUIT ""
 s value=$g(requestArray(fieldName))
 QUIT value
 ;
mergeGlobalFromRequest(globalName,fieldName,sessid)
 d mergeGlobalFromRequest^%zewdCompiler13($g(globalName),$g(fieldName),$g(sessid))
 QUIT
 ;
mergeFromRequest(array,fieldName,sessid)
 QUIT:fieldName=""
 m array=requestArray(fieldName)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
copyRequestValueToSession(fieldName,sessid)
 ;
 QUIT:$g(sessid)=""
 QUIT:$g(fieldName)=""
 i $$isTemp(fieldName) m zewdSession(fieldName)=requestArray(fieldName)
 m ^%zewdSession("session",sessid,fieldName)=requestArray(fieldName)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
getCookieValue(cookieName,sessid)
 QUIT:$g(cookieName)=""
 set $zt="getCookieValueErr"
 QUIT $g(requestArray(cookieName))
 ;
getCookieValueErr ; --- Come here if error occurred in 'getCookieValue' ---
 set $zt=""
 QUIT ""
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
deleteCookie(cookieName,sessid)
 d setCookieValue(cookieName,"",-3600,sessid)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
enableGetPage(page,sessid)
 d enableGetPage^%zewdCompiler24($g(page),$g(sessid))
 QUIT
 ;
disableGetPage(page,sessid)
 d disableGetPage^%zewdCompiler24($g(page),$g(sessid))
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
convertDaysToSeconds(days)
 QUIT days*86400
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
parseHTMLFile(filepath,docName)
 QUIT $$parseHTMLFile^%zewdCompiler16($g(filepath),$g(docName))
 ;
parseXMLFile(filepath,docName)
 QUIT $$parseXMLFile^%zewdCompiler16($g(filepath),$g(docName))
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
parseStream(streamName,docName,error,isHTML)
 d parseStream^%zewdCompiler16($g(streamName),$g(docName),.error,$g(isHTML))
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
parseHTMLStream(streamName,docName)
 QUIT $$parseHTMLStream^%zewdCompiler16($g(streamName),$g(docName))
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
parseURL(server,getPath,docName,port,isHTML,responseTime,browserType,post,maxLineLength,headers)
 ;
 QUIT $$parseURL^%zewdHTMLParser($g(server),$g(getPath),$g(docName),$g(port),$g(isHTML),.responseTime,$g(browserType),$g(post),$g(maxLineLength),.headers)
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
setCookieValue(cookieName,value,expiryDuration,sessid)
 ;
 ; expiryDuration is no of seconds
 ;
 n expires
 s expires=expiryDuration
 i $$isCSP(sessid) d
 . s expires=$$convertDateToSeconds($h)+expires
 . s expires=$$convertSecondsToDate(expires)
 . s expires=$$inetDate(expires)
 s value=value_$c(1)_expires
 d setSessionArray("ewd_cookie",cookieName,value,sessid)
 ;
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
setResponseHeader(headerName,headerValue,sessid)
 d setSessionArray^%zewdAPI("ewd_header",$g(headerName),$g(headerValue),$g(sessid))
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
suppressResponseHeader(headerName,sessid)
 i $$isCSP(sessid) d setResponseHeader(headerName,"",sessid)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
addServerToSession(sessid,serverArray)
 d addServerToSession^%zewdCompiler13($g(sessid),.serverArray)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
getServerValue(serverFieldName,sessid)
 ;
 n value,zt
 ;
 i $g(serverFieldName)="" QUIT ""
 ;
 QUIT $g(serverArray(serverFieldName))
 ;
getServerValueErr ; --- Come here if error occurred in 'getServerValue' ---
 set $zt=zt
 QUIT ""
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
deleteWarning(sessid)
 QUIT:$g(sessid)=""
 d deleteFromSession("ewd_warning",sessid)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
setWarning(warningMessage,sessid)
 QUIT:$g(sessid)=""
 QUIT:$g(warningMessage)=""
 s warningMessage=$$systemMessage(warningMessage,"warning",sessid)
 i '$$isCSP(sessid) s warningMessage=$$zcvt(warningMessage,"o","JS")
 d setSessionValue("ewd_warning",warningMessage,sessid)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
clearAllSelected(sessid)
 k ^%zewdSession("session",sessid,"ewd_selected")
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
clearSelected(fieldName,sessid)
 QUIT:$g(fieldName)=""
 QUIT:$g(sessid)=""
 s fieldName=$tr(fieldName,".","_")
 k ^%zewdSession("session",sessid,"ewd_selected",fieldName)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
addToSelected(fieldName,fieldValue,sessid)
 ;
 n shortFieldValue
 QUIT:$g(fieldName)=""
 QUIT:$g(sessid)=""
 QUIT:$g(fieldValue)=""
 s fieldName=$tr(fieldName,".","_")
 s shortFieldValue=$e(fieldValue,1,200)
 s ^%zewdSession("session",sessid,"ewd_selected",fieldName,shortFieldValue)=fieldValue
 QUIT
 ;
removeFromSelected(fieldName,fieldValue,sessid)
 ;
 n shortFieldValue
 QUIT:$g(fieldName)=""
 QUIT:$g(sessid)=""
 QUIT:$g(fieldValue)=""
 s fieldName=$tr(fieldName,".","_")
 s shortFieldValue=$e(fieldValue,1,200)
 k ^%zewdSession("session",sessid,"ewd_selected",fieldName,shortFieldValue)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
mergeFromSelected(fieldName,selected,sessid)
 ;
 k selected
 s fieldName=$tr(fieldName,".","_")
 m selected=^%zewdSession("session",sessid,"ewd_selected",fieldName)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
mergeToSelected(fieldName,selected,sessid)
 ;
 s fieldName=$tr(fieldName,".","_")
 ;
 k ^%zewdSession("session",sessid,"ewd_selected",fieldName)
 m ^%zewdSession("session",sessid,"ewd_selected",fieldName)=selected
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
isSelected(fieldName,fieldValue,sessid)
 n shortFieldValue
 i $g(fieldName)="" QUIT 0
 i $g(sessid)="" QUIT 0
 i $g(fieldValue)="" QUIT 0
 s fieldName=$tr(fieldName,".","_")
 set $zt="isSelectedErr"
 s shortFieldValue=$e(fieldValue,1,200)
 QUIT $d(^%zewdSession("session",sessid,"ewd_selected",fieldName,shortFieldValue))
 ;
isSelectedErr ; --- Come here if error occurred in 'isSelected' ---
 set $zt=""
 QUIT 0
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
clearTextArea(fieldName,sessid)
 QUIT:$g(fieldName)=""
 QUIT:$g(sessid)=""
 s fieldName=$tr(fieldName,".","_")
 k ^%zewdSession("session",sessid,"ewd_textarea",fieldName)
 s ^%zewdSession("session",sessid,"ewd_textarea",fieldName,1)=""
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
createTextArea(fieldName,textArray,sessid)
 ;
 QUIT:$g(fieldName)=""
 QUIT:$g(sessid)=""
 s fieldName=$tr(fieldName,".","_")
 m ^%zewdSession("session",sessid,"ewd_textarea",fieldName)=textArray
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
mergeTextAreaFromRequest(fieldName,requestArray,sessid)
 ;
 q:$g(sessid)=""
 s fieldName=$tr(fieldName,".","_")
 ;
 q:'$d(^%zewdSession("session",sessid,"ewd_textarea",fieldName))
 d clearTextArea(fieldName,sessid)
 m ^%zewdSession("session",sessid,"ewd_textarea",fieldName)=requestArray(fieldName)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
appendToTextArea(fieldName,lineOfText,sessid)
 ;
 n position
 ;
 QUIT:$g(fieldName)=""
 QUIT:$g(sessid)=""
 s fieldName=$tr(fieldName,".","_")
 ;
 s position=$o(^%zewdSession("session",sessid,"ewd_textarea",fieldName,""),-1)+1
 s ^%zewdSession("session",sessid,"ewd_textarea",fieldName,position)=lineOfText 
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
mergeFromTextArea(fieldName,textArray,sessid)
 ;
 s fieldName=$tr(fieldName,".","_")
 m textArray=^%zewdSession("session",sessid,"ewd_textarea",fieldName)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
mergeToTextArea(fieldName,textArray,sessid)
 ;
 s fieldName=$tr(fieldName,".","_")
 m ^%zewdSession("session",sessid,"ewd_textarea",fieldName)=textArray
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
clearList(listName,sessid)
 QUIT:$g(listName)=""
 QUIT:$g(sessid)=""
 s listName=$tr(listName,".","_")
 k ^%zewdSession("session",sessid,"ewd_list",listName)
 k ^%zewdSession("session",sessid,"ewd_listIndex",listName)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
isListDefined(listName,sessid)
 QUIT $d(^%zewdSession("session",sessid,"ewd_list",listName))
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
countList(listName,sessid)
 QUIT $$countList^%zewdCompiler16($g(listName),$g(sessid))
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
appendToList(listName,textValue,codeValue,sessid,otherAttrs)
 ;
 n position
 ;
 QUIT:$g(listName)=""
 QUIT:$g(sessid)=""
 ;QUIT:$g(textValue)=""
 ;QUIT:$g(codeValue)=""
 s listName=$tr(listName,".","_")
 ;
 s position=$o(^%zewdSession("session",sessid,"ewd_list",listName,""),-1)+1
 d addToList(listName,textValue,codeValue,position,sessid,.otherAttrs)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
addToList(listName,textValue,codeValue,position,sessid,otherAttrs)
 ;d addToList^%zewdCompiler16($g(listName),$g(textValue),$g(codeValue),$g(position),$g(sessid),.otherAttrs)
 ;
 n attrList,attrName
 ;
 QUIT:$g(listName)=""
 QUIT:$g(sessid)=""
 QUIT:$g(position)=""
 i $g(codeValue)="",$g(textValue)="" QUIT
 s position=+position
 d removeFromList(listName,codeValue,sessid) ; just in case
 s attrName="",attrList=""
 f  s attrName=$o(otherAttrs(attrName)) q:attrName=""  d
 . s attrList=attrList_attrName_$c(3)_otherAttrs(attrName)_$c(1)
 ;
 s codeValue=$g(codeValue) i codeValue="" s codeValue=textValue
 s ^%zewdSession("session",sessid,"ewd_list",listName,position)=textValue_$c(1)_codeValue_$c(1)_attrList
 s ^%zewdSession("session",sessid,"ewd_listIndex",listName,codeValue)=position
 k otherAttrs
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
mergeToList(listName,listArray,sessid)
 ;
 d mergeToList^%zewdCompiler7(listName,.listArray,sessid)
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
removeFromList(listName,codeValue,sessid)
 d removeFromList^%zewdCompiler20($g(listName),$g(codeValue),$g(sessid))
 QUIT
 ;
copyList(fromListName,toListName,sessid)
 d copyList^%zewdCompiler7($g(fromListName),$g(toListName),$g(sessid))
 QUIT
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
getTextFromList(listName,codeValue,sessid)
 ;
 QUIT $$getTextFromList^%zewdCompiler7(listName,codeValue,sessid)
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
replaceOptionsByFieldName(formName,fieldName,listName,sessid)
 ;
 QUIT $$replaceOptionsByFieldName^%zewdCompiler7(formName,fieldName,listName,sessid)
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
replaceOptionsByID(fieldID,listName,sessid)
 ;
 QUIT $$replaceOptionsByID^%zewdCompiler7(fieldID,listName,sessid)
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
getUploadedFileName(fieldName,sessid)
 QUIT $$getUploadedFileName^%zewdCompiler20($g(fieldName),$g(sessid))
 ;
getUploadedFileSize(fieldName,sessid)
 QUIT $$getUploadedFileSize^%zewdCompiler20($g(fieldName),$g(sessid))
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
getUploadedFileType(fieldName,sessid)
 ;
 set $zt="getUploadedFileTypeErr"
 QUIT 0
 ;
getUploadedFileTypeErr
 set $zt=""
 QUIT ""
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
errorOccurred(sessid)
 ;
 n warning
 ;
 i $g(Error)="" QUIT 0
 s warning=$$getSessionValue("ewd_warning",sessid)
 QUIT Error'=warning
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
removeQuotes(string)
 ;
 n quoted,c1,quote
 s quote=""
 s c1=$e(string,1)
 s quoted=0
 i c1=""""!(c1="'") s quoted=1,quote=c1
 i 'quoted QUIT string
 i $e(string,$l(string))'=quote QUIT string
 QUIT $e(string,2,$l(string)-1)
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
escapeQuotes(text)
 ;
 s text=$$replaceAll(text,"'",$c(4))
 s text=$$replaceAll(text,$c(4),"\'")
 s text=$$replaceAll(text,"""",$c(4))
 s text=$$replaceAll(text,$c(4),"\""")
 ;
 QUIT text
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
getAttrValue(attrName,attrValues,technology)
 QUIT $$getAttrValue^%zewdCompiler4(attrName,.attrValues,technology)
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
replaceAll(InText,FromStr,ToStr) ; Replace all occurrences of a substring
 ;
 n %p
 ;
 s %p=InText
 i ToStr[FromStr d  QUIT %p
 . n i,stop,tempText,tempTo
 . s stop=0
 . f i=0:1:255 d  q:stop
 . . q:InText[$c(i)
 . . q:FromStr[$c(i)
 . . q:ToStr[$c(i)
 . . s stop=1
 . s tempTo=$c(i)
 . s tempText=$$replaceAll(InText,FromStr,tempTo)
 . s %p=$$replaceAll(tempText,tempTo,ToStr)
 f  q:%p'[FromStr  S %p=$$replace(%p,FromStr,ToStr)
 QUIT %p
 ;
replace(InText,FromStr,ToStr) ; replace old with new in string
 ;
 n %np,%p1,%p2
 ;
 i InText'[FromStr q InText
 s %np=$l(InText,FromStr)+1
 s %p1=$p(InText,FromStr,1),%p2=$p(InText,FromStr,2,%np)
 QUIT %p1_ToStr_%p2
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
addImmediateOneOffTask(executeCode,startTime,namespace,rc,rm)
 QUIT $$addImmediateOneOffTask^%zewdScheduler($g(executeCode),$g(startTime),$g(namespace),.rc,.rm)
 ;
 ;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 ;
getDataTypeErrors(errorArray,sessid)
 k errorArray
 d mergeArrayFromSession(.errorArray,"ewd_DataTypeError",sessid)
 QUIT
 ;
clearSchemaFormErrors(sessid)
 d deleteFromSession("ewd_SchemaFormError",sessid)
 QUIT
 ;
getSchemaFormErrors(errorArray,sessid)
 QUIT $$getSchemaFormErrors^%zewdCompiler13(.errorArray,$g(sessid))
 ;
setSchemaFormErrors(errorArray,sessid)
 ;
 n sessionName
 ;
 s sessionName="ewd_SchemaFormError"
 d deleteFromSession(sessionName,sessid)
 d mergeArrayToSession(.errorArray,sessionName,sessid)
 QUIT
 ;
removeInstanceDocument(instanceName)
 ;
 n ok
 s ok=$$openDOM
 i ok'="" QUIT ok 
 s ok=$$removeDocument^%zewdDOM(instanceName,"","")
 d clearXMLIndex^%zewdSchemaForm(instanceName)
 s ok=$$closeDOM^%zewdDOM()
 QUIT ""
 ;
 ;
makeTokenString(length)
 ;
 n string,token,i
 ;
 s string="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890"
 s token=""
 f i=1:1:length s token=token_$e(string,($r($l(string))+1))
 QUIT token
 ;
makeString(%char,%len) ; create a string of len characters
 ;
 n %str
 ;
 s %str="",$p(%str,%char,%len+1)=""
 QUIT %str
 ;
convertDateToSeconds(hdate)
 ;
 Q (hdate*86400)+$p(hdate,",",2)
 ;
convertSecondsToDate(secs)
 ;
 QUIT (secs\86400)_","_(secs#86400)
 ;
getTokenExpiry(token)
 ;
 n sessid
 ;
 i $g(token)="" QUIT 0
 s sessid=$p($g(^%zewdSession("tokens",token)),"~",1)
 i sessid="" QUIT 0
 QUIT $$getSessionValue("ewd_sessionExpiry",sessid)
 ;
isTokenExpired(token)
 ;
 ;QUIT $$getTokenExpiry(token)'>$$convertDateToSeconds($h)
 QUIT $$getTokenExpiry(token)'>(($h*86400)+$p($h,",",2))
 ;
randChar()
 ;
 n string
 ;
 s string="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890"
 QUIT $e(string,($R($l(string))+1))
 ;
lowerCase(string)
 QUIT $tr(string,"ABCDEFGHIJKLMNOPQRSTUVQXYZ","abcdefghijklmnopqrstuvwxyz")
 ;
stripSpaces(string)
 s string=$$stripLeadingSpaces(string)
 QUIT $$stripTrailingSpaces(string)
 ;
stripLeadingSpaces(string)
 n i
 ;
 f i=1:1:$l(string) QUIT:$e(string,i)'=" "
 QUIT $e(string,i,$l(string))
 ;
stripTrailingSpaces(string)
 n i,spaces,new
 ;
 s spaces=$$makeString(" ",100)
 s new=string_spaces
 QUIT $p(new,spaces,1)
 ;
parseMethod(methodString,class,method)
 ;
 n %p1,%p2,meth
 ;
 s %p1=$p(methodString,"##class(",2)
 s class=$p(%p1,")",1)
 s %p2=$p(%p1,")",2,500)
 s method=$p(%p2,".",2)
 s method=$p(method,"(",1)
 QUIT
 ;
event(requestArray)
 QUIT $$event^%zewdPHP(.requestArray)
 ;
clearURLNVP(urlNo)
 ;
 QUIT
 ;
setURLNVP(urlNo,name)
 ;
 QUIT
 ;
decodeDataType(name,dataType,sessid)
 d decodeDataType^%zewdCompiler20($g(name),$g(dataType),$g(sessid))
 QUIT
 ;
encodeDataType(name,dataType,sessid)
 QUIT $$encodeDataType^%zewdCompiler13($g(name),$g(dataType),$g(sessid))
 ;
copyURLNVPsToSession(urlNo)
 ;
 n name
 ;
 QUIT
 ;
doubleQuotes(string)
 ;
 s string=$$replaceAll(string,"""",$c(1,1))
 s string=$tr(string,$c(1),"""")
 QUIT string
 ;
 ;  ==========================================================================
 ;     Error Trap Functions
 ;  ==========================================================================
 ;
copySessionToSymbolTable(sessid)
 d copySessionToSymbolTable^%zewdCompiler16($g(sessid))
 QUIT
 ;
saveSymbolTable(sessid)
 ;
 n ok
 ;s sessid=0
 k ^%zewdError(sessid)
 n %zzv
 k ^%zewdError(sessid)
 s %zzv="%"
 f  s %zzv=$o(@%zzv) Q:%zzv=""  m ^%zewdError(sessid,%zzv)=@%zzv
 QUIT
 ;
loadErrorSymbols(sessid)
 d loadErrorSymbols^%zewdCompiler19($g(sessid))
 QUIT
 ;
deleteErrorLog(sessid)
 k ^%zewdError(sessid)
 QUIT
 ;
deleteAllErrorLogs
 k ^%zewdError
 QUIT
 ;
fileSize(path)
 QUIT $$fileSize^%zewdCompiler13($g(path))
 ;
fileExists(path)
 QUIT $$fileExists^%zewdCompiler13($g(path))
 ;
fileInfo(path,info)
 d fileInfo^%zewdCompiler13($g(path),.info)
 QUIT
 ;
directoryExists(path)
 QUIT $$directoryExists^%zewdCompiler13($g(path))
 ;
deleteFile(filepath)
 QUIT $$deleteFile^%zewdCompiler13($g(filepath))
 ;
renameFile(filepath,newpath)
 QUIT $$renameFile^%zewdCompiler13($g(filepath),$g(newpath))
 ;
createDirectory(path)
 QUIT $$createDirectory^%zewdCompiler13($g(path))
 ;
removeCR(string)
 i $e(string,$l(string))=$c(13) s string=$e(string,1,$l(string)-1)
 QUIT string
 ;
setApplicationRootPath(path)
 d setApplicationRootPath^%zewdCompiler(path)
 QUIT
 ;
applicationRootPath()
 QUIT $$applicationRootPath^%zewdCompiler()
 ;
getApplicationRootPath()
 QUIT $$getApplicationRootPath^%zewdCompiler()
 ;
setOutputRootPath(path,technology)
 d setOutputRootPath^%zewdCompiler(path,technology)
 QUIT
 ;
getRootURL(technology)
 QUIT $$getRootURL^%zewdCompiler(technology)
 ;
setRootURL(cspURL,technology)
 d setRootURL^%zewdCompiler(cspURL,technology)
 QUIT
 ;
getDefaultTechnology()
 QUIT $$getDefaultTechnology^%zewdCompiler()
 ;
getDefaultMultiLingual()
 QUIT $$getDefaultMultiLingual^%zewdCompiler()
 ;
getOutputRootPath(technology)
 QUIT $$getOutputRootPath^%zewdCompiler(technology)
 ;
getJSScriptsPath(app,technology)
 QUIT $$getJSScriptsPath^%zewdCompiler8(app,technology)
 ;
getJSScriptsPathMode(technology)
 QUIT $$getJSScriptsPathMode^%zewdCompiler8(technology)
	;
setJSScriptsPathMode(technology,mode)
 d setJSScriptsPathMode^%zewdCompiler8(technology,mode)
 QUIT
	;
getJSScriptsRootPath(technology)
 QUIT $$getJSScriptsRootPath^%zewdCompiler8(technology)
	;
setJSScriptsRootPath(technology,path)
 d setJSScriptsRootPath^%zewdCompiler8(technology,path)
 QUIT
 ;
getHomePage()
 QUIT $$getHomePage^%zewdCompiler()
 ;
setHomePage(homePage)
 d setHomePage^%zewdCompiler($g(homePage))
 QUIT
 ;
getApplications(appList)
 QUIT $$getApplications^%zewdCompiler16(.appList)
 ;
getPages(application,pageList)
 QUIT $$getPages^%zewdCompiler16($g(application),.pageList)
 ;
getDefaultFormat()
 QUIT $$getDefaultFormat^%zewdCompiler()
 ;
getNextChild(parentOID,childOID)
 i $g(parentOID)="" QUIT ""
 i childOID="" QUIT $$getFirstChild^%zewdDOM(parentOID)
 QUIT $$getNextSibling^%zewdDOM(childOID)
 ;
addCSPServerScript(parentOID,text,atTop)
 QUIT $$addCSPServerScript^%zewdCompiler4(parentOID,text,$g(atTop))
 ;
createPHPCommand(data,docOID)
 QUIT $$createPHPCommand^%zewdCompiler4(data,docOID)
 ;
createJSPCommand(data,docOID)
 QUIT $$createJSPCommand^%zewdCompiler4(data,docOID)
 ;
instantiateJSPVar(var,type,docOID,arraySize,initialValue)
 d instantiateJSPVar^%zewdCompiler4(var,type,docOID,arraySize,initialValue)
 QUIT
 ;
removeIntermediateNode(inOID)
 d removeIntermediateNode^%zewdCompiler4(inOID)
 QUIT
 ;
getNormalisedAttributeValue(attrName,nodeOID,technology)
 QUIT $$getNormalAttributeValue^%zewdCompiler($g(attrName),$g(nodeOID),$g(technology))
 ;
getNormalAttributeValue(attrName,nodeOID,technology)
 QUIT $$getNormalAttributeValue^%zewdCompiler($g(attrName),$g(nodeOID),$g(technology))
 ;
getTagOID(tagName,docName,lowerCase)
 QUIT $$getTagOID^%zewdCompiler($g(tagName),$g(docName),$g(lowerCase))
 ;
getTagByNameAndAttr(tagName,attrName,attrValue,matchCase,docName)
 QUIT $$getTagByNameAndAttr^%zewdCompiler3($g(tagName),$g(attrName),$g(attrValue),$g(matchCase),$g(docName))
 ;
javascriptFunctionExists(functionName,docName)
 QUIT $$javascriptFunctionExists^%zewdCompiler7($g(functionName),$g(docName))
 ;
addJavascriptFunction(docName,jsTextArray)
 QUIT $$addJavascriptFunction^%zewdCompiler7($g(docName),.jsTextArray)
 ;
getJavascriptFunctionBody(functionName,docName)
 QUIT $$getJavascriptFunctionBody^%zewdCompiler7($g(functionName),docName)
 ;
replaceJavascriptFunctionBody(functionName,jsText,docName)
 QUIT $$replaceJavascriptFunctionBody^%zewdCompiler7($g(functionName),$g(jsText),$g(docName))
 ;
getDelim()
 QUIT $$getDelim^%zewdCompiler()
 ;
setJSONPage(sessid)
 QUIT $$setJSONPage^%zewdCompiler20($g(sessid))
 ; ===========================================================================
 ;    WLD conversion utilities
 ; ===========================================================================
 ;
configureWebLink(webserver,mode,alias,path)
 QUIT $$configure^%zewdWLD($g(webserver),$g(mode),$g(alias),$g(path))
 ;
mergeListToSession(fieldName,sessid)
 d mergeListToSession^%zewdCompiler16($g(fieldName),$g(sessid))
 QUIT
 ;
getPREVPAGE(sessid) ;
 QUIT $$getPREVPAGE^%zewdCompiler19($g(sessid)) ;
 ;
copyToWLDSymbolTable(sessid)
 d copyToWLDSymbolTable^%zewdCompiler16($g(sessid))
 ;
getPRESSED(sessid)
 QUIT $$getSessionValue("ewd_pressed",sessid)
 ;
copyToLIST(listName,sessid)
 ;
 k LIST(listName)
 m LIST(listName)=^%zewdSession("session",sessid,"ewd_list",listName)
 QUIT
 ;
copyToSELECTED(fieldName,sessid)
 ;
 k SELECTED(fieldName)
 m SELECTED(fieldName)=^%zewdSession("session",sessid,"ewd_selected",fieldName)
 QUIT
 ;
traceModeOn
 s ^zewd("trace")=1
 QUIT
 ;
traceModeOff
 k ^zewd("trace")
 QUIT
 ;
getTraceMode()
 i $g(^zewd("trace"))=1 QUIT 1
 QUIT 0
 ;
trace(text,clear) ; trace  ;
 n i
 s text=$g(text)
 i $g(clear)=1 k ^%zewdTrace
 s i=$increment(^%zewdTrace)
 s ^%zewdTrace(i)=text
 QUIT
 ;
inetDate(hdate) ; Decode $H date and time to Internet format
 ;
 N %d,%day,%time,%date
 ;
 S %time=$P(hdate,",",2)
 I %time>86400 D
 .S %time=%time-86400
 .S hdate=(hdate+1)_","_%time
 ;
 S %d="Thu,Fri,Sat,Sun,Mon,Tue,Wed"
 S %day=(hdate#7)+1
 S %day=$P(%d,",",%day)
 ;
 S %date=$$decDate(hdate)
 ;S %date=$TR(%date," ","-")
 S %time=$$inetTime(hdate)
 S %date=%day_", "_%date_" "_%time
 Q %date
decDate(hdate) ; Decode a date from $H format
 ;
 n %yy,%mm,%dd,%d1,%d
 i $zv'["GT.M" d
 . s %d1=$zd(hdate,5)
 . s %yy=$p(%d1,", ",2)
 . s %dd=+$p(%d1," ",2) I %dd<10 S %dd="0"_%dd
 . s %mm=$p(%d1," ",1)
 e  d
 . n p1,p2
 . s %d1=$zd(hdate,2) 
 . s %dd=$p(%d1,"-",1)
 . s %mm=$p(%d1,"-",2)
 . s p1=$e(%mm,1),p2=$e(%mm,2,$l(%mm))
 . s %mm=p1_$$lowerCase(p2)
 . s %yy=$p(%d1,"-",3)
 . i hdate>58073 s %yy="20"_%yy
 s %d=%dd_" "_%mm_" "_%yy
 QUIT %d
 ;
inetTime(hdate) ; Decode Internet Format Time from $H format
 ; Offset is relative to GMT, eg -0500
 ;
 n hh,mm,ss,time
 s time=$p(hdate,",",2)
 s hh=time\3600 i hh<10 s hh="0"_hh
 s time=time#3600
 s mm=time\60 i mm<10 s mm="0"_mm
 s ss=time#60 i ss<10 s ss="0"_ss
 QUIT hh_":"_mm_":"_ss
 ;
openNewFile(filepath)
 QUIT $$openNewFile^%zewdCompiler($g(filepath))
 ;
openFile(filepath)
 QUIT $$openFile^%zewdCompiler($g(filepath))
 ;
openDOM()
 ;
 n i,ok
 ;
 f i=1:1:20 s ok=$$openDOM^%zewdDOM(0,,,,,,,,,,,,,,,,,) q:$$zcvt(ok,"l")["licensing violation"  q:ok=""  h 1
 i ok'="" s ok="No eXtc Licenses available!"
 QUIT ok
 ;
removeChild(nodeOID,removeFromDOM)
 ;
 n ver
 ;
 s ver="" 
 QUIT $$removeChild^%zewdDOM(nodeOID,$g(removeFromDOM))
 ;
removeAttribute(attrName,nodeOID,removeFromDOM)
 ;
 n ver
 ;
 s ver="" 
 d removeAttribute^%zewdDOM(attrName,nodeOID,$g(removeFromDOM)) QUIT
 ;
removeAttributeNS(ns,attrName,nodeOID,removeFromDOM)
 ;
 n ver
 ;
 s ver="" 
 d removeAttributeNS^%zewdDOM(ns,attrName,nodeOID,$g(removeFromDOM)) QUIT
 ;
removeIntermediateNodeeXtc(nodeOID,removeFromDOM)
 ;
 n ver
 ;
 d removeIntermediateNode^%zewdDOM(nodeOID,$g(removeFromDOM))
 QUIT
 ;
export(fileName,prefix,extension)
 d export^%zewdCompiler16($g(fileName),$g(prefix),$g(extension))
 QUIT
 ;
import(fileName)
 ;
 i $g(fileName)="" s fileName="zewd.xml"
 QUIT
 ;
listDOMsByPrefix(prefix)
	d listDOMsByPrefix^%zewdCompiler19($g(prefix))
	QUIT
 ;
removeDOMsByPrefix(prefix)
	d removeDOMsByPrefix^%zewdCompiler19($g(prefix))
	QUIT
	;
dumpDOM(docName)
 ;
 d dumpDOM^%zewdCompiler20($g(docName))
 QUIT
 ;
namespace()
 QUIT $zdir
 ;
setNamespace(namespace)
 s $zdir=namespace
 QUIT
 ;
zcvt(string,param,param2)
 ;
 i $g(param)="" s param="l"
 i param="l"!(param="L") QUIT $tr(string,"ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz")
 i param="u"!(param="U") QUIT $tr(string,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")
 i param="O",param2="HTML" d  QUIT string
 . s string=$$replaceAll^%zewdAPI(string,"&",$c(1))
 . s string=$$replaceAll^%zewdAPI(string,"""","&quot;")
 . s string=$$replaceAll^%zewdAPI(string,"'","&#39;")
 . s string=$$replaceAll^%zewdAPI(string,"<","&lt;")
 . s string=$$replaceAll^%zewdAPI(string,">","&gt;")
 . s string=$$replaceAll^%zewdAPI(string,$c(1),"&amp;")
 i param="I",param2="HTML" d  QUIT string
 . n c,no,no2,p1,p2,p3,stop
 . s string=$$replaceAll^%zewdAPI(string,"&quot;","""")
 . s string=$$replaceAll^%zewdAPI(string,"&#39;","'")
 . s string=$$replaceAll^%zewdAPI(string,"&amp;","&")
 . s string=$$replaceAll^%zewdAPI(string,"&lt;","<")
 . s string=$$replaceAll^%zewdAPI(string,"&gt;",">")
 . s stop=0
 . f  d  q:stop
 . . s no=$l(string,"&#")
 . . i string'["&#" s stop=1 q
 . . s p1=$p(string,"&#",1)
 . . s p2=$p(string,"&#",2,no+1)
 . . s no2=$l(p2,";")
 . . s p3=$p(p2,";",1)
 . . i $l(p3)'=2 s stop=1 q
 . . s p2=$p(p2,";",2,no2+1)
 . . s c=$c(p3)
 . . s string=p1_c_p2
 QUIT string
 ;
getIP() ; Get own IP address
 QUIT $$getIP^%zewdAPI2()
 ;
ajaxErrorRedirect(sessid)
 QUIT $$ajaxErrorRedirect^%zewdAPI2($g(sessid))
 ;
classExport(className,methods,filepath)
 ;
 QUIT $$classExport^%zewdCompiler16($g(className),.methods,$g(filepath))
 ;
strx(string)
 d strx^%zewdAPI2($g(string))
 QUIT
 ;
disableEwdMgr
 s ^%zewd("disabled")=1
 QUIT
 ;
enableEwdMgr
 k ^%zewd("disabled")
 QUIT
 ;
enableWLDAccess(app,page)
 i $g(^zewd("allowWLDAccess",$$zcvt(app,"l"),$$zcvt(page,"l")))'=1 s ^zewd("allowWLDAccess",$$zcvt(app,"l"),$$zcvt(page,"l"))=1
 QUIT
 ;
disableWLDAccess(app,page)
 k ^zewd("allowWLDAccess",$$zcvt(app,"l"),$$zcvt(page,"l"))
 QUIT
isSSOValid(sso,username,password,sessid)
 QUIT $$isSSOValid^%zewdMgrAjax2($g(sso),$g(username),$g(password),$g(sessid))
 ;
uniqueId(nodeOID,filename)
 QUIT $p(filename,".ewd",1)_$p(nodeOID,"-",2)
 ;
linkToParentSession(sessid)
 QUIT $$linkToParentSession^%zewdCompiler20($g(sessid))
 ;
exportToGTM(routine)
