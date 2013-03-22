%zewdDemo ; Tutorial page functions
 ;
 ; Product: Enterprise Web Developer (Build 910)
 ; Build Date: Wed, 25 Apr 2012 17:59:25
 ;
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
getLanguage(sessid)
 ;
 s language=$$getRequestValue^%zewdAPI("ewd_Language",sessid)
 d setSessionValue^%zewdAPI("ewd_Language",language,sessid)
 d setSessionValue^%zewdAPI("url","www.mgateway.com",sessid)
 d setSessionValue^%zewdAPI("imageTest",2,sessid)
 d setSessionValue^%zewdAPI("tmp_testing",1234567,sessid)
 QUIT ""
 ;
login(sessid)
 ;
 n username,password
 ;
 s username=$$getTextValue^%zewdAPI("username",sessid)
 s password=$$getPasswordValue^%zewdAPI("password",sessid)
 ;d trace^%zewdAPI("in login - username="_username_" ; password="_password)
 i username'="ROB" QUIT "invalid username"
 i password'="ROB" QUIT "invalid password"
 QUIT ""
 ;
logine(sessid)
 ;
 n error,username,password,message,textid
 ;
 s error=""
 s message=$$errorMessage^%zewdAPI("invalid login attempt",sessid)
 s username=$$getTextValue^%zewdAPI("username",sessid)
 s password=$$getPasswordValue^%zewdAPI("password",sessid)
 ;
 i '$d(^ewdDemo("tutorial","authentication")) d  QUIT $$errorMessage^%zewdAPI(error,sessid)
 . i username'="ROB" s error=message q
 . i password'="ROB" s error=message q
 ;
 i username="" QUIT message
 i '$d(^ewdDemo("tutorial","authentication",username)) QUIT message
 i password'=$p(^ewdDemo("tutorial","authentication",username),"~",1) QUIT message
 QUIT ""
 ;
getUsernames(sessid) ;
 ;
 n user
 ;
 i '$d(^ewdDemo("tutorial","authentication")) d  QUIT ""
 . d clearList^%zewdAPI("user",sessid)
 . d appendToList^%zewdAPI("user","Select a user..","nul",sessid)
 . d appendToList^%zewdAPI("user","ROB","ROB",sessid)
 ;
 s user=""
 d clearList^%zewdAPI("user",sessid)
 d appendToList^%zewdAPI("user","Select a user..","",sessid)
 f  s user=$o(^ewdDemo("tutorial","authentication",user)) q:user=""  d
 . d appendToList^%zewdAPI("user",user,user,sessid)
 ;
 QUIT ""
 ;
addUsername(sessid)
 ;
 n newUsername
 ;
 s newUsername=$$getTextValue^%zewdAPI("newUsername",sessid)
 i newUsername="" QUIT "You must enter a username"
 i $d(^ewdDemo("tutorial","authentication",newUsername)) QUIT newUsername_" already exists"
 d setTextValue^%zewdAPI("user",newUsername,sessid)
 QUIT ""
 ;
testValue(sessid)
 ;
 n user,pass
 ;
 s user=$$getSelectValue^%zewdAPI("user",sessid)
 ;d trace^%zewdAPI("user="_user)
 QUIT ""
 ;
getPassword(sessid)
 ;
 n user,pass
 ;
 s user=$$getSelectValue^%zewdAPI("user",sessid)
 s pass=$g(^ewdDemo("tutorial","authentication",user))
 s pass=$p(pass,"~",1)
 i user="ROB",pass="" s pass="ROB"
 d setTextValue^%zewdAPI("pass",pass,sessid)
 QUIT ""
 ;
setPassword(sessid)
 ;
 n user,pass
 ;
 s user=$$getSelectValue^%zewdAPI("user",sessid)
 s pass=$$getTextValue^%zewdAPI("pass",sessid)
 i pass="" QUIT "You must enter a password"
 s ^ewdDemo("tutorial","authentication",user)=pass
 QUIT ""
 ;
getObjDetails(sessid)
 i '$$sessionNameExists^%zewdAPI("person.username",sessid) d
 . d setSessionValue^%zewdAPI("person.username","Rob",sessid)
 . d setSessionValue^%zewdAPI("person.password","secret!",sessid)
 . d setSessionValue^%zewdAPI("person.userType","g",sessid)
 . d setCheckboxOn^%zewdAPI("person.permissions","w",sessid)
 . d setCheckboxOn^%zewdAPI("person.permissions","e",sessid)
 . d clearList^%zewdAPI("person.language",sessid)
 . d appendToList^%zewdAPI("person.language","English","en",sessid)
 . d appendToList^%zewdAPI("person.language","French","fr",sessid)
 . d appendToList^%zewdAPI("person.language","German","d",sessid)
 . d appendToList^%zewdAPI("person.language","Italian","it",sessid)
 . d setMultipleSelectOn^%zewdAPI("person.language","en",sessid)
 . d setMultipleSelectOn^%zewdAPI("person.language","d",sessid)
 . d clearTextArea^%zewdAPI("person.comments",sessid)
 . s textarea(1)="This is a line of text"
 . s textarea(2)="This is the second line"
 . d createTextArea^%zewdAPI("person.comments",.textarea,sessid)
 . d setSessionValue^%zewdAPI("wld.%KEY.MGWLPN","EXTC",sessid)
 QUIT ""
 ;
setObjDetails(sessid)
 QUIT ""
 ;
getDetails(sessid)
 ;
 n user,pass,data,expireDate,userType,selected,textarea,confirmText
 ;
 ;d trace^%zewdAPI("got here!!")
 s browser=$$getServerValue^%zewdAPI("HTTP_USER_AGENT",sessid)
 d setSessionValue^%zewdAPI("browser",browser,sessid)
 s user=$$getTextValue^%zewdAPI("user",sessid)
 s data=""
 i user'="" s data=$g(^ewdDemo("tutorial","authentication",user))
 ;d trace^%zewdAPI("user="_user_" ; data="_data)
 s pass=$p(data,"~",1)
 i user="ROB",pass="" d  QUIT ""
 . d setTextValue^%zewdAPI("pass","ROB",sessid)
 . d setRadioOn^%zewdAPI("userType","a",sessid)
 . d initialiseCheckbox^%zewdAPI("permissions",sessid)
 . d setCheckboxOn^%zewdAPI("permissions","w",sessid)
 . d setCheckboxOn^%zewdAPI("permissions","e",sessid)
 . d setCheckboxOn^%zewdAPI("permissions","s",sessid)
 . d createLanguageList(sessid)
 . d setMultipleSelectOn^%zewdAPI("language","en",sessid)
 . d setMultipleSelectOn^%zewdAPI("language","d",sessid)
 . d clearTextArea^%zewdAPI("comments",sessid)
 . s textarea(1)="This is a line of text"
 . s textarea(2)="This is the second line"
 . d createTextArea^%zewdAPI("comments",.textarea,sessid)
 ;
 d setTextValue^%zewdAPI("pass",pass,sessid)
 ;d trace^%zewdAPI("data="_data)
 s userType=$p(data,"~",2)
 i userType="" s userType="g"
 d setRadioOn^%zewdAPI("userType",userType,sessid)
 d initialiseCheckbox^%zewdAPI("permissions",sessid)
 i user'="" m selected=^ewdDemo("tutorial","authentication",user,"permissions")
 d setCheckboxValues^%zewdAPI("permissions",.selected,sessid)
 d createLanguageList(sessid)
 k selected
 i user'="" m selected=^ewdDemo("tutorial","authentication",user,"language")
 d setMultipleSelectValues^%zewdAPI("language",.selected,sessid)
 d clearTextArea^%zewdAPI("comments",sessid)
 i user'="" m textarea=^ewdDemo("tutorial","authentication",user,"comments")
 d createTextArea^%zewdAPI("comments",.textarea,sessid)
 ;
 QUIT ""
 ;
createLanguageList(sessid)
 ;
 n attr
 d clearList^%zewdAPI("language",sessid)
 d appendToList^%zewdAPI("language","English","en",sessid)
 s attr("style")="color:red"
 d appendToList^%zewdAPI("language","French","fr",sessid,.attr)
 d appendToList^%zewdAPI("language","German","d",sessid,.attr)
 s attr("style")="color:green"
 d appendToList^%zewdAPI("language","Italian","it",sessid,.attr)
 s attr("style")="color:green"
 d appendToList^%zewdAPI("language","Spanish","esp",sessid,.attr)
 d appendToList^%zewdAPI("language","Portuguese","por",sessid)
 d appendToList^%zewdAPI("language","Danish","den",sessid)
 d appendToList^%zewdAPI("language","Swedish","swe",sessid)
 d appendToList^%zewdAPI("language","Norwegian","nor",sessid)
 d initialiseMultipleSelect^%zewdAPI("language",sessid)
 QUIT
 ;
setDetails(sessid)
 ;
 n error,expireDate,user,pass,userType,selected,comments,warning
 ;
 s user=$$getTextValue^%zewdAPI("user",sessid)
 s pass=$$getTextValue^%zewdAPI("pass",sessid)
 i pass="" d  QUIT "You must enter a password"
 . d setFieldError^%zewdAPI("pass",sessid)
 i pass="xxx" d setFieldError^%zewdAPI("testField",sessid) QUIT "test error"
 s userType=$$getRadioValue^%zewdAPI("userType",sessid)
 s ^ewdDemo("tutorial","authentication",user)=pass_"~"_userType
 k ^ewdDemo("tutorial","authentication",user,"permissions")
 d getCheckboxValues^%zewdAPI("permissions",.selected,sessid)
 m ^ewdDemo("tutorial","authentication",user,"permissions")=selected
 k ^ewdDemo("tutorial","authentication",user,"language")
 k selected
 d getMultipleSelectValues^%zewdAPI("language",.selected,sessid)
 m ^ewdDemo("tutorial","authentication",user,"language")=selected
 k ^ewdDemo("tutorial","authentication",user,"comments")
 d getTextArea^%zewdAPI("comments",.comments,sessid)
 m ^ewdDemo("tutorial","authentication",user,"comments")=comments
 ;s warning="Record successfully updated"
 ;d setWarning^%zewdAPI(warning,sessid)
 ;
 QUIT ""
 ;
testAjaxForm(sessid)
 ;
 i $$getRequestValue^%zewdAPI("testField1",sessid)="" d  QUIT "field1 must not be null"
 . d setFieldError^%zewdAPI("testField1",sessid)
 i $$getRequestValue^%zewdAPI("testField2",sessid)="" d  QUIT "javascript: x=1 ; document.getElementById('testField3').value = x"
 . d setFieldError^%zewdAPI("testField2",sessid)
 i $$getRequestValue^%zewdAPI("testField3",sessid)="" d  QUIT "field3 must not be null"
 . d setFieldError^%zewdAPI("testField3",sessid)
 ;
 QUIT ""
 ;
getVersion() ;
 QUIT $zv
 ;
getTime(sessid)
 ;
 d setSessionValue^%zewdAPI("dateTime",$$inetDate^%zewdAPI($h),sessid)
 QUIT ""
 ;
