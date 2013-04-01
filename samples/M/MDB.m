MDB ; M/DB: Mumps Emulation of Amazon SimpleDB
 ;
 ; ----------------------------------------------------------------------------
 ; | M/DB                                                                     |
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
 ;
version()	
	QUIT "44"
	;
buildDate()	
	QUIT "06 July 2011"
	;
indexLength()
 QUIT 180
 ;
 ; Note: keyId will have been tested and must be valid
 ; by the time these methods are called
 ;
 ; To Initialise the service: http://192.168.1.xxx/mdb/test.mgwsi?Action=Initialise
 ;
addUser(userKeyId,userSecretKey,requestId,boxUsage)
 ;
 n startTime,stop
 ;
 s requestId=$$init(.startTime)
 s ^MDBUAF("keys",userKeyId)=userSecretKey
 QUIT $$end(startTime,.boxUsage)
 ;
createDomain(keyId,domainName,requestId,boxUsage)
 ;
 n dn,dnx,id,noOfDomains,startTime,token
 ;
 s requestId=$$init(.startTime)
 s keyId=$g(keyId)
 i keyId="" QUIT $$end(startTime,.boxUsage,"MissingParameter","AWSAccessKeyId")
 s domainName=$g(domainName)
 i domainName="" QUIT $$end(startTime,.boxUsage,"MissingParameter","DomainName")
 s dn=$tr(domainName,"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890_-.","")
 i dn'="" QUIT $$end(startTime,.boxUsage,"InvalidParameterValue",domainName,"DomainName")
 ;
 s noOfDomains=+$g(^MDB(keyId))
 i $g(^MDBConfig("DomainsPerAccount"))'="",noOfDomains=^MDBConfig("DomainsPerAccount") QUIT $$end(startTime,.boxUsage,"NumberDomainsExceeded")
 s dnx=$e(domainName,1,$$indexLength())
 i '$$domainExists(keyId,domainName) d
 . s noOfDomains=$increment(^MDB(keyId))
 . s id=$increment(^MDB(keyId,"domains"))
 . s ^MDB(keyId,"domains",id,"name")=domainName
 . s ^MDB(keyId,"domains",id,"created")=$h
 . d updateDomainMetaData(keyId,id)
 . s ^MDB(keyId,"domainIndex",dnx,id)=""
 ;
 QUIT $$end(startTime,.boxUsage)
 ;
updateDomainMetaData(keyId,id)
 ;
 s ^MDB(keyId,"domains",id,"modified")=$h
 QUIT
 ;
getDomainMetaData(keyId,domainId,metaData)
 ;
 n size,timestamp
 ;
 k metaData
 s timestamp=$g(^MDB(keyId,"domains",domainId,"modified"))
 s timestamp=$$convertToEpochTime(timestamp)
 s metaData("Timestamp")=timestamp
 s metaData("ItemCount")=$$countItems(keyId,domainId,.size)_".0"
 s metaData("ItemNamesSizeBytes")=size_".0"
 s metaData("AttributeValueCount")=$$countNVPs(keyId,domainId,.size)_".0"
 s metaData("AttributeValuesSizeBytes")=size_".0"
 s metaData("AttributeNameCount")=$$countAttributeNames(keyId,domainId,.size)_".0"
 s metaData("AttributeNamesSizeBytes")=size_".0"
 QUIT
 ;
domainMetadata(keyId,domainName,metaData,requestId,boxUsage)
 ;
 n domainId,startTime
 ;
 s requestId=$$init(.startTime)
 s keyId=$g(keyId)
 i keyId="" QUIT $$end(startTime,.boxUsage,"MissingParameter","AWSAccessKeyId")
 s domainName=$g(domainName)
 i domainName="" QUIT $$end(startTime,.boxUsage,"MissingParameter","DomainName")
 s domainId=$$getDomainId(keyId,domainName)
 i domainId="" QUIT $$end(startTime,.boxUsage,"NoSuchDomain","The specified domain does not exist.")
 ;
 d getDomainMetaData(keyId,domainId,.metaData)
 ;
 QUIT $$end(startTime,.boxUsage)
 ;
convertToEpochTime(dh)
 ;
 n time
 ;
 s time=(dh*86400)+$p(dh,",",2)
 s time=time-4070908800
 QUIT time
 ;
convertFromEpochTime(time)
 ;
 n dh
 ;
 s time=time+4070908800
 s dh=time\86400
 s time=time#86400
 QUIT dh_","_time
 ;
countItems(keyId,domainId,size)
 ;
 n count,id
 ;
 s id="",count=0,size=0
 f  s id=$o(^MDB(keyId,"domains",domainId,"items",id)) q:id=""  d
 . s count=count+1
 . s size=size+$l(^MDB(keyId,"domains",domainId,"items",id))
 QUIT count
 ;
countNVPs(keyId,domainId,size)
 ;
 n attribId,count,itemId,valueId
 ;
 s itemId="",count=0,size=0
 f  s itemId=$o(^MDB(keyId,"domains",domainId,"items",itemId)) q:itemId=""  d
 . s attribId=0
 . f  s attribId=$o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId)) q:attribId=""  d
 . . s valueId=""
 . . f  s valueId=$o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)) q:valueId=""  d
 . . . s count=count+1
 . . . s size=size+$l(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId))
 QUIT count
 ;
countAttributeNames(keyId,domainId,size)
 ;
 n attribId,count,name
 ;
 s attribId=0,count=0,size=0
 f  s attribId=$o(^MDB(keyId,"domains",domainId,"attribs",attribId)) q:attribId=""  d
 . s count=count+1
 . s name=^MDB(keyId,"domains",domainId,"attribs",attribId)
 . s size=size+$l(name)
 QUIT count
 ;
 
domainExists(keyId,name)
 ;
 n id
 ;
 s id=$$getDomainId($g(keyId),$g(name))
 QUIT (id'="")
 ;
getDomainId(keyId,name)
 ;
 n found,id,namex
 ;
 i $g(name)="" QUIT ""
 i $g(keyId)="" QUIT ""
 s namex=$e(name,1,$$indexLength())
 s id="",found=0
 f  s id=$o(^MDB(keyId,"domainIndex",namex,id)) q:id=""  d  q:found
 . i $g(^MDB(keyId,"domains",id,"name"))=name s found=1
 i id'="",'$d(^MDB(keyId,"domains",id,"attribs",0)) d buildItemNameIndex(keyId,id)
 QUIT id
 ;
buildItemNameIndex(keyId,domainId)
 ;
 n itemId,itemValue,itemValuex
 ;
 s ^MDB(keyId,"domains",domainId,"attribs",0)="itemName()"
 s ^MDB(keyId,"domains",domainId,"attribsIndex","itemName()",0)=""
 s itemId=""
 f  s itemId=$o(^MDB(keyId,"domains",domainId,"items",itemId)) q:itemId=""  d
 . s itemValue=^MDB(keyId,"domains",domainId,"items",itemId)
 . s itemValuex=$e(itemValue,1,$$indexLength())
 . s ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",0,"value")=1
 . s ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",0,"value",1)=itemValue
 . s ^MDB(keyId,"domains",domainId,"queryIndex",0,itemValuex,itemId)=""
 QUIT 
 ;
countDomains(key)
 ;
 n id,no
 ;
 s id="",no=0
 f  s id=$o(^MDB(key,"domains",id)) q:id=""  s no=no+1
 QUIT no
 ;
deleteDomain(keyId,domainName,requestId,boxUsage)
 ;
 n dn,dnx,id,noOfDomains,startTime,token
 ;
 s requestId=$$init(.startTime)
 s keyId=$g(keyId)
 i keyId="" QUIT $$end(startTime,.boxUsage,"MissingParameter","AWSAccessKeyId")
 s domainName=$g(domainName)
 i domainName="" QUIT $$end(startTime,.boxUsage,"MissingParameter","DomainName")
 s id=$$getDomainId(keyId,domainName)
 i id="" QUIT $$end(startTime,.boxUsage)
 k ^MDB(keyId,"domains",id)
 k ^MDB(keyId,"domainIndex",$e(domainName,1,$$indexLength()),id)
 s noOfDomains=$$countDomains(keyId)
 i noOfDomains>0 d
 . s ^MDB(keyId)=noOfDomains
 e  d
 . k ^MDB(keyId)
 ;
 QUIT $$end(startTime,.boxUsage)
 ;
listDomains(keyId,maxNoOfDomains,nextToken,domainList,requestId,boxUsage)
 ;
 n domainName,fullName,id,noOfDomains,startTime,stop,token
 ;
 s requestId=$$init(.startTime)
 s keyId=$g(keyId)
 i keyId="" QUIT $$end(startTime,.boxUsage,"MissingParameter","AWSAccessKeyId")
 k domainList s noOfDomains=0,stop=0
 i $g(nextToken)="" d
 . s domainName=""
 e  d
 . s domainName=$$decodeBase64(nextToken)
 s nextToken=""
 f  s domainName=$o(^MDB(keyId,"domainIndex",domainName)) q:domainName=""  d  q:stop
 . s id=""
 . f  s id=$o(^MDB(keyId,"domainIndex",domainName,id)) q:id=""  d
 . . s fullName=$g(^MDB(keyId,"domains",id,"name"))
 . . s noOfDomains=noOfDomains+1
 . . s domainList(noOfDomains)=fullName
 . i noOfDomains=maxNoOfDomains d  q
 . . s stop=1
 . . s nextToken=$$encodeBase64(domainName)
 ;
 QUIT $$end(startTime,.boxUsage)
 ;
itemExists(keyId,domainId,name)
 ;
 n id
 ;
 s id=$$getItemId($g(keyId),$g(domainId),$g(name))
 QUIT (id'="")
 ;
getItemId(keyId,domainId,name)
 ;
 n found,id,namex
 ;
 i $g(domainId)="" QUIT ""
 i $g(name)="" QUIT ""
 i $g(keyId)="" QUIT ""
 i domainId="" QUIT ""
 s namex=$e(name,1,$$indexLength())
 s id="",found=0
 f  s id=$o(^MDB(keyId,"domains",domainId,"itemIndex",namex,id)) q:id=""  d  q:found
 . i $g(^MDB(keyId,"domains",domainId,"items",id))=name s found=1
 QUIT id
 ;
getAttributeId(keyId,domainId,name)
 ;
 n found,id,namex
 ;
 i $g(domainId)="" QUIT ""
 i $g(name)="" QUIT ""
 i $g(keyId)="" QUIT ""
 i domainId="" QUIT ""
 s namex=$e(name,1,$$indexLength())
 s id="",found=0
 f  s id=$o(^MDB(keyId,"domains",domainId,"attribsIndex",namex,id)) q:id=""  d  q:found
 . i $g(^MDB(keyId,"domains",domainId,"attribs",id))'=name q
 . s found=1
 QUIT id
 ;
getAttributeValueId(keyId,domainId,itemId,attribId,value)
 ;
 n found,id,valuex
 ;
 i $g(domainId)="" QUIT ""
 i '$d(value) QUIT ""
 i $g(keyId)="" QUIT ""
 i domainId="" QUIT ""
 i value="" s value=$c(31)
 s itemId=$g(itemId)
 i itemId="" QUIT ""
 s valuex=$e(value,1,$$indexLength())
 s id="",found=0
 f  s id=$o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"valueIndex",valuex,id)) q:id=""  d  q:found
 . i $g(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",id))'=value q
 . s found=1
 QUIT id
 
putAttributes(keyId,domainName,itemName,attributes,requestId,boxUsage)
 ;
 n attribId,domainId,itemId,name,namex,no,replace,startTime,value,valueId,valuex,xvalue
 ;
 s requestId=$$init(.startTime)
 s keyId=$g(keyId)
 i keyId="" QUIT $$end(startTime,.boxUsage,"MissingParameter","AWSAccessKeyId")
 s domainName=$g(domainName)
 i domainName="" QUIT $$end(startTime,.boxUsage,"MissingParameter","DomainName")
 s domainId=$$getDomainId(keyId,domainName)
 i domainId="" QUIT $$end(startTime,.boxUsage,"NoSuchDomain","The specified domain does not exist.")
 s itemName=$g(itemName)
 i itemName="" QUIT $$end(startTime,.boxUsage,"MissingParameter","ItemName")
 s itemId=$$getItemId(keyId,domainId,itemName)
 i itemId="" d
 . ; add Item to Domain if it's new
 . n itemNamex
 . s itemNamex=$e(itemName,1,$$indexLength())
 . s itemId=$increment(^MDB(keyId,"domains",domainId,"items"))
 . s ^MDB(keyId,"domains",domainId,"itemIndex",itemNamex,itemId)=""
 . s ^MDB(keyId,"domains",domainId,"items",itemId)=itemName
 . s ^MDB(keyId,"domains",domainId,"attribs",0)="itemName()"
 . s ^MDB(keyId,"domains",domainId,"attribsIndex","itemName()",0)=""
 . s ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",0,"value")=1
 . s ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",0,"value",1)=itemName
 . s ^MDB(keyId,"domains",domainId,"queryIndex",0,itemNamex,itemId)=""
 ;
 ; attributes(no,"name")=attribute name
 ; attributes(no,"value")=attribute value
 ; attributes(no,"replace")=1
 ;
 s no=""
 f  s no=$o(attributes(no)) q:no=""  d
 . s name=$g(attributes(no,"name"))
 . s value=$g(attributes(no,"value"))
 . i value="" s value=$c(31)
 . s replace=+$g(attributes(no,"replace"))
 . s namex=$e(name,1,$$indexLength())
 . s valuex=$e(value,1,$$indexLength())
 . s attribId=$$getAttributeId(keyId,domainId,name)
 . i attribId="" d
 . . ; add new attribute name to the domain
 . . s attribId=$increment(^MDB(keyId,"domains",domainId,"attribs"))
 . . s ^MDB(keyId,"domains",domainId,"attribs",attribId)=name
 . . s ^MDB(keyId,"domains",domainId,"attribsIndex",namex,attribId)=""
 . s valueId=$$getAttributeValueId(keyId,domainId,itemId,attribId,value)
 . i 'replace,valueId'="" q  ; Not allowed to have more than one attribute with the same name and value
 . i replace d
 . . ; first remove any existing values for this attribute name
 . . f  s valueId=$o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)) q:valueId=""  d
 . . . s xvalue=^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)
 . . . k ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"valueIndex",xvalue,valueId)
 . . . k ^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)
 . . k ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value")
 . ; now add the new attribute name/value pair
 . s valueId=$increment(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value"))
 . s ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)=value
 . s ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"valueIndex",valuex,valueId)=""
 . s ^MDB(keyId,"domains",domainId,"queryIndex",attribId,valuex,itemId)=""
 ;
 d updateDomainMetaData(keyId,domainId)
 QUIT $$end(startTime,.boxUsage)
 ;
batchPutItem(keyId,domainId,itemName,attributesJSON)
 ;
 n attribId,attributes,error,itemId,name,namex,no,replace,value,valueId,valuex,xvalue
 ;
 ;d trace^zmwire("batchPutItem: keyid="_keyId_"; domainId="_domainId_"; itemName="_itemName_"; attributes="_attributesJSON)
 s itemName=$g(itemName)
 i itemName="" QUIT 0
 s itemId=$$getItemId(keyId,domainId,itemName)
 i itemId="" d
 . ; add Item to Domain if it's new
 . n itemNamex
 . s itemNamex=$e(itemName,1,$$indexLength())
 . s itemId=$increment(^MDB(keyId,"domains",domainId,"items"))
 . s ^MDB(keyId,"domains",domainId,"itemIndex",itemNamex,itemId)=""
 . s ^MDB(keyId,"domains",domainId,"items",itemId)=itemName
 . s ^MDB(keyId,"domains",domainId,"attribs",0)="itemName()"
 . s ^MDB(keyId,"domains",domainId,"attribsIndex","itemName()",0)=""
 . s ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",0,"value")=1
 . s ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",0,"value",1)=itemName
 . s ^MDB(keyId,"domains",domainId,"queryIndex",0,itemNamex,itemId)=""
 ;
 s error=$$parseJSON^zmwire(attributesJSON,.attributes,1)
 ;
 ; attributes(no,"name")=attribute name
 ; attributes(no,"value")=attribute value
 ; attributes(no,"replace")=1
 ;
 s no=""
 f  s no=$o(attributes(no)) q:no=""  d
 . s name=$g(attributes(no,"name"))
 . s value=$g(attributes(no,"value"))
 . i value="" s value=$c(31)
 . s replace=+$g(attributes(no,"replace"))
 . s namex=$e(name,1,$$indexLength())
 . s valuex=$e(value,1,$$indexLength())
 . s attribId=$$getAttributeId(keyId,domainId,name)
 . i attribId="" d
 . . ; add new attribute name to the domain
 . . s attribId=$increment(^MDB(keyId,"domains",domainId,"attribs"))
 . . s ^MDB(keyId,"domains",domainId,"attribs",attribId)=name
 . . s ^MDB(keyId,"domains",domainId,"attribsIndex",namex,attribId)=""
 . s valueId=$$getAttributeValueId(keyId,domainId,itemId,attribId,value)
 . i 'replace,valueId'="" q  ; Not allowed to have more than one attribute with the same name and value
 . i replace d
 . . ; first remove any existing values for this attribute name
 . . f  s valueId=$o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)) q:valueId=""  d
 . . . s xvalue=^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)
 . . . k ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"valueIndex",xvalue,valueId)
 . . . k ^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)
 . . k ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value")
 . ; now add the new attribute name/value pair
 . s valueId=$increment(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value"))
 . s ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)=value
 . s ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"valueIndex",valuex,valueId)=""
 . s ^MDB(keyId,"domains",domainId,"queryIndex",attribId,valuex,itemId)=""
 QUIT 1
 ;
getAttributes(keyId,domainName,itemName,attributes,requestId,boxUsage,suppressBoxUsage)
 ;
 n attribId,attrNo,domainId,itemId,name,startTime,value,valueId,valueNo
 ;
 s requestId=""
 i '$g(suppressBoxUsage) s requestId=$$init(.startTime)
 s keyId=$g(keyId)
 i keyId="" QUIT $$end(startTime,.boxUsage,"MissingParameter","AWSAccessKeyId")
 s domainName=$g(domainName)
 i domainName="" QUIT $$end(startTime,.boxUsage,"MissingParameter","DomainName")
 s domainId=$$getDomainId(keyId,domainName)
 i domainId="" QUIT $$end(startTime,.boxUsage,"NoSuchDomain","The specified domain does not exist.")
 s itemName=$g(itemName)
 i itemName="" QUIT $$end(startTime,.boxUsage,"MissingParameter","ItemName")
 s itemId=$$getItemId(keyId,domainId,itemName)
 i itemId="" QUIT $$end(startTime,.boxUsage,"NoSuchItemName","The specified ItemName does not exist.")
 ;
 ; attributes(no)=attribute name
 ; attributes(no,"value",vno)=attribute value
 ;
 i '$d(attributes) d
 . s name="",attrNo=0
 . f  s name=$o(^MDB(keyId,"domains",domainId,"attribsIndex",name)) q:name=""  d 
 . . s attribId=0
 . . f  s attribId=$o(^MDB(keyId,"domains",domainId,"attribsIndex",name,attribId)) q:attribId=""  d
 . . . i '$d(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId)) q
 . . . s attrNo=attrNo+1
 . . . s attributes(attrNo)=^MDB(keyId,"domains",domainId,"attribs",attribId)
 . . . s valueId="",valueNo=0
 . . . f  s valueId=$o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)) q:valueId=""  d
 . . . . s value=^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)
 . . . . s valueNo=valueNo+1
 . . . . i value=$c(31) s value=""
 . . . . s attributes(attrNo,"value",valueNo)=value
 e  d
 . s attrNo=""
 . f  s attrNo=$o(attributes(attrNo)) q:attrNo=""  d
 . . s name=attributes(attrNo)
 . . s attribId=$$getAttributeId(keyId,domainId,name)
 . . i attribId="" q
 . . s valueId="",valueNo=0
 . . f  s valueId=$o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)) q:valueId=""  d
 . . . s value=^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)
 . . . s valueNo=valueNo+1
 . . . i value=$c(31) s value=""
 . . . s attributes(attrNo,"value",valueNo)=value
 . . . ;i $g(^zewd("trace")) d trace($h_": attributes("_attrNo_",value,"_valueNo_")="_value)
 ;
 i $g(suppressBoxUsage) QUIT ""
 QUIT $$end(startTime,.boxUsage)
 ;
deleteAttributes(keyId,domainName,itemName,attributes,requestId,boxUsage)
 ;
 n attribId,attrNo,domainId,itemId,name,namex,startTime,value,valueId,valueNo,valuex
 ;
 s requestId=$$init(.startTime)
 s keyId=$g(keyId)
 i keyId="" QUIT $$end(startTime,.boxUsage,"MissingParameter","AWSAccessKeyId")
 s domainName=$g(domainName)
 i domainName="" QUIT $$end(startTime,.boxUsage,"MissingParameter","DomainName")
 s domainId=$$getDomainId(keyId,domainName)
 i domainId="" QUIT $$end(startTime,.boxUsage,"NoSuchDomain","The specified domain does not exist.")
 s itemName=$g(itemName)
 i itemName="" QUIT $$end(startTime,.boxUsage,"MissingParameter","ItemName")
 s itemId=$$getItemId(keyId,domainId,itemName)
 i itemId="" QUIT $$end(startTime,.boxUsage,"NoSuchItemName","The specified ItemName does not exist.")
 i '$d(attributes) d
 . ; delete all attributes for this item, first the associated queryIndex records
 . s attribId=""
 . f  s attribId=$o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId)) q:attribId=""  d
 . . s valuex=""
 . . f  s valuex=$o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"valueIndex",valuex)) q:valuex=""  d
 . . . k ^MDB(keyId,"domains",domainId,"queryIndex",attribId,valuex,itemId)
 . k ^MDB(keyId,"domains",domainId,"items",itemId)
 . s namex=$e(itemName,1,$$indexLength())
 . k ^MDB(keyId,"domains",domainId,"itemIndex",namex,itemId)
 e  d
 . ;delete the specified attribute name/value pairs
 . ; attributes(no)=name
 . ; attributes(no,"value",vno)=value
 . s attrNo=""
 . f  s attrNo=$o(attributes(attrNo)) q:attrNo=""  d
 . . s name=$g(attributes(attrNo))
 . . i name="" q
 . . s attribId=$$getAttributeId(keyId,domainId,name)
 . . i attribId="" q
 . . s valueNo=""
 . . f  s valueNo=$o(attributes(attrNo,"value",valueNo)) q:valueNo=""  d
 . . . s value=attributes(attrNo,"value",valueNo)
 . . . s valueId=$$getAttributeValueId(keyId,domainId,itemId,attribId,value)
 . . . i valueId="" q
 . . . ; remove specified value
 . . . s valuex=$e(value,1,$$indexLength())
 . . . k ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"valueIndex",valuex,valueId)
 . . . k ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",valueId)
 . . . k ^MDB(keyId,"domains",domainId,"queryIndex",attribId,valuex,itemId)
 . . ; if no values are left, completely remove attribute from item
 . . i $o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value",""))="" d
 . . . k ^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId,"value")
 . . ; if no references left to this attribute name, remove the attribute name
 . . i '$d(^MDB(keyId,"domains",domainId,"queryIndex",attribId)) d
 . . . s namex=$e(name,1,$$indexLength())
 . . . k ^MDB(keyId,"domains",domainId,"attribs",attribId)
 . . . k ^MDB(keyId,"domains",domainId,"attribsIndex",namex,attribId)
 . ; if no attributes are left, remove the item
 . i $o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",""))="" d
 . . k ^MDB(keyId,"domains",domainId,"items",itemId)
 . . s namex=$e(itemName,1,$$indexLength())
 . . k ^MDB(keyId,"domains",domainId,"itemIndex",namex,itemId)
 . . k ^MDB(keyId,"domains",domainId,"attribs")
 . . k ^MDB(keyId,"domains",domainId,"attribsIndex")
 ;
 QUIT $$end(startTime,.boxUsage)
 ;
query(keyId,domainName,queryExpression,maxNoOfItems,nextToken,itemList,requestId,boxUsage)
 ;
 n context,name,error,startTime,stop,token,value,xvalue
 ;
 i $zv["GT.M" d
 . s context=1
 . i $d(^zewd("config","MGWSI")) s context=0
 s requestId=$$init(.startTime)
 s keyId=$g(keyId)
 i keyId="" QUIT $$end(startTime,.boxUsage,"MissingParameter","keyId")
 s domainName=$g(domainName)
 i domainName="" QUIT $$end(startTime,.boxUsage,"MissingParameter","DomainName")
 s queryExpression=$g(queryExpression)
 i $g(^zewd("trace"))=1 d trace("Query Expression="_queryExpression)
 s maxNoOfItems=$g(maxNoOfItems)
 k itemList
 ;
 s error=""
 i $g(nextToken)'="" d  QUIT error
 . n n,pos
 . s pos=$g(^MDB(keyId,"queryResults","nextToken",nextToken))
 . i pos="" s error="InvalidNextToken~The specified next token is not valid" q
 . s n=0,nextToken=""
 . f  s pos=$o(^MDB(keyId,"queryResults","itemList",pos)) q:pos=""  d  q:n=maxNoOfItems
 . . s n=n+1
 . . s itemList(n)=^MDB(keyId,"queryResults","itemList",pos)
 . i pos'="" d
 . . s nextToken=itemList(n)
 . . i $zv["GT.M" d
 . . . s nextToken=$$B64^%ZMGWSIS(nextToken)
 . . e  d
 . . . s nextToken=$$b64Encode^MDBMCache(nextToken)
 . . s ^MDB(keyId,"queryResults","nextToken",nextToken)=pos
 . e  d
 . . k ^MDB(keyId,"queryResults")
 . s error=$$end(startTime,.boxUsage)
 ;
 s nextToken=""
 s error=$$runQuery(keyId,domainName,queryExpression,nextToken,.itemList)
 i error'="" QUIT error
 i $g(maxNoOfItems)>0 d
 . ; add session identifier to stored records
 . k ^MDB(keyId,"queryResults")
 . m ^MDB(keyId,"queryResults","itemList")=itemList
 . ;i queryExpression["sort " s ^MDB(keyId,"queryResults","sorted")=1
 . k itemList
 . s n=0,pos=""
 . f  s pos=$o(^MDB(keyId,"queryResults","itemList",pos)) q:pos=""  d  q:n=maxNoOfItems
 . . s n=n+1
 . . s itemList(n)=^MDB(keyId,"queryResults","itemList",pos)
 . i pos'="" d
 . . s nextToken=itemList(n)
 . . i $zv["GT.M" d
 . . . s nextToken=$$B64^%ZMGWSIS(nextToken,context)
 . . e  d
 . . . s nextToken=$$b64Encode^MDBMCache(nextToken)
 . . s ^MDB(keyId,"queryResults","nextToken",nextToken)=pos
 ;
 QUIT $$end(startTime,.boxUsage)
 ;
 ;
 ;  MDB Server side response to incoming REST requests
 ;
mgwsiResponse(cgi,data)
 ; m_apache HTTP entry point: normalise to WebLink interface
 n %CGIEVAR,n,name,%KEY,unescName
 i $g(^zewd("trace"))=1 k ^mdbcgi m ^mdbcgi=cgi
 i $g(^zewd("trace"))=1 s n=$increment(^mdbdata) m ^mdbdata(n)=data
 i $g(^%zewd("relink"))=1,'$d(^%zewd("relink","process",$j)) s ok=$$relink^%zewdGTMRuntime()
 m %CGIEVAR=cgi
 s name=""
 f  s name=$o(data(name)) q:name=""  d
 . i name="$CONTENT" q
 . s unescName=$$urlDecode(name)
 . s %KEY(unescName)=$$urlDecode($g(data(name,1)))
 . i %KEY(unescName)[$c(13,10) s %KEY(unescName)=$$replace(%KEY(unescName),$c(13,10),"")
 d response
 QUIT
 ;
response
 i $d(%KEY) d
 . ; WebLink access entry point here
 . n action,attributes,AWSAcessKeyId,boxUsage,db,error,hash,itemsAndAttrs,keyId
 . n requestId,secretKey,signatureMethod,signatureVersion,stringToSign
 . ;
 . ;k ^rltKey m ^rltKey=%KEY
 . s db=$g(%KEY("db"))
 . i db="" s db="mdb" ;,%KEY("db")=db
 . s action=$g(%KEY("Action"))
 . i $g(^zewd("trace"))=1 d
 . . n i
 . . d trace("MDB request processing for "_action_": started at "_$h_"; process: "_$j)
 . . s i=$increment(^mdbKey)
 . . m ^mdbKey(i)=%KEY
 . i action="Initialise"!(action="initialise")!(action="Initialize")!(action="initialize") d  QUIT
 . . s %KEY("db")=db
 . . i $d(^MDBUAF("administrator")) d errorResponse("InvalidAction","M DB has already been initialised") q
 . . s error=$$initialise(.requestId,.boxUsage)
 . . i error'="" d errorResponse("InvalidConfigurationFile",error) q
 . . d createResponse(action,requestId,boxUsage)
 . i action="InstallMDBX" d  QUIT
 . . s %KEY("db")=db
 . . s error=$$installMDBX(.requestId,.boxUsage)
 . . i error'="" d errorResponse("InvalidInstallRequest",error) q
 . . d createResponse(action,requestId,boxUsage)
 . i action="InstallMDBM" d  QUIT
 . . s %KEY("db")=db
 . . s error=$$installMDBM(.requestId,.boxUsage)
 . . i error'="" d errorResponse("InvalidInstallRequest",error) q
 . . d createResponse(action,requestId,boxUsage)
 . i $g(%KEY("MDBToken"))'="" d  q:error
 . . n keyId
 . . s error=""
 . . s keyId=$$authenticate^MDBSession(%KEY("MDBToken"))
 . . i keyId="" s error=1 d errorResponse("InvalidTokenId","The Access Token you provided does not exist in our records") QUIT
 . e  d  q:error=1
 . . s error=""
 . . s signatureVersion=+$g(%KEY("SignatureVersion"))
 . . s signatureMethod=$g(%KEY("SignatureMethod"))
 . . s stringToSign=$$createResponseStringToSign(signatureVersion)
 . . s keyId=$g(%KEY("AWSAccessKeyId"))
 . . i keyId="" s keyId=$g(%KEY("MDBAccessKeyId"))
 . . i action="AddUser" d  i error'="" QUIT
 . . . s error=""
 . . . i keyId=$g(^MDBUAF("administrator")) q
 . . . d errorResponse("InvalidAdministratorKey","The Access Key Id was not that of the Administrator")
 . . . s error=1
 . . i keyId="" s error=1 d errorResponse("InvalidClientTokenId","The Access Key Id you provided does not exist in our records") QUIT
 . . i $g(^MDBUAF("keys",keyId))="" s error=1 d errorResponse("InvalidClientTokenId","The Access Key Id you provided does not exist in our records") QUIT
 . . i $g(%KEY("Signature"))="" s error=1 d errorResponse("SignatureDoesNotMatch","The request signature we calculated does not match the signature you provided.  Check your Secret Access Key and signing method.  Consult the service documentation for details") QUIT
 . . s secretKey=$g(^MDBUAF("keys",keyId))
 . . s hash=$$getSignedString(stringToSign,secretKey,signatureMethod)
 . . i $g(^zewd("trace"))=1 d trace($h_": string to sign:"_stringToSign)
 . . i $g(^zewd("trace"))=1 d trace($h_": hash="_hash_"; signature rcvd="_%KEY("Signature"))
 . . i hash'=%KEY("Signature") s error=1 d errorResponse("SignatureDoesNotMatch","The request signature we calculated does not match the signature you provided.  Check your Secret Access Key and signing method.  Consult the service documentation for details") QUIT
 . ;
 . ; Security OK
 . ;
 . ;d trace("security ok: db="_db)
 . i $g(^MDBAPI(db,action))'="" d  QUIT
 . . ; Custom extension gateway.  Method should return output in response(lineNo)
 . . ; any error should be returned as errorCode~errorText
 . . n doll,lineNo,method,no,requestId,response,startTime,x
 . . s method=^MDBAPI(db,action)
 . . s x="s error=$$"_method_"(.%KEY,.response)"
 . . s requestId=$$init(.startTime)
 . . x x
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . i $$end(startTime,.boxUsage)
 . . s lineNo=1
 . . i $g(%KEY("mdbRawOutput"))'="true",$g(%KEY("OutputFormat"))'="JSON" d
 . . . s ^CacheTempEWD($j,lineNo)="<?xml version='1.0'?>"_$c(13,10),lineNo=lineNo+1
 . . . s ^CacheTempEWD($j,lineNo)="<"_action_"Response xmlns=""http://"_db_".mgateway.com/doc/2009-06-05/"">",lineNo=lineNo+1
 . . s no=""
 . . f  s no=$o(response(no)) q:no=""  d
 . . . s ^CacheTempEWD($j,lineNo)=response(no),lineNo=lineNo+1
 . . d createResponse(db_":"_action,requestId,boxUsage) QUIT
 . ;
 . i db="mdb" s %KEY("db")="mdb"
 . i action="AddUser" d  QUIT
 . . n userKeyId,userSecretKey
 . . s userKeyId=$g(%KEY("UserAccessKeyId"))
 . . i userKeyId="" d errorResponse("InvalidKeyId","The new user Access Key Id was not defined") QUIT
 . . s userSecretKey=$g(%KEY("UserSecretKey"))
 . . i userSecretKey="" d errorResponse("InvalidSecretKey","The new user Secret Key was not defined") QUIT
 . . s error=$$addUser(userKeyId,userSecretKey,.requestId,.boxUsage)
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . d createResponse(action,requestId,boxUsage) QUIT
 . ;
 . i action="GetToken" d  QUIT
 . . n boxUsage,requestId,startTime,token
 . . s requestId=$$init(.startTime)
 . . s token=$$createNewSession^MDBSession(keyId,1200)
 . . i $$end(startTime,.boxUsage)
 . . d createResponse(action,requestId,boxUsage)
 . ;
 . i action="CreateDomain" d  QUIT
 . . n domainName,resp
 . . s domainName=$g(%KEY("DomainName"))
 . . s error=$$createDomain(keyId,domainName,.requestId,.boxUsage)
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . d createResponse(action,requestId,boxUsage) QUIT
 . ;
 . i action="DeleteDomain" d  QUIT
 . . n domainName,resp
 . . s domainName=$g(%KEY("DomainName"))
 . . s error=$$deleteDomain(keyId,domainName,.requestId,.boxUsage)
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . d createResponse(action,requestId,boxUsage) QUIT
 . ;
 . i action="ListDomains" d  QUIT
 . . ;d trace("in ListDomains")
 . . n domainList,maxNoOfDomains,nextToken,resp
 . . s maxNoOfDomains=$g(%KEY("MaxNumberOfDomains"))
 . . s nextToken=$g(%KEY("NextToken"))
 . . s error=$$listDomains(keyId,maxNoOfDomains,.nextToken,.domainList,.requestId,.boxUsage)
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . d createResponse(action,requestId,boxUsage) QUIT
 . ;
 . i action="DomainMetadata" d  QUIT
 . . n domainName,metaData
 . . s domainName=$g(%KEY("DomainName"))
 . . s error=$$domainMetadata(keyId,domainName,.metaData,.requestId,.boxUsage)
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . d createResponse(action,requestId,boxUsage) QUIT
 . ;
 . i action="PutAttributes" d  QUIT
 . . n domainName,error,i,itemName,name,paramName,paramValue,replace,start,stop
 . . s domainName=$g(%KEY("DomainName"))
 . . s itemName=$g(%KEY("ItemName"))
 . . s stop=0
 . . s name=$o(%KEY("Attribute."))
 . . s start=$p(name,".",2)
 . . s error=""
 . . f i=start:1 d  q:stop
 . . . s paramName="Attribute."_i_".Name"
 . . . i '$d(%KEY(paramName)) s stop=1 q
 . . . s paramValue="Attribute."_i_".Value"
 . . . ;i '$d(%KEY(paramValue)) s stop=1 q
 . . . i '$d(%KEY(paramValue)) s error="MissingParameter~Attribute Value missing for Attribute Name='"_%KEY(paramName),stop=1 q
 . . . ;i %KEY(paramValue)="" s error="MissingParameter~Attribute Value missing for Attribute Name='"_%KEY(paramName),stop=1
 . . . s replace=$g(%KEY("Attribute."_i_".Replace"))
 . . . i replace="true" s replace=1
 . . . ; attributes(no,"name")=attribute name
 . . . ; attributes(no,"value")=attribute value
 . . . ; attributes(no,"replace")=1
 . . . s attributes(i,"name")=%KEY(paramName)
 . . . s attributes(i,"value")=%KEY(paramValue)
 . . . i replace s attributes(i,"replace")=1
 . . i error="" s error=$$putAttributes(keyId,domainName,itemName,.attributes,.requestId,.boxUsage)
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . d createResponse(action,requestId,boxUsage) QUIT
 . ;
 . i action="GetAttributes" d  QUIT
 . . n domainName,error,i,itemName,name,paramName,paramValue,replace,start,stop
 . . s domainName=$g(%KEY("DomainName"))
 . . s itemName=$g(%KEY("ItemName"))
 . . s stop=0
 . . s name=$o(%KEY("AttributeName."))
 . . s start=$p(name,".",2)
 . . s error=""
 . . k attributes
 . . f i=start:1 d  q:stop
 . . . s paramName="AttributeName."_i
 . . . i '$d(%KEY(paramName)) s stop=1 q
 . . . s attributes(i)=%KEY(paramName)
 . . . ;s attributes(1)="test"
 . . s error=$$getAttributes(keyId,domainName,itemName,.attributes,.requestId,.boxUsage)
 . . ; attributes(no)=attribute name
 . . ; attributes(no,"value",vno)=attribute value
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . d createResponse(action,requestId,boxUsage)
 . ;
 . i action="DeleteAttributes" d  QUIT
 . . n attributes,domainName,error,i,itemName,name,paramName,paramValue,start,stop
 . . s domainName=$g(%KEY("DomainName"))
 . . s itemName=$g(%KEY("ItemName"))
 . . s stop=0
 . . s name=$o(%KEY("Attribute."))
 . . s start=$p(name,".",2)
 . . s error=""
 . . ; attributes(no)=name
 . . ; attributes(no,"value",vno)=value
 . . k attributes
 . . f i=start:1 d  q:stop
 . . . s paramName="Attribute."_i_".Name"
 . . . i '$d(%KEY(paramName)) s stop=1 q
 . . . s name=%KEY(paramName)
 . . . s attributes(i)=name
 . . . s paramValue="Attribute."_i_".Value"
 . . . i '$d(%KEY(paramValue)) s stop=1 q
 . . . i %KEY(paramValue)="" s error="MissingParameter~Attribute Value missing for Attribute Name='"_%KEY(paramName),stop=1
 . . . s attributes(i,"value",1)=%KEY(paramValue)
 . . i error="" s error=$$deleteAttributes(keyId,domainName,itemName,.attributes,.requestId,.boxUsage)
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . d createResponse(action,requestId,boxUsage) QUIT
 . ;
 . i action="Query" d  QUIT
 . . n domainName,error,itemList,maxNoOfItems,nextToken,queryExpression
 . . s domainName=$g(%KEY("DomainName"))
 . . s queryExpression=$g(%KEY("QueryExpression"))
 . . s maxNoOfItems=$g(%KEY("MaxNumberOfItems"))
 . . s nextToken=$g(%KEY("NextToken"))
 . . s error=$$query(keyId,domainName,queryExpression,maxNoOfItems,.nextToken,.itemList,.requestId,.boxUsage)
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . d createResponse(action,requestId,boxUsage) QUIT
 . ;
 . i action="QueryWithAttributes" d  QUIT
 . . n attribs,bx,domainName,error,ex,i,name,itemList,itemName,itemsAndAttrs,maxNoOfItems,name,nextToken
 . . n paramName,pos,queryExpression,rx,sorted,start,stop,sub
 . . ;
 . . s domainName=$g(%KEY("DomainName"))
 . . s queryExpression=$g(%KEY("QueryExpression"))
 . . s maxNoOfItems=$g(%KEY("MaxNumberOfItems"))
 . . s nextToken=$g(%KEY("NextToken"))
 . . s name=$o(%KEY("AttributeName."))
 . . s start=$p(name,".",2)
 . . s error="",stop=0
 . . f i=start:1 d  q:stop
 . . . s paramName="AttributeName."_i
 . . . i '$d(%KEY(paramName)) s stop=1 q
 . . . s attributes(i)=%KEY(paramName)
 . . s error=$$query(keyId,domainName,queryExpression,maxNoOfItems,.nextToken,.itemList,.requestId,.boxUsage)
 . . s pos=""
 . . f  s pos=$o(itemList(pos)) q:pos=""  d
 . . . s itemName=itemList(pos)
 . . . k attribs
 . . . m attribs=attributes
 . . . s ex=$$getAttributes(keyId,domainName,itemName,.attribs,.rx,.bx,1)
 . . . m itemsAndAttrs(pos)=attribs
 . . . s itemsAndAttrs(pos)=itemName
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . d createResponse(action,requestId,boxUsage) QUIT
 . ;
 . i action="Select" d  QUIT
 . . n attribs,attributes,boxUsage,bx,domainName,error,ex,itemList,itemName,nextToken
 . . n pos,requestId,rx,selectExpression,startTime
 . . ;
 . . s requestId=$$init(.startTime)
 . . s selectExpression=$g(%KEY("SelectExpression"))
 . . s nextToken=$g(%KEY("NextToken"))
 . . i $g(^zewd("trace"))=1 d trace($h_": Action Select, entering runSelect. keyId="_keyId_"; selectExpression="_selectExpression)
 . . s error=$$runSelect(keyId,selectExpression,.itemList,.attributes,.domainName)
 . . i $g(^zewd("trace")) d trace($h_": finished runSelect")
 . . i error'="" d errorResponse($p(error,"~",1),$p(error,"~",2)) QUIT
 . . i $g(attributes)="count(*)" d
 . . . n count
 . . . s itemsAndAttrs(1)="Domain"
 . . . s itemsAndAttrs(1,1)="Count"
 . . . s pos="",count=0
 . . . f  s pos=$o(itemList(pos)) q:pos=""  s count=count+1
 . . . s itemsAndAttrs(1,1,"value",1)=count
 . . e  d
 . . . s pos=""
 . . . ;i $g(^zewd("trace")) d trace($h_": pos=''")
 . . . f  s pos=$o(itemList(pos)) q:pos=""  d
 . . . . ;i $g(^zewd("trace")) d trace($h_": pos="_pos)
 . . . . s itemName=itemList(pos)
 . . . . k attribs
 . . . . m attribs=attributes
 . . . . ;i $g(^zewd("trace")) d trace($h_": calling getAttributes for keyId="_keyId_"; domainName="_domainName_"; itemName "_itemName)
 . . . . s ex=$$getAttributes(keyId,domainName,itemName,.attribs,.rx,.bx,1)
 . . . . ;i $g(^zewd("trace")) d trace($h_": finished getAttributes")
 . . . . m itemsAndAttrs(pos)=attribs
 . . . . s itemsAndAttrs(pos)=itemName
 . . ;i $g(^zewd("trace")) d trace($h_": about to call $$end")
 . . i $$end(startTime,.boxUsage)
 . . ;i $g(^zewd("trace")) d trace($h_": about to start createResponse")
 . . d createResponse(action,requestId,boxUsage) QUIT
 . ;
 . d errorResponse("InvalidAction","The action "_action_" is not valid for this web service") QUIT
 i $g(^zewd("trace"))=1 d trace("MDB request processing ended at "_$h)
 QUIT
 ;
createResponse(action,requestId,boxUsage)
 ;
 n len,lineNo
 ;
 ;i $g(^zewd("trace"))=1 d trace($h_": Commencing createResponse")
 i '$d(%KEY("isCSP")) d
 . w "HTTP/1.0 200 OK"_$c(13,10)
 . w "Date: "_$$inetDate^%zewdAPI($h)_" "_$tr($g(^MDBConfig("GMTOffset")),":","")_$c(13,10)
 . i $g(%KEY("OutputFormat"))="JSON" d
 . . w "Content-type: application/json"_$c(13,10)
 . e  d
 . . w "Content-type: text/xml"_$c(13,10)
 s lineNo=1
 i $g(%KEY("OutputFormat"))'="JSON" d
 . i $g(%KEY("db"))="mdb" d
 . . n apiVersion
 . . k ^CacheTempEWD($j)
 . . s lineNo=1
 . . s ^CacheTempEWD($j,lineNo)="<?xml version='1.0'?>"_$c(13,10),lineNo=lineNo+1
 . . s apiVersion=$g(%KEY("Version")) i apiVersion="" s apiVersion="2009-04-15"
 . . s ^CacheTempEWD($j,lineNo)="<"_action_"Response xmlns=""http://sdb.amazonaws.com/doc/"_apiVersion_"/"">",lineNo=lineNo+1
 . e  d
 . . s action=$p(action,":",2)
 . . s lineNo=$o(^CacheTempEWD($j,""),-1)+1
 ;
 i action="GetToken" d
 . i $g(%KEY("OutputFormat"))="JSON" d
 . . s ^CacheTempEWD($j,lineNo)="{token:"""_token_"""}"
 . e  d
 . . s ^CacheTempEWD($j,lineNo)="<GetTokenResult>"_token_"</GetTokenResult>",lineNo=lineNo+1
 ;
 i action="ListDomains" d
 . i '$d(domainList) d
 . . i $g(%KEY("OutputFormat"))="JSON" d
 . . . s ^CacheTempEWD($j,lineNo)="[]",lineNo=lineNo+1
 . . e  d
 . . . s ^CacheTempEWD($j,lineNo)="<ListDomainsResult />",lineNo=lineNo+1
 . e  d
 . . n comma,no
 . . i $g(%KEY("OutputFormat"))="JSON" d
 . . . s ^CacheTempEWD($j,lineNo)="[",lineNo=lineNo+1
 . . . s no="",comma=""
 . . . f  s no=$o(domainList(no)) q:no=""  d
 . . . . s ^CacheTempEWD($j,lineNo)=comma_""""_domainList(no)_"""",lineNo=lineNo+1,comma=","
 . . . s ^CacheTempEWD($j,lineNo)="]",lineNo=lineNo+1
 . . e  d
 . . . s ^CacheTempEWD($j,lineNo)="<ListDomainsResult>",lineNo=lineNo+1
 . . . s no=""
 . . . f  s no=$o(domainList(no)) q:no=""  d
 . . . . s ^CacheTempEWD($j,lineNo)="<DomainName>"_domainList(no)_"</DomainName>",lineNo=lineNo+1
 . . . i $g(nextToken)'="" s ^CacheTempEWD($j,lineNo)="<NextToken>"_nextToken_"</NextToken>",lineNo=lineNo+1
 . . . s ^CacheTempEWD($j,lineNo)="</ListDomainsResult>",lineNo=lineNo+1
 ;
 i action="CreateDomain",$g(%KEY("OutputFormat"))="JSON" d
 . s ^CacheTempEWD($j,lineNo)="{ok:true}",lineNo=lineNo+1
 ;
 i action="DeleteAttributes",$g(%KEY("OutputFormat"))="JSON" d
 . s ^CacheTempEWD($j,lineNo)="{ok:true}",lineNo=lineNo+1
 ;
 i action="DeleteDomain",$g(%KEY("OutputFormat"))="JSON" d
 . s ^CacheTempEWD($j,lineNo)="{ok:true}",lineNo=lineNo+1
 ;
 i action="PutAttributes",$g(%KEY("OutputFormat"))="JSON" d
 . s ^CacheTempEWD($j,lineNo)="{ok:true}",lineNo=lineNo+1
 ;
 i action="DomainMetadata" d
 . i '$d(metaData) d
 . . i $g(%KEY("OutputFormat"))="JSON" d
 . . . s ^CacheTempEWD($j,lineNo)="{}",lineNo=lineNo+1
 . . e  d
 . . . s ^CacheTempEWD($j,lineNo)="<DomainMetadataResult />",lineNo=lineNo+1
 . e  d
 . . n comma,name
 . . i $g(%KEY("OutputFormat"))="JSON" d
 . . . s ^CacheTempEWD($j,lineNo)="{",lineNo=lineNo+1
 . . . s name="",comma=""
 . . . f  s name=$o(metaData(name)) q:name=""  d
 . . . . s ^CacheTempEWD($j,lineNo)=comma_name_":"_metaData(name),lineNo=lineNo+1,comma=","
 . . . s ^CacheTempEWD($j,lineNo)="}",lineNo=lineNo+1
 . . e  d
 . . . s ^CacheTempEWD($j,lineNo)="<DomainMetadataResult>",lineNo=lineNo+1
 . . . s name=""
 . . . f  s name=$o(metaData(name)) q:name=""  d
 . . . . s ^CacheTempEWD($j,lineNo)="<"_name_">"_metaData(name)_"</"_name_">",lineNo=lineNo+1
 . . . s ^CacheTempEWD($j,lineNo)="</DomainMetadataResult>",lineNo=lineNo+1
 ;
 i action="GetAttributes",%KEY("db")="mdb" d
 . i '$d(attributes) d
 . . i $g(%KEY("OutputFormat"))="JSON" d
 . . . s ^CacheTempEWD($j,lineNo)="{}",lineNo=lineNo+1
 . . e  d
 . . . s ^CacheTempEWD($j,lineNo)="<GetAttributesResult />",lineNo=lineNo+1
 . e  d
 . . n acomma,comma,count,name,no,quote,stop,value,valueNo
 . . i $g(%KEY("OutputFormat"))="JSON" d
 . . . s ^CacheTempEWD($j,lineNo)="{",lineNo=lineNo+1
 . . . s no="",comma=""
 . . . f  s no=$o(attributes(no)) q:no=""  d
 . . . . s name=attributes(no)
 . . . . s valueNo="",count=0,stop=0
 . . . . f  s valueNo=$o(attributes(no,"value",valueNo)) q:valueNo=""  d  q:stop
 . . . . . s count=count+1 i count>1 s stop=1
 . . . . i count=1 d
 . . . . . s ^CacheTempEWD($j,lineNo)=comma_name_":",acomma="",comma=",",lineNo=lineNo+1
 . . . . e  d
 . . . . . s ^CacheTempEWD($j,lineNo)=comma_name_":[",acomma="",comma=",",lineNo=lineNo+1
 . . . . s valueNo=""
 . . . . f  s valueNo=$o(attributes(no,"value",valueNo)) q:valueNo=""  d
 . . . . . s value=attributes(no,"value",valueNo)
 . . . . . s quote="""" i $$numeric(value) s quote=""
 . . . . . s ^CacheTempEWD($j,lineNo)=acomma_quote_value_quote,lineNo=lineNo+1,acomma=","
 . . . . i count>1 s ^CacheTempEWD($j,lineNo)="]",lineNo=lineNo+1
 . . . s ^CacheTempEWD($j,lineNo)="}",lineNo=lineNo+1
 . . e  d
 . . . s ^CacheTempEWD($j,lineNo)="<GetAttributesResult>",lineNo=lineNo+1
 . . . s no=""
 . . . f  s no=$o(attributes(no)) q:no=""  d
 . . . . s name=attributes(no)
 . . . . s valueNo=""
 . . . . f  s valueNo=$o(attributes(no,"value",valueNo)) q:valueNo=""  d
 . . . . . s value=attributes(no,"value",valueNo)
 . . . . . s value=$$escape(value)
 . . . . . s ^CacheTempEWD($j,lineNo)="<Attribute>",lineNo=lineNo+1
 . . . . . s ^CacheTempEWD($j,lineNo)="<Name>"_name_"</Name>",lineNo=lineNo+1
 . . . . . s ^CacheTempEWD($j,lineNo)="<Value>"_value_"</Value>",lineNo=lineNo+1
 . . . . . s ^CacheTempEWD($j,lineNo)="</Attribute>",lineNo=lineNo+1
 . . . s ^CacheTempEWD($j,lineNo)="</GetAttributesResult>",lineNo=lineNo+1
 ;
 i action="Select" d
 . ;i $g(^zewd("trace"))=1 d trace($h_": action=Select")
 . i '$d(itemsAndAttrs) d  q
 . . s ^CacheTempEWD($j,lineNo)="<SelectResult />",lineNo=lineNo+1
 . e  d
 . . n attrName,attrNo,attrValue,attrValueNo,itemName,itemNo
 . . s ^CacheTempEWD($j,lineNo)="<SelectResult>",lineNo=lineNo+1
 . . s itemNo=""
 . . f  s itemNo=$o(itemsAndAttrs(itemNo)) q:itemNo=""  d
 . . . s itemName=itemsAndAttrs(itemNo)
 . . . s ^CacheTempEWD($j,lineNo)="<Item>",lineNo=lineNo+1
 . . . s ^CacheTempEWD($j,lineNo)="<Name>"_itemName_"</Name>",lineNo=lineNo+1
 . . . s attrNo=""
 . . . f  s attrNo=$o(itemsAndAttrs(itemNo,attrNo)) q:attrNo=""  d
 . . . . s attrName=itemsAndAttrs(itemNo,attrNo)
 . . . . s attrValueNo=""
 . . . . f  s attrValueNo=$o(itemsAndAttrs(itemNo,attrNo,"value",attrValueNo)) q:attrValueNo=""  d
 . . . . . s attrValue=itemsAndAttrs(itemNo,attrNo,"value",attrValueNo)
 . . . . . s attrValue=$$escape(attrValue)
 . . . . . s ^CacheTempEWD($j,lineNo)="<Attribute>",lineNo=lineNo+1
 . . . . . s ^CacheTempEWD($j,lineNo)="<Name>"_attrName_"</Name><Value>"_attrValue_"</Value>",lineNo=lineNo+1
 . . . . . s ^CacheTempEWD($j,lineNo)="</Attribute>",lineNo=lineNo+1
 . . . s ^CacheTempEWD($j,lineNo)="</Item>",lineNo=lineNo+1
 . . s ^CacheTempEWD($j,lineNo)="</SelectResult>",lineNo=lineNo+1
 . ;i $g(^zewd("trace"))=1 d trace($h_": finished action=Select")
 ;
 i action="Query" d
 . i '$d(itemList) d
 . . i $g(%KEY("OutputFormat"))="JSON" d
 . . . s ^CacheTempEWD($j,lineNo)="[]",lineNo=lineNo+1
 . . e  d
 . . . s ^CacheTempEWD($j,lineNo)="<QueryResult />",lineNo=lineNo+1
 . e  d
 . . n comma,position,quotes
 . . i $g(%KEY("OutputFormat"))="JSON" d
 . . . s ^CacheTempEWD($j,lineNo)="[",lineNo=lineNo+1
 . . . s position="",comma=""
 . . . f  s position=$o(itemList(position)) q:position=""  d
 . . . . s quotes="""" i $$numeric(itemList(position)) s quotes=""
 . . . . s ^CacheTempEWD($j,lineNo)=comma_quotes_itemList(position)_quotes,lineNo=lineNo+1,comma=","
 . . . s ^CacheTempEWD($j,lineNo)="]",lineNo=lineNo+1
 . . e  d
 . . . n position
 . . . s ^CacheTempEWD($j,lineNo)="<QueryResult>",lineNo=lineNo+1
 . . . s position=""
 . . . f  s position=$o(itemList(position)) q:position=""  d
 . . . . s ^CacheTempEWD($j,lineNo)="<ItemName>"_itemList(position)_"</ItemName>",lineNo=lineNo+1
 . . . i $g(nextToken)'="" s ^CacheTempEWD($j,lineNo)="<NextToken>"_nextToken_"</NextToken>",lineNo=lineNo+1
 . . . s ^CacheTempEWD($j,lineNo)="</QueryResult>",lineNo=lineNo+1
 ;
 i action="QueryWithAttributes" d
 . i '$d(itemsAndAttrs) d  q
 . . s ^CacheTempEWD($j,lineNo)="<QueryWithAttributesResult />",lineNo=lineNo+1
 . e  d
 . . n attrName,attrNo,attrValue,attrValueNo,itemName,itemNo
 . . s ^CacheTempEWD($j,lineNo)="<QueryWithAttributesResult>",lineNo=lineNo+1
 . . s itemNo=""
 . . f  s itemNo=$o(itemsAndAttrs(itemNo)) q:itemNo=""  d
 . . . s itemName=itemsAndAttrs(itemNo)
 . . . s ^CacheTempEWD($j,lineNo)="<Item>",lineNo=lineNo+1
 . . . s ^CacheTempEWD($j,lineNo)="<Name>"_itemName_"</Name>",lineNo=lineNo+1
 . . . s attrNo=""
 . . . f  s attrNo=$o(itemsAndAttrs(itemNo,attrNo)) q:attrNo=""  d
 . . . . s attrName=itemsAndAttrs(itemNo,attrNo)
 . . . . s attrValueNo=""
 . . . . f  s attrValueNo=$o(itemsAndAttrs(itemNo,attrNo,"value",attrValueNo)) q:attrValueNo=""  d
 . . . . . s attrValue=itemsAndAttrs(itemNo,attrNo,"value",attrValueNo)
 . . . . . s attrValue=$$escape(attrValue)
 . . . . . s ^CacheTempEWD($j,lineNo)="<Attribute>",lineNo=lineNo+1
 . . . . . s ^CacheTempEWD($j,lineNo)="<Name>"_attrName_"</Name><Value>"_attrValue_"</Value>",lineNo=lineNo+1
 . . . . . s ^CacheTempEWD($j,lineNo)="</Attribute>",lineNo=lineNo+1
 . . . s ^CacheTempEWD($j,lineNo)="</Item>",lineNo=lineNo+1
 . . s ^CacheTempEWD($j,lineNo)="</QueryWithAttributesResult>",lineNo=lineNo+1
 ;
 i $g(%KEY("db"))="mdb",$g(%KEY("OutputFormat"))'="JSON" d
 . s ^CacheTempEWD($j,lineNo)="<ResponseMetadata>",lineNo=lineNo+1
 . s ^CacheTempEWD($j,lineNo)="<RequestId>"_$g(requestId)_"</RequestId>",lineNo=lineNo+1
 . s ^CacheTempEWD($j,lineNo)="<BoxUsage>"_$g(boxUsage)_"</BoxUsage>",lineNo=lineNo+1
 . s ^CacheTempEWD($j,lineNo)="</ResponseMetadata>",lineNo=lineNo+1
 i $g(%KEY("mdbRawOutput"))'="true",$g(%KEY("OutputFormat"))'="JSON" d
 . s ^CacheTempEWD($j,lineNo)="</"_action_"Response>"_$c(13,10),lineNo=lineNo+1
 ;
 s len=0,lineNo=""
 f  s lineNo=$o(^CacheTempEWD($j,lineNo)) q:lineNo=""  d
 . s len=len+$l(^CacheTempEWD($j,lineNo))
 i '$d(%KEY("isCSP")) w "Content-length: "_len_$c(13,10,13,10) 
 s lineNo=""
 f  s lineNo=$o(^CacheTempEWD($j,lineNo)) q:lineNo=""  d
 . w ^CacheTempEWD($j,lineNo)
 k ^CacheTempEWD($j)
 w !
 ;i $g(^zewd("trace"))=1 d trace($h_": finished createResponse")
 QUIT
 ;
errorResponse(ec,em)
 ;
 n resp
 ;
 i $g(%KEY("OutputFormat"))="JSON" d  QUIT
 . s resp="400 Bad Request"
 . i ec="SignatureDoesNotMatch" s resp="403 Forbidden"
 . w "HTTP/1.0 "_resp_$c(13,10)
 . w "Date: "_$$inetDate^%zewdAPI($h)_" "_$tr($g(^MDBConfig("GMTOffset")),":","")_$c(13,10)
 . w "Content-type: application/json"_$c(13,10),$c(13,10)
 . w "{""ErrorCode"":"""_ec_""",""ErrorMessage"":"""_em_"""}"_$c(13,10)
 . w !
 ;
 s resp="400 Bad Request"
 i ec="SignatureDoesNotMatch" s resp="403 Forbidden"
 w "HTTP/1.0 "_resp_$c(13,10)
 w "Date: "_$$inetDate^%zewdAPI($h)_" "_$tr($g(^MDBConfig("GMTOffset")),":","")_$c(13,10)
 w "Content-type: text/xml"_$c(13,10),$c(13,10)
 w "<?xml version='1.0'?>"_$c(13,10)
 w "<Response><Errors><Error>"
 w "<Code>"_ec_"</Code>"
 w "<Message>"_em_"</Message>"
 i ec'="SignatureDoesNotMatch",ec'="InvalidClientTokenId" w "<BoxUsage>0</BoxUsage>"
 w "</Error></Errors>"
 ; Note mis-spelling of ID instead of Id to follow SimpleDB's "feature"!
 w "<RequestID>"_$$createRequestId()_"</RequestID>"
 w "</Response>"_$c(13,10)
 w !
 QUIT
 ;
createResponseStringToSign(version)
 ;
 n amp,n,name,nlc,stringToSign,nvpListlc,value
 ;
 s stringToSign=""
 ;
 i version=0 d  QUIT stringToSign
 . s stringToSign=$g(%KEY("Action"))_$g(%KEY("Timestamp"))
 ;
 i version=1 d  QUIT stringToSign
 . s n=""
 . f  s n=$o(%KEY(n)) q:n=""  d
 . . q:$e(n,1,3)="MGW"
 . . q:n="Signature"
 . . q:n="isCSP"
 . . s nvpListlc($zconvert(n,"l"))=n
 . . ;i $zv["GT.M" d
 . . ;. s nvpListlc($zconvert(n,"l"))=n
 . . ;e  d
 . . ;. s nvpListlc($zconvert(n,"l"))=n
 . s nlc=""
 . f  s nlc=$o(nvpListlc(nlc)) q:nlc=""  d
 . . s name=nvpListlc(nlc)
 . . s value=%KEY(name)
 . . s stringToSign=stringToSign_name_value
 ;
 i version=2 d  QUIT stringToSign
 . n location,method,url
 . s name="",amp=""
 . f  s name=$o(%KEY(name)) q:name=""  d
 . . q:$e(name,1,3)="MGW"
 . . q:name="Signature"
 . . q:name="isCSP"
 . . s value=$$urlEscape(%KEY(name))
 . . s stringToSign=stringToSign_amp_name_"="_value
 . . s amp="&"
 . s method=$g(%CGIEVAR("REQUEST_METHOD"))
 . s url=$g(%CGIEVAR("SERVER_NAME"))
 . s location=$g(%CGIEVAR("REQUEST_URI"))
 . i location["?" s location=$p(location,"?",1)
 . i location="" d
 . . s location="/scripts/mgwms32.dll"
 . . i $d(%KEY("isCSP")) s location=$$baseUri^MDBMCache()
 . e  d
 . . i location["http://"!(location["https://") d
 . . . s location=$p(location,"://",2)
 . . . s location="/"_$p(location,"/",2,2000)
 . . . s location=$p(location,"?",1)
 . s stringToSign=method_$c(10)_url_$c(10)_location_$c(10)_stringToSign
 . ;s stringToSign=$$replaceAll(stringToSign,$c(13,10),"")
 ;
 QUIT stringToSign
 ;
createToken(length)
 ;
 n i,string,token
 ;
 s string="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890"
 s token=""
 f i=1:1:length s token=token_$e(string,($r($l(string))+1))
 QUIT token_"="
 ;
init(startTime)
 ;
 i $g(^zewd("trace")) d trace($h_": $$init")
 i $zv["GT.M" d
 . s startTime=$$ZTS^%ZMGWSIS(1)
 e  d
 . s startTime=$zts
 s startTime=(startTime*86400)+$p(startTime,",",2)
 QUIT $$createRequestId()
 ;
createRequestId()
 n hex,i,responseId
 s responseId="" 
 f i=1:1:16 d
 . s hex=$$hex($r(256))
 . i $l(hex)=1 s hex=0_hex
 . s responseId=responseId_hex
 . i i=4!(i=6)!(i=8)!(i=10) s responseId=responseId_"-"
 QUIT $zconvert(responseId,"l")
 ;i $zv["GT.M" QUIT $zconvert(responseId,"l")
 ;QUIT $zconvert(responseId,"l")
 
end(startTime,boxUsage,errorCode,parameter1,parameter2)
 ;
 n endTime,error
 ;
 i $g(^zewd("trace")) d trace($h_": $$end")
 i $zv["GT.M" d
 . s endTime=$$ZTS^%ZMGWSIS(1)
 e  d
 . s endTime=$zts
 s endTime=(endTime*86400)+$p(endTime,",",2)
 s boxUsage=endTime-startTime,boxUsage=$j(boxUsage,1,10)
 i $g(errorCode)="" QUIT ""
 s error=$g(^MDBErrors(errorCode))
 i error="" QUIT errorCode_"~"_$g(parameter1)
 i $g(parameter1)'="" s $p(error,"~",2)=parameter1
 i $g(parameter2)'="" s $p(error,"~",4)=parameter2
 QUIT errorCode_"~"_$tr(error,"~","")
 ;
getSignedString(string,secretKey,signatureMethod)
 ;
 n context,hash,returnValue
 ;
 i $zv["GT.M" d  QUIT returnValue
 . s context=1
 . i $d(^zewd("config","MGWSI")) s context=0
 . i $zconvert($g(signatureMethod),"l")="hmacsha256" d
 . . s returnValue=$$HMACSHA256^%ZMGWSIS(string,secretKey,1,context)
 . e  d
 . . s returnValue=$$HMACSHA1^%ZMGWSIS(string,secretKey,1,context)
 ;
 QUIT $$sign^MDBMCache(signatureMethod,string,secretKey)
 ;
runQuery(keyId,domainName,queryExpression,nextToken,itemList)
 ;
 n domainId,error,filter,itemId,itemName,matchList,name,no,pos,stop
 ;
 ;i $g(^zewd("trace")) d trace($h_": runQuery started")
 s error=""
 s filter="",stop=0
 i queryExpression["itemName{}" s queryExpression=$$replaceAll(queryExpression,"itemName{}","itemName()")
 s queryExpression=$$stripSpaces($g(queryExpression))
 s domainId=$$getDomainId(keyId,domainName)
 i $g(^zewd("trace"))=1 d trace("runQuery: domainId="_domainId_"; queryExpression="_queryExpression)
 i domainId="" QUIT ""
 ;
 i queryExpression="" d  QUIT error
 . s itemId="",no=0
 . f  s itemId=$o(^MDB(keyId,"domains",domainId,"items",itemId)) q:itemId=""  d
 . . s no=no+1
 . . s itemList(no)=^MDB(keyId,"domains",domainId,"items",itemId)
 ;
 i queryExpression["sort " d  i error'="" QUIT error
 . i queryExpression["union "!(queryExpression["union[")!(queryExpression["not ")!(queryExpression["not[") s error=$$queryError(1)
 ;
 f  d  q:stop  q:queryExpression=""  q:error'=""
 . k matchList
 . s error=$$queryPredicate(.queryExpression,keyId,domainId,.matchList,.name)
 . q:error'=""
 . i filter="" d mergeLists(.itemList,.matchList) ;m itemList=matchList
 . i filter="intersection" d
 . . s pos=""
 . . f  s pos=$o(itemList(pos)) q:pos=""  d
 . . . s itemName=itemList(pos)
 . . . i '$$inMatchList(.matchList,itemName) k itemList(pos)
 . . . ;i '$d(matchList(itemId)) k itemList(itemId)
 . i filter="union" d mergeLists(.itemList,.matchList) ;m itemList=matchList
 . q:queryExpression=""
 . s filter=""
 . i $$startsWith(queryExpression,"intersection") d  q
 . . s queryExpression=$e(queryExpression,13,$l(queryExpression))
 . . s queryExpression=$$stripSpaces(queryExpression)
 . . s filter="intersection"
 . i $$startsWith(queryExpression,"union") d  q
 . . s queryExpression=$e(queryExpression,6,$l(queryExpression))
 . . s queryExpression=$$stripSpaces(queryExpression)
 . . s filter="union"
 . i $$startsWith(queryExpression,"sort") s stop=1 q
 . s error=$$queryError(2)
 ;
 i $$startsWith(queryExpression,"sort") d
 . n attrNo,direction,found,itemName,itemId,n,sortAttr,sortAttrId,sortedList,xvalue
 . s queryExpression=$e(queryExpression,5,$l(queryExpression))
 . s queryExpression=$$stripSpaces(queryExpression)
 . s sortAttr=$p(queryExpression,"'",2)
 . i sortAttr="" s error=$$queryError(3) q
 . s sortAttrId=$$getAttributeId(keyId,domainId,sortAttr)
 . i sortAttrId="" s error=$$queryError(4) q
 . i $g(name)'="",name'=sortAttr s error=$$queryError(5) q
 . i $g(name)="" d  q:error'=""
 . . n no,ok
 . . s no="",ok=0
 . . f  s no=$o(name(no)) q:no=""  d  q:ok
 . . . i name(no)=sortAttr s ok=1 q
 . . i 'ok s error=$$queryError(6)
 . s direction="1"
 . s queryExpression=$p(queryExpression,"'",3)
 . i queryExpression["desc" s direction="-1"
 . s pos=""
 . ;f  s pos=$o(itemList(pos)) q:pos=""  d
 . ;. s itemName=itemList(pos)
 . ;. s itemNamex=$e(itemName,1,$$indexLength())
 . ;. s sortedList(itemNamex,pos)=itemName
 . s pos=""
 . f  s pos=$o(itemList(pos)) q:pos=""  d
 . . s itemName=itemList(pos)
 . . s itemId=$$getItemId(keyId,domainId,itemName)
 . . s attrNo=""
 . . f  s attrNo=$o(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",sortAttrId,"value",attrNo)) q:attrNo=""  d
 . . . s xvalue=^MDB(keyId,"domains",domainId,"items",itemId,"attribs",sortAttrId,"value",attrNo)
 . . . s sortedList(xvalue,itemName)=""
 . k itemList
 . s n=0
 . s xvalue=""
 . f  s xvalue=$o(sortedList(xvalue),direction) q:xvalue=""  d
 . . s itemName=""
 . . f  s itemName=$o(sortedList(xvalue,itemName)) q:itemName=""  d
 . . . i $d(found(itemName)) q
 . . . s n=n+1
 . . . s itemList(n)=itemName
 . . . s found(itemName)=""
 ;i $g(^zewd("trace")) d trace($h_": runQuery ended")
 QUIT error
 ;
inMatchList(matchList,itemName)
 ;
 n pos,present
 ;
 s pos="",present=0
 f  s pos=$o(matchList(pos)) q:pos=""  d  q:present
 . i matchList(pos)=itemName s present=1
 QUIT present
 ;
 ;
mergeLists(toList,fromList)
 ;
 n index,itemName,maxPos,newPos,pos,posx,stop,toPos
 ;
 s pos=""
 f  s pos=$o(toList(pos)) q:pos=""  d
 . s itemName=toList(pos)
 . i itemName="" s itemName=" "
 . s index(itemName)=pos
 ;
 s pos="",maxPos=$o(toList(""),-1)
 f  s pos=$o(fromList(pos)) q:pos=""  d
 . s itemName=fromList(pos)
 . ;s toPos="",stop=0
 . ;f  s toPos=$o(toList(toPos)) q:toPos=maxPos  q:toPos=""  d  q:stop
 . ;. i toList(toPos)=itemName s stop=1
 . ;i 'stop d
 . s posx=$g(index(itemName))
 . i posx="" d
 . . s newPos=$o(toList(""),-1)+1
 . . s toList(newPos)=itemName
 . . s index(itemName)=newPos
 QUIT
 ;
queryPredicate(query,keyId,domainId,itemList,name)
 n c,compOp,error,i,inString,no,not,predicate,relation,stop,value
 ;
 ;d trace("in queryPredicate - query="_query)
 d
 . n queryx
 . s queryx=$$replaceAll(query,"\\",$c(3))
 . i queryx["\'" s query=$$replaceAll(queryx,"\'",$c(1))
 i query["''" s query=$$replaceAll(query,"''",$c(1))
 i query[$c(32,1,39) s query=$$replaceAll(query,$c(32,1,39),$c(32,39,1))
 i query[$c(61,1,39) s query=$$replaceAll(query,$c(61,1,39),$c(61,39,1))
 i query[$c(2) s query=$$replaceAll(query,$c(2),$c(1))
 s error="",predicate=""
 s not=$$queryNot(.query)
 i $e(query,1)'="[" QUIT $$queryError(7)
 s inString=0
 s query=$e(query,2,$l(query))
 s stop=0
 ;d trace("1 query="_query)
 f i=1:1:$l(query) d  q:stop
 . s c=$e(query,i)
 . i c="'" s inString='inString
 . i c="]",'inString d  q
 . . s predicate=$e(query,1,i-1)
 . . s query=$e(query,i+1,$l(query))
 . . s stop=1
 ;d trace("predicate="_predicate)
 i predicate="" QUIT $$queryError(8)
 s query=$$stripSpaces(query)
 ;d trace("query="_query)
 s error=$$parsePredicate(.predicate,.name,.compOp,.value)
 i error'="" QUIT error
 s no=1
 ;d trace("predicate="_predicate)
 i predicate'="" d  i error'="" QUIT error
 . ;d trace("name="_name)
 . s name(1)=name
 . ;d trace("compOp="_compOp)
 . s compOp(1)=compOp
 . ;d trace("value="_value)
 . i value="" s value=$c(31)
 . s value(1)=value
 . f  q:predicate=""  d  q:error'=""
 . . i $e(predicate,1,3)="and" d  q
 . . . s predicate=$e(predicate,4,$l(predicate))
 . . . s error=$$parseSubPredicate(.predicate,.name,.compOp,.value,.no)
 . . . s relation(no)="&"
 . . i $e(predicate,1,2)="or" d  q
 . . . s predicate=$e(predicate,3,$l(predicate))
 . . . s error=$$parseSubPredicate(.predicate,.name,.compOp,.value,.no)
 . . . s relation(no)="!"
 ;d trace("about to run executeQuery")
 d executeQuery(keyId,domainId,no,.name,.compOp,.value,.relation,.itemList)
 i not d not(.itemList)
 QUIT error
 ;
parseSubPredicate(predicate,name,compOp,value,no)
 n error
 ;
 s predicate=$$stripSpaces(predicate)
 s error=$$parsePredicate(.predicate,.name,.compOp,.value)
 i error'="" QUIT error
 i name'=name(1) QUIT $$queryError(9)
 i value="" s value=$c(31)
 s no=no+1
 s name(no)=name,compOp(no)=compOp,value(no)=value
 QUIT ""
 ;
parsePredicate(predicate,name,compOp,value)
 n c1,cx,error
 ;
 ;d trace("in parsePredicate - predicate="_predicate)
 s predicate=$$stripSpaces(predicate)
 s c1=$e(predicate,1)
 ;d trace("c1="_c1)
 i c1'="'" QUIT $$queryError(10)
 s cx=$e(predicate,$l(predicate))
 ;d trace("cx="_cx)
 i cx'="'",cx'="""" QUIT $$queryError(11)
 ;d trace("passed 1")
 ;d trace("111 predicate="_predicate)
 s error=$$getIdentifier(.predicate,.name)
 ;d trace("name="_name)
 i error'="" QUIT error
 i name="" QUIT $$queryError(13)
 s error=$$getCompOp(.predicate,.compOp)
 i error'="" QUIT error
 ;d trace("222 predicate="_predicate)
 s error=$$getIdentifier(.predicate,.value)
 ;d trace("222 value="_value)
 i value="" s value=$c(31)
 i error'="",compOp="starts-with",value="" s error=""
 QUIT error
 ;
getIdentifier(predicate,identifier)
 ;
 n c,c1,cx,error,escaped,i
 ;
 s error=""
 s identifier=""
 s escaped=0
 s c1=$e(predicate,1)
 ;d trace("in getIdentifier: c1="_c1)
 i c1'="'",c1'="""" QUIT $$queryError(12)
 s cx=c1
 f i=2:1:$l(predicate) d  q:identifier'=""
 . s c=$e(predicate,i)
 . i c="\",'escaped s escaped=1 q
 . i 'escaped,c=cx d  q
 . . s identifier=$e(predicate,2,i-1)
 . . s predicate=$e(predicate,i+1,$l(predicate))
 . . s predicate=$$stripSpaces(predicate)
 . i escaped s escaped=0
 i identifier["\\" d
 . s identifier=$$replaceAll(identifier,"\\",$c(5))
 . s identifier=$$replaceAll(identifier,$c(5),"\")
 ;d trace("predicate="_predicate_"; identifier="_identifier)
 ;i identifier="" QUIT $$queryError(13)
 QUIT error
 ;
getCompOp(predicate,compOp)
 ;
 n error
 ;
 s error=""
 i $e(predicate,1)="=" d  QUIT ""
 . s compOp="="
 . s predicate=$e(predicate,2,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 i $e(predicate,1,2)="<=" d  QUIT ""
 . s compOp="<="
 . s predicate=$e(predicate,3,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 i $e(predicate,1,2)=">=" d  QUIT ""
 . s compOp=">="
 . s predicate=$e(predicate,3,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 i $e(predicate,1,2)="!=" d  QUIT ""
 . s compOp="!="
 . s predicate=$e(predicate,3,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 i $e(predicate,1)="<" d  QUIT ""
 . s compOp="<"
 . s predicate=$e(predicate,2,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 i $e(predicate,1)=">" d  QUIT ""
 . s compOp=">"
 . s predicate=$e(predicate,2,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 i $e(predicate,1,11)="starts-with" d  QUIT ""
 . s compOp="starts-with"
 . s predicate=$e(predicate,12,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 i $e(predicate,1,4)="like" d  QUIT ""
 . s compOp="like"
 . s predicate=$e(predicate,5,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 i $e(predicate,1,8)="not like" d  QUIT ""
 . s compOp="notlike"
 . s predicate=$e(predicate,9,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 i $e(predicate,1,19)="does-not-start-with" d  QUIT ""
 . s compOp="does-not-start-with"
 . s predicate=$e(predicate,20,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 i $e(predicate,1,6)="isNull" d  QUIT ""
 . s compOp="isNull"
 . s predicate=$e(predicate,7,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 i $e(predicate,1,9)="isNotNull" d  QUIT ""
 . s compOp="isNotNull"
 . s predicate=$e(predicate,10,$l(predicate))
 . s predicate=$$stripSpaces(predicate)
 QUIT $$queryError(14)
 ;
queryNot(query)
 n not
 s not=0
 i $e(query,1,3)="not" d
 . s not=1
 . s query=$e(query,4,$l(query))
 . s query=$$stripSpaces(query)
 QUIT not
 ;
queryError(x)
 i x=5 QUIT "InvalidSortExpression~Invalid sort expression. The sort attribute must be present in at least one of the predicates, and the predicate cannot contain the is null operator."
 QUIT "InvalidQueryExpression~The specified query expression syntax is not valid ("_x_")"
 ;
not(itemList)
 ;
 n itemId,itemName,pos,selected
 ;
 s pos=""
 f  s pos=$o(itemList(pos)) q:pos=""  d
 . s itemName=itemList(pos)
 . s itemId=$$getItemId(keyId,domainId,itemName)
 . s selected(itemId)=""
 k itemList
 s itemId="",pos=0
 f  s itemId=$o(^MDB(keyId,"domains",domainId,"items",itemId)) q:itemId=""  d
 . i '$d(selected(itemId)) d
 . . s pos=pos+1
 . . s itemList(pos)=^MDB(keyId,"domains",domainId,"items",itemId)
 ;
 QUIT
 ;
executeQuery(keyId,domainId,no,name,compOp,value,relation,itemList)
 ;
 n attribId,expr,func,itemId,itemName,pos,stop,xvalue
 ;
 s attribId=""
 ;d trace("xx keyId="_keyId_"; domainId="_domainId_"; name="_name_": no="_no)
 i $g(name)'="" d
 . i $e(name,1)="`",$e(name,$l(name))="`" s name=$e(name,2,$l(name)-1)
 . i name["``" s name=$$replace(name,"``","`")
 . s attribId=$$getAttributeId(keyId,domainId,name)
 ;d trace("xx attribId="_attribId)
 i attribId="",compOp'="isNull" QUIT
 ;
 ;d trace("name="_name_"; value="_value_"; compOp="_compOp)
 i value[$c(1) s value=$tr(value,$c(1),"'")
 i name[$c(1) s name=$tr(name,$c(1),"'")
 i no=1,compOp="=" d  QUIT
 . s pos=0,itemId=""
 . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,value,itemId)) q:itemId=""  d
 . . s pos=pos+1
 . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 . . ;d trace(value_" found for itemId="_itemId)
 ;
 i no=1,compOp="!=" d  QUIT
 . s xvalue="",pos=0
 . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d
 . . i value=xvalue q
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp="<" d  QUIT
 . s xvalue="",stop=0,pos=0
 . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue'<value  q:xvalue=""  d  q:stop
 . . i $e(xvalue,1)'?1N s stop=1 q
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp="<=" d  QUIT
 . s xvalue="",stop=0,pos=0
 . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue>value  q:xvalue=""  d  q:stop
 . . i $e(xvalue,1)'?1N s stop=1 q
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp=">" d  QUIT
 . s xvalue=value,stop=0,pos=0
 . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d  q:stop
 . . i $e(xvalue,1)'?1N s stop=1 q
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp=">=" d  QUIT
 . s xvalue=value-1,stop=0,pos=0
 . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d  q:stop
 . . i $e(xvalue,1)'?1N s stop=1 q
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp="isNull" d  QUIT
 . s itemId="",pos=0
 . f  s itemId=$o(^MDB(keyId,"domains",domainId,"items",itemId)) q:itemId=""  d
 . . i attribId'="",$d(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId)) q
 . . s pos=pos+1
 . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp="isNotNull" d  QUIT
 . s itemId="",pos=0
 . f  s itemId=$o(^MDB(keyId,"domains",domainId,"items",itemId)) q:itemId=""  d
 . . i '$d(^MDB(keyId,"domains",domainId,"items",itemId,"attribs",attribId)) q
 . . s pos=pos+1
 . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d
 . . i $e(xvalue,1,len)=value q
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp="like" d
 . i value["\%" s value=$$replaceAll(value,"\%",$c(6))
 . i $e(value,1)="%",$e(value,$l(value))="%" d  q
 . . s value=$e(value,2,$l(value)-1)
 . . s compOp="contains"
 . i $e(value,$l(value))="%" d  q
 . . s value=$e(value,1,$l(value)-1)
 . . s compOp="starts-with"
 . i $e(value,1)="%" d  q
 . . s value=$e(value,2,$l(value))
 . . s compOp="ends-with"
 i no=1,compOp="notlike" d
 . i value["\%" s value=$$replaceAll(value,"\%",$c(6))
 . i $e(value,1)="%",$e(value,$l(value))="%" d  q
 . . s value=$e(value,2,$l(value)-1)
 . . s compOp="does-not-contain"
 . i $e(value,$l(value))="%" d  q
 . . s value=$e(value,1,$l(value)-1)
 . . s compOp="does-not-start-with"
 . i $e(value,1)="%" d  q
 . . s value=$e(value,2,$l(value))
 . . s compOp="does-not-end-with"
 . ;s compOp="does-not-start-with"
 . ;i $e(value,$l(value))="%" s value=$e(value,1,$l(value)-1)
 i no=1 s value=$$replaceAll(value,$c(6),"%")
 i no=1,compOp="starts-with" d  QUIT
 . i value=$c(31) s value=""
 . i $l(value)=1 d
 . . s xvalue=$c($a(value)-1)
 . e  d
 . . n c
 . . q:value=""
 . . s c=$e(value,$l(value))
 . . s c=$c($a(c)-1)
 . . s xvalue=$e(value,1,$l(value)-1)_c
 . s stop=0,pos=0
 . i value?1N.N d
 . . n len
 . . s xvalue=value-1
 . . s len=$l(value)
 . . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d
 . . . i $e(xvalue,1,len)'=value q
 . . . s itemId=""
 . . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . . s pos=pos+1
 . . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 . e  d
 . . n len
 . . i value'="" d
 . . . s xvalue=xvalue_"~"
 . . . s len=$l(value)
 . . e  d
 . . . s xvalue=""
 . . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d  q:stop
 . . . i value'="",$e(xvalue,1,len)'=value s stop=1 q
 . . . s itemId=""
 . . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . . s pos=pos+1
 . . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp="does-not-start-with" d  QUIT
 . n len
 . i value=$c(31) q
 . s xvalue="",len=$l(value),pos=0
 . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d
 . . i $e(xvalue,1,len)=value q
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp="contains" d  QUIT
 . n len
 . i value=$c(31) q
 . s xvalue="",pos=0
 . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d
 . . i xvalue'[value q
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp="does-not-contain" d  QUIT
 . n len
 . i value=$c(31) q
 . s xvalue="",pos=0
 . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d
 . . i xvalue[value q
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp="ends-with" d  QUIT
 . n len,xend,xlen
 . i value=$c(31) q
 . s xvalue="",len=$l(value),pos=0
 . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d
 . . s xlen=$l(xvalue)
 . . s xend=$e(xvalue,(xlen-len+1),xlen)
 . . i xend'=value q
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1,compOp="does-not-end-with" d  QUIT
 . n len,xend,xlen
 . i value=$c(31) q
 . s xvalue="",len=$l(value),pos=0
 . f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d
 . . s xlen=$l(xvalue)
 . . s xend=$e(xvalue,(xlen-len+1),xlen)
 . . i xend=value q
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId))
 ;
 i no=1 QUIT
 s expr=""
 f i=1:1:no d
 . i i>1 s expr=expr_relation(i)
 . d
 . . i compOp(i)="=" s func="equals" q
 . . i compOp(i)="<" s func="lt" q
 . . i compOp(i)=">" s func="gt" q
 . . i compOp(i)="!=" s func="notEquals" q
 . . i compOp(i)="<=" s func="le" q
 . . i compOp(i)=">=" s func="ge" q
 . . i compOp(i)="starts-with" s func="startsWith" q
 . . i compOp(i)="notlike" d  q
 . . . i $e(value(i),1)="%",$e(value(i),$l(value(i)))="%" d  q
 . . . . s value(i)=$e(value(i),2,$l(value(i))-1)
 . . . . s func="notContains"
 . . . i $e(value(i),$l(value(i)))="%" d  q
 . . . . s value(i)=$e(value(i),1,$l(value(i))-1)
 . . . . s func="notStartsWith"
 . . . i $e(value(i),1)="%" d  q
 . . . . s value(i)=$e(value(i),2,$l(value(i)))
 . . . . s func="notEndsWith"
 . . . ;s func="notStartsWith"
 . . . ;i $e(value(i),$l(value(i)))="%" s value(i)=$e(value(i),1,$l(value(i))-1)
 . . i compOp(i)="like" d  q
 . . . i $e(value(i),1)="%",$e(value(i),$l(value(i)))="%" d  q
 . . . . s value(i)=$e(value(i),2,$l(value(i))-1)
 . . . . s func="contains"
 . . . i $e(value(i),$l(value(i)))="%" d  q
 . . . . s value(i)=$e(value(i),1,$l(value(i))-1)
 . . . . s func="startsWith"
 . . . i $e(value(i),1)="%" d  q
 . . . . s value(i)=$e(value(i),2,$l(value(i)))
 . . . . s func="endsWith"
 . . . ;s func="startsWith"
 . . . ;i $e(value(i),$l(value(i)))="%" s value(i)=$e(value(i),1,$l(value(i))-1)
 . . i compOp(i)="does-not-start-with" s func="notStartsWith"
 . s value(i)=$$replaceAll(value(i),$c(6),"%")
 . s expr=expr_"$$"_func_"(xvalue,"""_value(i)_""")"
 . ;d trace("expr="_expr)
 s xvalue=""
 s attribId=$$getAttributeId(keyId,domainId,name(1))
 s pos=0
 f  s xvalue=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue)) q:xvalue=""  d
 . i @expr d
 . . s itemId=""
 . . f  s itemId=$o(^MDB(keyId,"domains",domainId,"queryIndex",attribId,xvalue,itemId)) q:itemId=""  d
 . . . s pos=pos+1
 . . . s itemList(pos)=$g(^MDB(keyId,"domains",domainId,"items",itemId)) 
 QUIT
 ;
equals(indexValue,targetValue)
 QUIT indexValue=targetValue
 ;
notEquals(indexValue,targetValue)
 QUIT indexValue'=targetValue
 ;
lt(indexValue,targetValue)
 i $e(indexValue,1)'?1N QUIT 0
 QUIT indexValue<targetValue
 ;
gt(indexValue,targetValue)
 i $e(indexValue,1)'?1N QUIT 0
 QUIT indexValue>targetValue
 ;
le(indexValue,targetValue)
 i $e(indexValue,1)'?1N QUIT 0
 QUIT indexValue'>targetValue
 ;
ge(indexValue,targetValue)
 i $e(indexValue,1)'?1N QUIT 0
 QUIT indexValue'<targetValue
 ;
startsWith(index,targetValue)
 QUIT $e(index,1,$l(targetValue))=targetValue
 ;
notStartsWith(index,targetValue)
 QUIT $e(index,1,$l(targetValue))'=targetValue
 ;
contains(indexValue,targetValue)
 QUIT indexValue[targetValue
 ;
notContains(indexValue,targetValue)
 QUIT indexValue'[targetValue
 ;
endsWith(indexValue,targetValue)
 n ilen,tlen,end
 s ilen=$l(indexValue)
 s tlen=$l(targetValue)
 s end=$e(indexValue,(ilen-tlen+1),ilen)
 QUIT end=targetValue
 ;
notEndsWith(indexValue,targetValue)
 n ilen,tlen,end
 s ilen=$l(indexValue)
 s tlen=$l(targetValue)
 s end=$e(indexValue,(ilen-tlen+1),ilen)
 QUIT end'=targetValue
 ;
hex(number)
 n hex,no,str
 s hex=""
 s str="123456789ABCDEF"
 f  d  q:number=0
 . s no=number#16
 . s number=number\16
 . i no s no=$e(str,no)
 . s hex=no_hex
 QUIT hex
 ;
hexDecode(hex)
 QUIT $f("0123456789ABCDEF",hex)-2
 
hexToDecimal(hex)
 ;
 n i,num
 s num=0
 f i=1:1:$l(hex) d
 . s num=num*16+$$hexDecode($e(hex,i))
 QUIT num
 ;
urlDecode(string)
 ;
 n ascii,c,hex,pos
 ;
 i string["%25" s string=$$replaceAll(string,"%25",$c(1))
 i string["+" s string=$$replaceAll(string,"+","%20")
 f  q:string'["%"  d
 . s pos=$f(string,"%")
 . s c=$e(string,pos) s c=$zconvert(c,"l") i "0123456789abcdef"'[c s string=$$replace(string,"%",$c(1)) q
 . s c=$e(string,pos+1) s c=$zconvert(c,"l") i "0123456789abcdef"'[c s string=$$replace(string,"%",$c(1)) q
 . s hex=$e(string,pos,pos+1)
 . i $l(hex)'=2 s string=$$replace(string,"%",$c(1)) q
 . s ascii=$$hexToDecimal(hex)
 . s string=$e(string,1,pos-2)_$c(ascii)_$e(string,pos+2,$l(string))
 i string[$c(1) s string=$$replaceAll(string,$c(1),"%")
 QUIT string
 ;
urlEscape(string)
 ;The unreserved characters are A-Z, a-z, 0-9, hyphen ( - ), underscore ( _ ), period ( . ), and tilde ( ~ ). 
 i string?1AN.AN QUIT string
 n a,c,esc,i,pass
 f i=45,46,95,126 s pass(i)=""
 s esc=""
 f i=1:1:$l(string) d
 . s c=$e(string,i)
 . i c?1AN s esc=esc_c q
 . s a=$a(c)
 . i $d(pass(a)) s esc=esc_c q
 . s a=$$hex^MDB(a)
 . s esc=esc_"%"_$zconvert(a,"u")
 QUIT esc
 ;
installMDBX(requestId,boxUsage) ; Install M/DB:X Extensions
 n error,startTime
 s requestId=$$init(.startTime)
 s error=""
 i $t(MDBX^MDBX)'="" d
 . d install^MDBX
 e  d
 . s error="The M/DB:X routine is not available.  Download and install it and try again"
 i error'="" QUIT error
 QUIT $$end(startTime,.boxUsage)
 ;
installMDBM(requestId,boxUsage) ; Install M/DB:M(umps) Extensions
 n error,startTime
 s requestId=$$init(.startTime)
 s error=""
 i $t(MDBMumps^MDBMumps)'="" d
 . d install^MDBMumps
 e  d
 . s error="The M/DB:M routine is not available.  Download and install it and try again"
 i error'="" QUIT error
 QUIT $$end(startTime,.boxUsage)
 ;
initialise(requestId,boxUsage) ; Initialise the M/DB database
	n configFile,i,io,key,line,ok,secret,startTime,stop
	s requestId=$$init(.startTime)
	s io=$io
	s configFile="/usr/MDB/MDB.conf"
	c configFile
	o configFile:(readonly:exception="g configFileNotExists")
	u configFile
	s stop=0,key="",secret=""
	f i=1:1 r line d  q:stop
	. i $e(line,1)="#" q
	. i line["AdminstratorAccessKeyId"!(line["AdministratorAccessKeyId") d  q
	. . s key=$p(line,"=",2)
	. . s key=$$stripSpaces(key)
	. . s key=$$replaceAll(key,$c(9),"")
	. . i $e(key,1)="<" s key=""  ; user hasn't changed the explanatory text
	. i line["AdminstratorSecretKey"!(line["AdministratorSecretKey") d  q
	. . s secret=$p(line,"=",2)
	. . s secret=$$stripSpaces(secret)
	. . s secret=$$replaceAll(secret,$c(9),"")
	. . i $e(secret,1)="<" s secret=""  ; user hasn't changed the explanatory text
	. . s stop=1
	c configFile
	u io
	i key'="",secret'="" d  QUIT $$end(startTime,.boxUsage)
	. i $$createAdministrator^MDBConfig(key,secret)
	. i $$reset^MDBConfig(key,secret)
	QUIT "The contents of the Configuration File /usr/MDB/MDB.conf were invalid"
	;
configFileNotExists	
	c configFile
	u io
	QUIT "Configuration File /usr/MDB/MDB.conf was not found"
	;
initialisationResponse
	QUIT
	;
decodeBase64(string)
 ;
 n b64,context
 ;
 i $zv["GT.M" d  QUIT b64
 . s context=1
 . i $d(^zewd("config","MGWSI")) s context=0
 . s b64=$$DB64^%ZMGWSIS(string,context)
 ;
 QUIT $$b64Decode^MDBMCache(string)
 ;
encodeBase64(string)
 ;
 n b64,context
 ;
 i $zv["GT.M" d  QUIT b64
 . s context=1
 . i $d(^zewd("config","MGWSI")) s context=0
 . s b64=$$B64^%ZMGWSIS(string,context)
 ;
 QUIT $$b64Encode^MDBMCache(string)
 ;
parseSelect(selectExpression,domainName,queryExpression,attributes,orderBy,limit)
 ;select Ranking,Keyword from books where Title = 'The Right Stuff'
 ;
 n attributeList,error,no,p1
 ;
 ;d trace("in parseSelect - selectExpression="_selectExpression)
 s error=""
 s limit=""
 s selectExpression=$$convertSubstringToLowerCase(selectExpression,"select ")
 s selectExpression=$$convertSubstringToLowerCase(selectExpression," and ")
 i selectExpression=$$stripSpaces(selectExpression)
 i $e(selectExpression,1,7)'="select " QUIT $$invalid(1)
 s selectExpression=$$convertSubstringToLowerCase(selectExpression," from ")
 i selectExpression'[" from " QUIT $$invalid(3)
 s selectExpression=$$convertSubstringToLowerCase(selectExpression,"count(*)")
 s p1=$p(selectExpression,"select",2)
 s attributeList=$p(p1,"from",1)
 s attributeList=$$stripSpaces(attributeList)
 i attributeList="" QUIT $$invalid(2)
 i attributeList="*" d
 . ; do nothing
 e  i attributeList="count(*)" d
 . s attributes="count(*)"
 e  i attributeList'["," d
 . s attributes(1)=attributeList
 e  d
 . f no=1:1 q:attributeList=""  d
 . . s p1=$p(attributeList,",",1)
 . . s attributeList=$p(attributeList,",",2,5000)
 . . s p1=$$stripSpaces(p1)
 . . s attributes(no)=p1
 s p1=$p(selectExpression," from",2,1000)
 s domainName=p1
 s domainName=$$stripSpaces(domainName)
 s domainName=$p(domainName," ",1)
 i domainName="" QUIT $$invalid(4)
 i $e(domainName,1)="`",$e(domainName,$l(domainName))="`" s domainName=$e(domainName,2,$l(domainName)-1)
 s selectExpression=$p(selectExpression,domainName,2,5000)
 s selectExpression=$$convertSubstringToLowerCase(selectExpression," where ")
 s selectExpression=$$stripSpaces(selectExpression)
 s selectExpression=$$convertSubstringToLowerCase(selectExpression,"order by")
 s orderBy=""
 i selectExpression["order by" d
 . n attrName,dir
 . s orderBy=$p(selectExpression,"order by",2)
 . s orderBy=$$stripSpaces(orderBy)
 . s orderBy=$$convertSubstringToLowerCase(orderBy," limit ")
 . i orderBy[" limit " d
 . . s limit=$p(orderBy," limit ",2)
 . . s orderBy=$p(orderBy," limit ",1)
 . s attrName=$p(orderBy," ",1)
 . s dir=$p(orderBy," ",2)
 . i dir="" s dir="asc"
 . s orderBy="'"_attrName_"' "_dir
 . s selectExpression=$p(selectExpression,"order by",1)
 s selectExpression=$$convertSubstringToLowerCase(selectExpression," limit ")
 i limit="",selectExpression["limit " d
 . s limit=$p(selectExpression,"limit ",2)
 . s selectExpression=$p(selectExpression,"limit ",1)
 s selectExpression=$p(selectExpression,"where",2,1000)
 s queryExpression=selectExpression
 s queryExpression=$$stripSpaces(queryExpression)
 ;
 QUIT error
 ;
convertSubstringToLowerCase(string,subString)
 ;
 n lcString,newString,p1,pos,to1
 ;
 s lcString=$zconvert(string,"l")
 s subString=$zconvert(subString,"l")
 i lcString'[subString QUIT string
 s p1=$p(lcString,subString,1)
 s to1=$l(p1)
 s pos=$f(lcString,subString)
 s newString=$e(string,1,to1)_subString_$e(string,pos,$l(string))
 ;
 QUIT newString
 ;
inProc(queryExpression,expr,thisWord)
 ;
 n attrName,c,i,inValue,list,no,np,or,str,value
 ;
 i thisWord="in" s expr=expr_" "
 s list=$p(queryExpression,")",1)
 s queryExpression=$p(queryExpression,")",2,5000)
 s inValue=0,no=0,str=""
 f  q:list=""  d
 . s c=$e(list,1),list=$e(list,2,$l(list))
 . i c="'" d  q
 . . s inValue='inValue
 . . i inValue q
 . . s no=no+1
 . . s value(no)=str
 . . s str=""
 . i 'inValue q
 . s str=str_c
 s np=$l(expr," ")
 s attrName=$p(expr," ",np-2)
 s expr=$p(expr," ",1,np-3)
 s or=""
 f i=1:1:no d
 . s expr=expr_or_attrName_" = '"_value(i)_"'"
 . s or=" or "
 QUIT
 ;
executeSelect(queryExpression,itemList,keyId,itemStack)
 n c,error,expr,inAttr,lastWord,thisWord
 i $g(^zewd("trace"))=1 d trace($h_": in executeSelect.  queryExpression="_queryExpression)
 i queryExpression["''" d
 . s queryExpression=$$replaceAll(queryExpression," '''"," '"_$c(2))
 . s queryExpression=$$replaceAll(queryExpression,"='''","='"_$c(2))
 . s queryExpression=$$replaceAll(queryExpression,"'''",$c(2)_"'")
 . s queryExpression=$$replaceAll(queryExpression,"''",$c(2))
 k itemList
 s error=""
 s inAttr=0,expr="",lastWord="",thisWord=""
 f  q:queryExpression=""  d  q:c=")"
 . s c=$e(queryExpression,1),queryExpression=$e(queryExpression,2,$l(queryExpression))
 . i c="(" d  q
 . . n rel
 . . i thisWord="in"!(lastWord="in") d  q
 . . . d inProc(.queryExpression,.expr,thisWord)
 . . s error=$$executeSelect(.queryExpression,.itemList,keyId,.itemStack)
 . . i error'="" s queryExpression="",expr="" q
 . . s rel=$g(itemStack("rel"))
 . . i '$d(itemStack("list")) d
 . . . m itemStack("list")=itemList
 . . e  d
 . . . i rel="or" d
 . . . . n index,itemNo,no,newList
 . . . . s no=""
 . . . . f  s no=$o(itemList(no)) q:no=""  d
 . . . . . s itemNo=itemList(no)
 . . . . . s index(itemNo)=""
 . . . . s no=""
 . . . . f  s no=$o(itemStack("list",no)) q:no=""  d
 . . . . . s itemNo=itemStack("list",no)
 . . . . . s index(itemNo)=""
 . . . . s itemNo="",no=0
 . . . . k itemStack
 . . . . f  s itemNo=$o(index(itemNo)) q:itemNo=""  d
 . . . . . s no=no+1
 . . . . . s itemStack("list",no)=itemNo
 . . . i rel="and" d
 . . . . n index,itemNo,no,newList
 . . . . s no=""
 . . . . f  s no=$o(itemList(no)) q:no=""  d
 . . . . . s itemNo=itemList(no)
 . . . . . s index(itemNo)=""
 . . . . s no=""
 . . . . f  s no=$o(itemStack("list",no)) q:no=""  d
 . . . . . s itemNo=itemStack("list",no)
 . . . . . i $d(index(itemNo)) s newList(itemNo)=""
 . . . . s itemNo="",no=0
 . . . . k itemStack
 . . . . f  s itemNo=$o(newList(itemNo)) q:itemNo=""  d
 . . . . . s no=no+1
 . . . . . s itemStack("list",no)=itemNo
 . . s queryExpression=$$stripSpaces(queryExpression)
 . . s rel=$p(queryExpression," ",1)
 . . s queryExpression=$p(queryExpression," ",2,5000)
 . . s queryExpression=$$stripSpaces(queryExpression)
 . . i queryExpression'="",$e(queryExpression,1)'="(" s queryExpression="("_queryExpression_")"
 . . s itemStack("rel")=rel
 . i c=")" d  quit  ; pop
 . . k itemList
 . i expr="",c=" " q
 . i expr="" s inAttr=1,expr="'",thisWord=expr
 . i 'inAttr d
 . . n stop
 . . s stop=0
 . . i lastWord="and",expr["between" d  q:stop
 . . . n np
 . . . s np=$l(expr," ")
 . . . i $p(expr," ",np-3)="between" d
 . . . . n p1
 . . . . s stop=1
 . . . . s p1=$p(expr," ",1,np-4)_" >= "
 . . . . s p1=p1_$p(expr," ",np-2)_" and "_$p(expr," ",np-4)_" <= "
 . . . . s expr=p1
 . . . . s lastWord="<="
 . . i lastWord="and" s inAttr=1,expr=expr_"'",thisWord="'"
 . . i lastWord="or" s inAttr=1,expr=expr_"'",thisWord="'"
 . . i lastWord="union" s inAttr=1,expr=expr_"'",thisWord="'"
 . . i lastWord="intersection" s inAttr=1,expr=expr_"'",thisWord="'"
 . i inAttr,c="=" d
 . . s c=" "
 . . i $e(queryExpression,1)'=" " s queryExpression=" "_queryExpression
 . . s queryExpression="="_queryExpression
 . i c'=" " d  q
 . . s thisWord=thisWord_c
 . . s expr=expr_c
 . i inAttr d
 . . s expr=expr_"'"
 . . s thisWord=thisWord_"'"
 . . s inAttr=0
 . s expr=expr_c
 . s lastWord=thisWord
 . s thisWord=""
 . i lastWord="is" d
 . . i $e(queryExpression,1,4)="null" d
 . . . s lastWord="isNull"
 . . . s expr=$e(expr,1,$l(expr)-3)_"isNull "
 . . . s queryExpression="'null'"_$e(queryExpression,5,$l(queryExpression))
 . . i $e(queryExpression,1,8)="not null" d
 . . . s lastWord="isNotNull"
 . . . s expr=$e(expr,1,$l(expr)-3)_"isNotNull "
 . . . s queryExpression="'null'"_$e(queryExpression,9,$l(queryExpression))
 i expr'="" d
 . n i,name,np,offset,prevName,rel,term
 . s expr=$$escVals(expr)
 . s np=$l(expr," ")
 . i np>3 d
 . . n diffNames,name
 . . f i=1:1:np s term(i)=$p(expr," ",i)
 . . s offset=4
 . . s diffNames=0
 . . s name=term(1)
 . . f i=1:offset q:'$d(term(i))  d  q:diffNames
 . . . i term(i)'=name s diffNames=1 q
 . . . i $g(term(i+3))="intersection" s diffNames=1 q
 . . . i $g(term(i+3))="union" s diffNames=1 q
 . . i diffNames d
 . . . s expr="["_term(1)_" "_term(2)_" "_term(3)
 . . . f  d  q:'$d(term(offset+1))
 . . . . s name=term(offset+1)
 . . . . s rel=term(offset)
 . . . . i rel="and"!(rel="intersection") d
 . . . . . s expr=expr_"] intersection ["
 . . . . i rel="or"!(rel="union") d
 . . . . . s expr=expr_"] union ["
 . . . . s expr=expr_term(offset+1)_" "_term(offset+2)_" "_term(offset+3)
 . . . . s offset=offset+4
 . . . s expr=expr_"]"
 . . e  d
 . . . s expr="["_expr_"]"
 . e  d
 . . s expr="["_expr_"]"
 . k itemList
 . s expr=$tr(expr,$c(1)," ")
 . i $g(orderBy)'="" s expr=expr_" sort "_orderBy
 . s error=$$runQuery(keyId,domainName,expr,,.itemList)
 QUIT error
 ;
escVals(expr)
 ;
 n c,inValue,no,str
 ;
 s inValue=0,no=0,str=""
 f  q:expr=""  d
 . s c=$e(expr,1),expr=$e(expr,2,$l(expr))
 . i c="'" d  q
 . . s inValue='inValue
 . . s str=str_c
 . i 'inValue s str=str_c q
 . i c=" " s c=$c(1)
 . s str=str_c
 QUIT str
 ;
invalid(x)
 QUIT "InvalidQueryExpression~The specified query expression syntax is not valid. ("_x_")"
 ;
runSelect(keyId,query,itemList,attributes,domainName)
 ;
 n error,limit,orderBy,queryExpression
 ;
 ;d trace("in runSelect: query="_query)
 s error=$$parseSelect(query,.domainName,.queryExpression,.attributes,.orderBy,.limit)
 i $g(^zewd("trace"))=1 d trace($h_": completed parseSelect: queryExpression="_$g(queryExpression))
 i error'="" QUIT error
 i queryExpression="" d
 . i orderBy'="" d
 . . s error=$$invalid(5)
 . e  d
 . . s error=$$runQuery(keyId,domainName,"",,.itemList)
 e  d
 . i queryExpression["itemName()" s queryExpression=$$replaceAll(queryExpression,"itemName()","itemName{}")
 . s error=$$executeSelect(queryExpression,.itemList,keyId,.itemStack)
 . i $g(^zewd("trace"))=1 d trace($h_": finished executeSelect and returned to runSelect")
 ;***
 i $d(itemStack("list")) k itemList m itemList=itemStack("list")
 ;***
 i limit>0 d
 . n count,listCopy,no,stop
 . s count=0,no="",stop=0
 . f  s no=$o(itemList(no)) q:no=""  d  q:stop
 . . s count=count+1
 . . i count>limit s stop=1 q
 . . s listCopy(no)=itemList(no)
 . k itemList
 . m itemList=listCopy
 QUIT error
 ;
numeric(value)
 i $e(value,1)=0,$l(value)>1 QUIT 0
 i value?1N.N QUIT 1
 i value?1"-"1N.N QUIT 1
 i value?1N.N1"."1N.N QUIT 1
 i value?1"-"1N.N1"."1N.N QUIT 1
 QUIT 0
 ; 
escape(value)
 i value["&" s value=$$replaceAll(value,"&","&amp;")
 i value["<" s value=$$replaceAll(value,"<","&lt;")
 i value[">" s value=$$replaceAll(value,">","&gt;")
 QUIT value
 ;
externalSelect(keyId,selectExpression) ;
 ;
 n attributes,count,domainName,error,itemList,itemsAndAttrs,json,pos
 ;
 i $g(^zewd("trace"))=1 d trace("in externalSelect - keyId = "_keyId_": select="_selectExpression)
 s error=$$runSelect(keyId,selectExpression,.itemList,.attributes,.domainName)
 i error'="" d  QUIT json
 . s json="{""error"":{""errorCode"":"""_$p(error,"~",1)_""",""errorMessage"":"""_$p(error,"~",2)_"""}}"
 i $g(attributes)="count(*)" d
 . n count,pos
 . s itemsAndAttrs(1,"i")="Domain"
 . s itemsAndAttrs(1,"a",1,"n")="Count"
 . s pos="",count=0
 . f  s pos=$o(itemList(pos)) q:pos=""  s count=count+1
 . s itemsAndAttrs(1,"a",1,"v",1)=count
 e  d
 . n attribs,attribArray,ex,itemName,no,rx,bx,val,vno
 . s pos=""
 . f  s pos=$o(itemList(pos)) q:pos=""  d
 . . s itemName=itemList(pos)
 . . k attribs,attribArray
 . . m attribs=attributes
 . . s ex=$$getAttributes(keyId,domainName,itemName,.attribs,.rx,.bx,1)
 . . s no=""
 . . f  s no=$o(attribs(no)) q:no=""  d
 . . . s attribArray(no,"n")=$g(attribs(no))
 . . . s vno=""
 . . . f  s vno=$o(attribs(no,"value",vno)) q:vno=""  d
 . . . . s val=attribs(no,"value",vno)
 . . . . ;s val=$$replaceAll(val,"""","\""")
 . . . . s val=$$replaceAll(val,"\","\\")
 . . . . s attribArray(no,"v",vno)=val
 . . m itemsAndAttrs(pos,"a")=attribArray
 . . s itemsAndAttrs(pos,"i")=itemName
 s pos="",count=0
 f  s pos=$o(itemsAndAttrs(pos)) q:pos=""  s count=count+1
 i count=1 d
 . n array
 . m array=itemsAndAttrs(1)
 . ;m ^rob=array
 . s json=$$arrayToJSON^zmwire("array")
 . s json="["_json_"]"
 e  d
 . s json=$$arrayToJSON^zmwire("itemsAndAttrs")
 i $g(^zewd("trace"))=1 d trace("json="_json)
 QUIT json
 ;
trace(text,clear) ; trace  ;
 n i
 s text=$g(text)
 i $g(clear)=1 k ^%zewdTrace
 s i=$increment(^%zewdTrace)
 s ^%zewdTrace(i)=text
 QUIT
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
test
 s select="select * from testing where attr3 = 'control'"
 s key="rob"
 s ok=$$externalSelect^MDB(key,select)
 QUIT
 ;
