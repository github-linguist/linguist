//%attributes = {"preemptive":"capable"}
C_OBJECT:C1216($webObject)
$webObject:=WEB server:C1674(2)  //host db webServer

  //some initiatialisation
clear_files 


C_OBJECT:C1216($settings)
C_OBJECT:C1216($result)
C_OBJECT:C1216($path;$newIndex)
C_TEXT:C284($response)
$settings:=New object:C1471
$settings.rootFolder:=Folder:C1567(fk web root folder:K87:15).parent.folder("WebFolder2").platformPath
$settings.certificateFolder:=Folder:C1567(fk database folder:K87:14).folder("certificate").platformPath
$settings.HTTPEnabled:=True:C214
$settings.HTTPPort:=8888
$settings.HTTPSEnabled:=True:C214
$settings.HTTPSPort:=4443
$settings.defaultHomepage:="index2.html"

$result:=$webObject.start($settings)

ASSERT:C1129($result#Null:C1517;"result should not be null")
ASSERT:C1129($result.success=True:C214;"result must contain a 'success' attribute and must be true")
ASSERT:C1129($webObject.isRunning=True:C214;"web server 'isRunning' should be true")

  //test HTTP
$statusCode:=HTTP Get:C1157("http://localhost:8888";$response)

ASSERT:C1129($statusCode=200;"Web Server not started")

$path:=Folder:C1567($settings.rootFolder;fk platform path:K87:2).file("index2.html")
$newIndex:=File:C1566($path.platformPath;fk platform path:K87:2)

ASSERT:C1129($response=$newIndex.getText();"unexpected index page content")


$statusCode:=HTTP Get:C1157("https://localhost:4443";$response)

ASSERT:C1129($statusCode=200;"Web Server not started")

$webObject.stop()




