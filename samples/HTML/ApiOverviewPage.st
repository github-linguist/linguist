<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Frameset//EN""http://www.w3.org/TR/REC-html40/frameset.dtd">
<html>
<head>
$Common_meta()$
<title>
Android API Differences Report
</title>
<body>
<div class="body">

$Header()$


<div class="content">
<h2>Android API Differences Report</h2>
<p>This document details the changes in the Android framework API. It shows 
additions, modifications, and removals for packages, classes, methods, and 
fields. Each reference to an API change includes a brief description of the 
API and an explanation of the change and suggested workaround, where available.</p>

<p>The differences described in this report are based a comparison of the APIs 
whose versions are specified in the upper-right corner of this page. It compares a 
newer "to" API to an older "from" version, noting any changes relative to the 
older API. So, for example, indicated API removals are no longer present in the "to" 
API.</p>
<p>For more information about the Android framework API and SDK, 
see the <a href="http://code.google.com/android/index.html" target="_top">Android product site</a>.</p>

$if(no_delta)$
<h3>Congratulation!</h3>
No differences were detected between the two provided APIs.
$endif$


$if(removed_packages)$
$Table(name="Removed Packages", rows=removed_packages:{$it.from:ModelElementRow()$})$
<br/>
$endif$


$if(added_packages)$
$Table(name="Added Packages", rows=added_packages:{$it.to:PackageAddedLink()$}:SimpleTableRow())$
<br/>
$endif$

$if(changed_packages)$
$Table(name="Changed Packages", rows=changed_packages:{$it.to:PackageChangedLink()$}:SimpleTableRow())$
<br/>
$endif$
</div>


</div>
</body>
</html>





