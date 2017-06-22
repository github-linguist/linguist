Import System.Web

aPageVars = []

htmlcssattributes = ["classname","id","name","align",
"style","dir","value","onclick","oncontextmenu",
"ondblclick","onmousedown","onmouseenter","onmouseleave","onmousemove",
"onmouseover","onmouseout","onmouseup","onkeydown","onkeypress",
"onkeyup","onabort","onbeforeunload","onerror","onhashchange",
"onload","onpageshow","onpagehide","onresize","onscroll",
"onunload","onblur","onchange","onfocus","onfocusin",
"onfocusout","oninput","oninvalid","onreset","onsearch",
"onselect","onsubmit","ondrag","ondragend","ondragenter",
"ondragleave","ondragover","ondragstart","ondrop","oncopy",
"oncut","onpaste","onafterprint","onbeforeprint","oncanplay",
"oncanplaythrough","ondurationchange","onemptied","onended","onloadeddata",
"onloadedmetadata","onloadstart","onpause","onplay","onplaying",
"onprogress","onratechange","onseeked","onseeking","onstalled",
"onsuspend","ontimeupdate","onvolumechange","onwaiting","animationend",
"animationiteration","animationstart","transitionend","onmessage","onopen",
"onmousewheel","ononline","onoffline","onpostate","onshow",
"onstorage","ontoggle","onwheel","ontouchcancel","ontouchend",
"ontouchmove","ontouchstart","color","opacity","background","backgroundattachment",
"backgroundcolor","backgroundimage","backgroundposition","backgroundrepeat","backgroundclip",
"backgroundorigin","backgroundsize","border","borderbottom","borderbottomcolor",
"borderbottomleftradius","borderbottomrightradius","borderbottomstyle","borderbottomwidth","bordercolor",
"borderimage","borderimageoutset","borderimagerepeat","borderimageslice","borderimagesource",
"borderimagewidth","borderleft","borderleftcolor","borderleftstyle","borderleftwidth",
"borderradius","borderright","borderrightcolor","borderrightstyle","borderrightwidth",
"borderstyle","bordertop","bordertopcolor","bordertopleftradius","bordertoprightradius",
"bordertopstyle","bordertopwidth","borderwidth","boxdecorationbreak","boxshadow",
"bottom","clear","clip","display","float",
"height","left","margin","marginbottom","marginleft",
"marginright","margintop","maxheight","maxwidth","minheight",
"minwidth","overflow","overflowx","overflowy","padding",
"paddingbottom","paddingleft","paddingright","paddingtop","position",
"right","top","visibility","width","verticalalign",
"zindex","aligncontent","alignitems","alignself","flex",
"flexbasis","flexdirection","flexflow","flexgrow","flexshrink",
"flexwrap","justifycontent","order","hangingpunctuation","hyphens",
"letterspacing","linebreak","lineheight","overflowwrap","tabsize",
"textalign","textalignlast","textcombineupright","textindent","textjustify",
"texttransform","whitespace","wordbreak","wordspacing","wordwrap",
"textdecoration","textdecorationcolor","textdecorationline","textdecorationstyle","textshadow","textunderlineposition","@fontface","@fontfeaturevalues","font","fontfamily",
"fontfeaturesettings","fontkerning","fontlanguageoverride","fontsize","fontsizeadjust",
"fontstretch","fontstyle","fontsynthesis","fontvariant","fontvariantalternates",
"fontvariantcaps","fontvarianteastasian","fontvariantligatures","fontvariantnumeric","fontvariantposition",
"fontweight","direction","textorientation","unicodebidi","writingmode",
"bordercollapse","borderspacing","captionside","emptycells","tablelayout",
"counterincrement","counterreset","liststyle","liststyleimage","liststyleposition",
"liststyletype","@keyframes","animation","animationdelay","animationdirection",
"animationduration","animationfillmode","animationiterationcount","animationname","animationplaystate",
"animationtimingfunction","backfacevisibility","perspective","perspectiveorigin","transform",
"transformorigin","transformstyle","transition","transitionproperty","transitionduration",
"transitiontimingfunction","transitiondelay","boxsizing","content","cursor",
"imemode","navdown","navindex","navleft","navright",
"navup","outline","outlinecolor","outlineoffset","outlinestyle",
"outlinewidth","resize","textoverflow","breakafter","breakbefore",
"breakinside","columncount","columnfill","columngap","columnrule",
"columnrulecolor","columnrulestyle","columnrulewidth","columnspan","columnwidth",
"columns","widows","orphans","pagebreakafter","pagebreakbefore",
"pagebreakinside","marks","quotes","filter","imageorientation",
"imagerendering","imageresolution","objectfit","objectposition","mask",
"masktype","mark","markafter","markbefore","phonemes",
"rest","restafter","restbefore","voicebalance","voiceduration",
"voicepitch","voicepitchrange","voicerate","voicestress","voicevolume",
"marqueedirection","marqueeplaycount","marqueespeed","marqueestyle",
"datatoggle","dataride","datatarget",
"dataslideto","dataslide","datadismiss", "dataplacement",
"datacontent","datatrigger","dataspy","dataoffset","dataoffsettop"]

aObjsAttributes = ["link","newline","div","form","input",
		   "textarea","select","option","image","ul","li","table",
		   "tr","td","th","audio","video","h1","h2","h3","h4","h5",
		   "h6","p","nav","span","button"]

mergemethods("page","stylefunctions")
mergemethods("page","scriptfunctions")

mergemethods("ObjsBase","stylefunctions")
mergemethods("ObjsBase","scriptfunctions")
mergemethods("objsbase","newobjectsfunctions")

mergemethods("webpage","stylefunctions")
mergemethods("webpage","scriptfunctions")
mergemethods("webpage","newobjectsfunctions")

loadvars()

Func LoadVars

	New Application
	{
	    	if get("REQUEST_METHOD") = "GET"
			cInput = get("QUERY_STRING")
		else
			cInput = input(get("CONTENT_LENGTH"))
		ok
		
		aPageVars = decode(cInput)
		aArray = getcookies()

		for x in aArray
			aPageVars+x
		next
	}

	return cInput

Func WebPage return new WebPage

Func BootStrapWebPage return new BootStrapWebPage

Func htmlspecialchars cStr
	cResult = ""
	if isstring(cStr) and len(cStr) > 0
		for x in cStr
			if x = '&'  cResult += '&amp;'
			but x = '"' cResult += '&quot;'
			but x = "'" cResult += '&#039;'
			but x = '<' cResult += '&lt;'
			but x = '>' cResult += '&gt;'
			but x = ' ' cResult += '&nbsp;'
			else 	    cResult += x
			ok
		next
	ok
	return cResult

Func Template cFile,oObject

	cStr = Read(cFile)
	aList = []
	cResult = ""
	cCode = ""
	nPos = substr(cStr,"<%")
	if nPos = 0
		aList + cStr
		cCode += "cResult += aList[" + len(aList) + "]" + nl
	ok
	while nPos > 0
		cText = left(cStr,nPos-1)
		if cText != ""
			aList + cText
			cCode += "cResult += aList[" + len(aList) + "]" + nl
		ok
		cStr = substr(cStr,nPos+2)
		nPos = substr(cStr,"%>")
		if nPos > 0					
			if left(cStr,1) = "="
				cCode += "cResult += (" + substr(cStr,2,nPos-2) + ")" + nl	
			else
				cCode += left(cStr,nPos-1) + nl						
			ok
			cStr = substr(cStr,nPos+2)
		ok
		nPos = substr(cStr,"<%")
		if nPos = 0
			aList + cStr
			cCode += "cResult += aList[" + len(aList) + "]" + nl
		ok
	end
	oObject { 
		eval(cCode)
	}
	return cResult

Func Alert cMessage
	New Page
	{
		script( "document.onready = function() { alert(" +'"' + cMessage + '"' + ") }" )
	}

func HTML2PDF cStr

	cFileName = "temp/"+tempname()
	cHTML = cFileName + ".html"
	cPDF =  cFileName + ".pdf"
	write(cHTML,cStr)
	system("wkhtmltopdf " + cHTML + " " + cPDF)
	New Page 
	{  
		script(scriptredirection(cPDF))  
	}


Package System.Web
			
	Class Application

		cStart = ""
		cCookies = ""
		cOutput = ""
		cCSS = ""
		Title = "Test"
		cBody = ""

		lBootstrap = False

		Func DecodeString cStr
			cStr = cStr + "&" 
			aOutput = [] 	
			aRes = [] 
			cValue = ""
			for x = 1 to len(cStr)
				t = cStr[x]
				if t = "="  
					aRes + cValue
					cValue = ""
				but t = "&" 
					aRes + cValue
					cValue = "" 
					aOutput + aRes
					aRes = []
				but t = "+"
					cValue = cValue + " "
				but t = "%"
					cValue = cValue + hex2str(cStr[x+1]+cStr[x+2])
					x = x+2
				but t = '"'
				else 
					cValue = cValue + t 
				ok
			next
			return aOutput

		Func Decode cInput
			if left(get("CONTENT_TYPE"),20) != "multipart/form-data;"
				return decodestring(cInput)
			ok

			# Decode (File Uploading)

			nPos = substr(cInput,"Content-Disposition") 
			cMark = left(cInput,nPos-3)

			VarsList = []

			while true

				NewVar = [] # List contains the new variable
				nPos = substr(cInput,'"')  # start of the name

				cInput = substr(cInput,nPos+1) #remove before start
				nPos = substr(cInput,'"')  # end of the name

				NewVar + left(cInput,nPos-1)	# add Name to List
				cInput = substr(cInput,nPos+1) # after name	

				if left(cInput,1) = ";"

					nPos = substr(cInput,'"')  # start of the file name
					cInput = substr(cInput,nPos+1) # after file name
					nPos = substr(cInput,'"')  # end of the file name
					cFile = left(cInput,nPos-1)	# File Name to be added to the List
					
					for x = 1 to 3
						nPos = substr(cInput,nl)
						cInput = substr(cInput,nPos+1) # after new line
					next

					nPos = substr(cInput,cMark) 
					NewVar + left(cInput,nPos-2)    # Get File content
					NewVar + cFile			# Add file Name
					cInput = substr(cInput,nPos)
					
				else

					for x = 1 to 2
						nPos = substr(cInput,nl)
						cInput = substr(cInput,nPos+1) # after new line
					next

					nPos = substr(cInput,nl)
					NewVar + left(cInput,nPos-1)

					cInput = substr(cInput,nPos+1) # after value

				ok

				VarsList + NewVar

				nPos = substr(cInput,cMark) 

				if substr(cInput,cMark+"--") = nPos
					exit
				ok
			end

			return VarsList
		
		Func GetFileName  aArray,cVar
			for x in aArray
				if len(x) >= 3
					if x[1] = cVar
						return x[3]
					ok
				ok
			next
			return "NULL"

		Func setcookie name,value,expires,path,domain,secure
			cCookies += "Set-Cookie: "+name+"="+value+"; expires="+expires+
				    "; path="+path+"; domain="+domain+"; "+secure + "; HttpOnly"+ nl

		Func cookie name,value
			cCookies += "Set-Cookie: "+name+"="+value+";" + nl

		Func getcookies 
			cStr = get("HTTP_COOKIE")
			if cStr = "NULL" 	return 	OK 
			# var1=value; var2=value; var3=value
			cStr += ";"
			aOutput = [] 	 
			nPos = substr(cStr,";")
			While nPos > 0
				nPos2 = substr(cStr,"=") 
				if nPos2 = 0 exit ok
				aRes = []
				aRes + trim(left(cStr,nPos2-1))
				aRes + substr(cStr,nPos2+1,nPos-nPos2-1)
				aOutput + aRes 
				cStr = substr(cStr,nPos+1)
				nPos = substr(cStr,";")
			end
			return aOutput

		Func URLEncode cStr
			cOut = ""
			for x in cStr
				if isalnum(x)
					cOut += x
				but x = " "
					cOut += "+"
				else
					cOut += "%"+str2hex(x)
				ok
			next
			return cOut	
		
		Func ScriptLibs
			if lBootstrap
				cStr = nl+'<meta name="viewport" content="width=device-width, initial-scale=1">' + nl +
  				       '<link rel="stylesheet" href="http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css">'+nl+
				       '<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>'+nl+
			 	       '<script src="http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"></script>'+nl
			else
				cStr = nl+'<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>'+nl
			ok	
			return TabMLString(cStr)

		Func Print
			See cCookies + cStart + "<html>" + nl +
			"<header>"+nl+CHAR(9)+scriptlibs()+nl+
			CHAR(9)+"<title>"+nl+CHAR(9)+Char(9)+Title+nl+Char(9)+"</title>"+nl
			if cCSS != NULL
				See Char(9)+"<style>"+nl+CHAR(9)+CHAR(9)+cCSS+nl+Char(9)+"</style>"+nl
			ok
			see nl+"</header>" + nl +
			"<body"+ cBody + "> " + nl + cOutput + nl + "</body>" + nl + "</html>"

		Func style cStyle
			cCSS += cStyle 

		Func starthtml 

			cStart = "Content-type: text/html" + nl + nl +
				   "<meta charset='UTF-8'>" + nl

		Private

			nTabs = 1

			Func TabPush
				nTabs++

			Func TabPOP
				nTabs--

			Func GetTabs
				return copy(char(9),nTabs)

			Func TabMLString cStr
				aList = str2list(cStr)
				for t in aList
					t = GetTabs() + t
				next
				return list2str(aList)		


	Class Page from Application
		
		StartHtml()	

		Func text x
			if isstring(x)
				cOutput += nl+GetTabs() + htmlspecialchars(x)
			but isnumber(x)
				cOutput += nl+GetTabs() + htmlspecialchars(string(x))
			ok


		Func html x
			cOutput += x

		Func h1 x
			html( GetTabs()+"<h1>" + x + "</h1>" )

		Func h2 x
			html( GetTabs()+"<h2>" + x + "</h2>" )

		Func h3 x
			html( GetTabs()+"<h3>" + x + "</h3>" )

		Func h4 x
			html( GetTabs()+"<h4>" + x + "</h4>" )

		Func h5 x
			html( GetTabs()+"<h5>" + x + "</h5>" )

		Func h6 x
			html( GetTabs()+"<h6>" + x + "</h6>" )

		Func p aPara
			html( nl+GetTabs()+'<p ' )
			addattributes(aPara)
			html( '>' )
			if aPara[:text] != NULL
				html( nl+GetTabs()+aPara[:text] )
			ok
			html( nl+GetTabs()+'</p>' )

		Func newline 
			cOutput += nl+GetTabs()+"<br />"

		
		Func addattributes aPara

			aList = [ :class,:id,:name,:align,:style,:dir,:value,:onclick,:oncontextmenu,
				  :ondblclick,:onmousedown,:onmouseenter,:onmouseleave,
				  :onmousemove,:onmouseover,:onmouseout,:onmouseup,
				  :onkeydown,:onkeypress,:onkeyup,:onabort,:onbeforeunload,
				  :onerror,:onhashchange,:onload,:onpageshow,:onpagehide,
				  :onresize,:onscroll,:onunload,:onblur,:onchange,:onfocus,
				  :onfocusin,:onfocusout,:oninput,:oninvalid,:onreset,
				  :onsearch,:onselect,:onsubmit,:ondrag,:ondragend,:ondragenter,
				  :ondragleave,:ondragover,:ondragstart,:ondrop,:oncopy,:oncut,
				  :onpaste,:onafterprint,:onbeforeprint,:oncanplay,
				  :oncanplaythrough,:ondurationchange,:onemptied,:onended,
				  :onloadeddata,:onloadedmetadata,:onloadstart,
				  :onpause,:onplay,:onplaying,:onprogress,:onratechange,
				  :onseeked,:onseeking,:onstalled,:onsuspend,:ontimeupdate,
				  :onvolumechange,:onwaiting,:animationend,:animationiteration,
				  :animationstart,:transitionend, :onmessage, :onopen, 
				  :onmousewheel, :ononline, :onoffline, :onpostate, :onshow,
				  :onstorage, :ontoggle, :onwheel, :ontouchcancel, :ontouchend,
				  :ontouchmove, :ontouchstart ]
			
			for t in aPara
				for x in aList					
					if t[1] = x
						html( ' ' + lower(t[1]) + '="'+t[2]+'"')
						exit
					ok
				next
			next			
	
			
		Func link aPara
			html( nl + GetTabs() + "<a href='" )
			if aPara[:url] != NULL
				cOutput += aPara[:url] + "'"
			else
				cOutput += "'"
			ok
			addattributes(aPara)
			cOutput += "> "
			if aPara[:title] != NULL
				cOutput += aPara[:title]
			ok
			html( " </a> " )

		Func Image aPara
			cOutput += nl+GetTabs()+'<img'
			addattributes(aPara)
			if aPara["url"] != NULL
				cOutput += ' src="'+aPara["URL"]+'"'
			ok
			if aPara["alt"] != NULL
				cOutput += ' alt="'+aPara["Alt"]+'"'
			ok
			cOutput += '> '

		Func button aPara
			cOutput += nl+GetTabs()+'<input type="button"'
			addattributes(aPara)
			if aPara[:value] != NULL
				cOutput += ' value="'+aPara[:value]+'"'
			ok
			cOutput += '>'

		Func buttonlink aPara
			cOutput += nl+GetTabs()+'<input type="button"'
			addattributes(aPara)
			if aPara[:link] != NULL
				cOutput += ' onclick="window.location.href = '+"'"+aPara[:link]+"'"+'"'
			ok
			if aPara[:value] != NULL
				cOutput += ' value="'+aPara[:value]+'"'
			ok
			cOutput += '>'

		Func textbox aPara
			cOutput += nl+GetTabs()+'<input type="'
			if aPara[:type] != NULL
				cOutput += aPara[:type] + '"'
			else
				cOutput += 'text"'
			ok
			addattributes(aPara)
			cOutput += ' >' + nl

		Func editbox aPara
			cOutput += nl+GetTabs()+'<textarea '
			if aPara[:rows] != NULL
				cOutput += ' rows="'+ aPara[:rows] + '"'
			else
				cOutput += ' rows="4"'
			ok
			if aPara[:cols] != NULL
				cOutput += ' cols="'+ aPara[:cols] + '"'
			else
				cOutput += ' cols="50"'
			ok
			addattributes(aPara)
			cOutput += ' >' + nl 
			TabPush()
			if aPara[:value] != NULL
				cOutput += aPara[:value]
			ok
			TabPOP()
			cOutput += nl+GetTabs()+'</textarea>' + nl

		Func combobox aPara
			TabPush()
			cOutput += nl+GetTabs()+'<select '
			addattributes(aPara)
			cOutput += ">" + nl
			if islist(aPara[:items])

				for x in aPara[:items]	
  					cOutput += nl+GetTabs()+'<option>'+x+'</option>'
				next
				TabPOP()
			ok
			cOutput += nl+GetTabs()+"</select>" + nl

		Func listbox aPara
			cOutput += nl+GetTabs()+'<select '
			addattributes(aPara)
			cOutput += "multiple='multiple' >" + nl
			if islist(aPara[:items])
				for x in aPara[:items]	
  					cOutput += nl+GetTabs()+'<option>'+x+'</option>'
				next
			ok
			cOutput += "</select>" + nl

		Func tagstart x,aPara
			html( nl + GetTabs() + '<'+x+' ' )
			addattributes(aPara)
			html( '>' + nl )
			TabPush()

		Func tagend x
			TabPOP()
			html( nl + GetTabs() +'</'+x+'>' + nl )

		Func ulstart aPara
			tagstart("ul",aPara)

		Func ulend
			tagend("ul")

		Func listart aPara
			tagstart("li",aPara)

		Func liend
			tagend("li")

		Func List2UL aList
			ulstart([])
				for x in aList
					listart([])
						text(x)
					liend()
				next
			ulend()	

		Func divstart aPara
			html( nl + GetTabs() + '<div ' )
			addattributes(aPara)
			html( '>' + nl )
			TabPush()

		Func navstart aPara
			html( nl + GetTabs() + '<nav ' )
			addattributes(aPara)
			html( '>' + nl )
			TabPush()

		Func spanstart aPara
			html( nl + GetTabs() + '<span ' )
			addattributes(aPara)
			html( '>' + nl )
			TabPush()


		Func boxstart 
			html( nl + GetTabs() +'<div style="width:100% ; height:30px ; color: yellow ; background-color:black">' + nl)
			TabPush()

		Func divend
			TabPOP()
			html( nl + GetTabs() +'</div>' + nl )

		Func navend
			TabPOP()
			html( nl + GetTabs() +'</nav>' + nl )

		Func spanend
			TabPOP()
			html( nl + GetTabs() +'</span>' + nl )


		Func boxend
			divend()		

		Func formstart x
			if x = ""
				html( nl + GetTabs() +"<form>" + nl )
			else
				html( nl + GetTabs() +'<form action="' + x + '">' + nl )
			ok
			TabPush()


		Func formpost x
			if x = ""
				html( nl + GetTabs() +"<form>" + nl )
			else
				html( nl + GetTabs() +'<form action="' + x + '" method="post">' + nl )
			ok
			TabPush()

		Func formend
			TabPOP()
			html( nl + GetTabs() +"</form>" + nl )

		Func submit aPara
			cOutput += nl+GetTabs()+'<input type="submit" '
			addattributes(aPara)
			cOutput += '>' + nl
		
		Func hidden name,value
			cOutput += nl+GetTabs()+'<input type="hidden" name="' + name + '" value="' + value + '">' + nl

		Func formupload x
			if x = ""
				cOutput += nl+GetTabs()+"<form>" + nl
			else
				cOutput += nl+GetTabs()+'<form action="' + x + '" method="post" enctype="multipart/form-data">' + nl
			ok
			TabPush()
		
		Func uploadfile x
			cOutput += nl+GetTabs()+'<input type="file" name="'+x+'"/>'

		Func Video aPara
			cOutput += nl+GetTabs()+'<video '
			addattributes(aPara)
			if aPara[:width] != NULL
				cOutput += ' width="'+aPara[:width]+'"'
			ok
			if aPara[:height] != NULL
				cOutput += ' height="'+aPara[:height]+'"'
			ok
			cOutput +=' controls>' 
			if aPara[:file] != NULL
				cOutput += '<source src="'+aPara[:file]+'"'
			ok
			if aPara[:type] != NULL
				cOutput += ' type="'+aPara[:type]+'"' 
			ok
			cOutput += '> Your browser does not support the video tag. </video>' 

		Func Audio aPara
			cOutput += nl+GetTabs()+'<audio controls' 
			addattributes(aPara)
			cOutput += '> <source '
			if aPara[:file] != NULL
				cOutput += ' src="'+aPara[:file]+'"'
			ok
			if aPara[:type] != NULL
				cOutput += ' type="'+aPara[:type]+'"' 
			ok
			cOutput += '> Your browser does not support the audio element. </audio>'

		Func GetColor aPara
			cOutput += nl+GetTabs()+'<input' 
			addattributes(aPara)
			if aPara[:value] != NULL
				cOutput += ' value="' + aPara[:value] + '"'
			else
				cOutput += ' value="#ff0000"'
			ok				
			cOutput += 'onchange="clickColor(0, -1, -1, 5)" type="color">'

		Func Radio aPara
			cOutput += nl+GetTabs()+'<input type="radio"'
			addattributes(aPara)
			if aPara[:value] != Null
				cOutput += ' value="'+aPara[:value]+'"'
			ok
			if aPara[:status] != NULL
				cOutput += " checked"
			ok
			cOutput += '>'
			if aPara[:text] != NULL
				cOutput += aPara[:text]
			ok

		Func Checkbox aPara
			cOutput += nl+GetTabs()+'<input type="checkbox"'
			addattributes(aPara)
			if aPara[:value] != Null
				cOutput += ' value="'+aPara[:value]+'"'
			ok
			if aPara[:status] != NULL
				cOutput += " checked"
			ok
			cOutput += '>'
			if aPara[:text] != NULL
				cOutput += aPara[:text]
			ok

		Func Spinner aPara
			cOutput += nl+GetTabs()+'<input type="number"'
			addattributes(aPara)
			if aPara[:min] != NULL
				cOutput += ' min="'+aPara[:min]+'"'
			ok
			if aPara[:max] != NULL
				cOutput += ' max="'+aPara[:max]+'"'
			ok
			cOutput += '>' + nl

		Func Slider aPara
			cOutput += nl+GetTabs()+'<input type="range"'
			addattributes(aPara)
			if aPara[:min] != NULL
				cOutput += ' min="'+aPara[:min]+'"'
			ok
			if aPara[:max] != NULL
				cOutput += ' max="'+aPara[:max]+'"'
			ok
			cOutput += '>' + nl

		Func tablestart aPara
			cOutput += nl+GetTabs()+'<table '
			addattributes(aPara)
			cOutput += '>'
			TabPush()

		Func tableend
			TabPOP()
			cOutput += nl+GetTabs()+'</table>'

		Func rowstart aPara
			cOutput += nl+GetTabs()+'<tr '
			addattributes(aPara)
			cOutput += '>'
			TabPush()

		Func rowend
			TabPOP()
			cOutput += nl+GetTabs()+'</tr>'		

		Func cellstart aPara
			TabPush()
			cOutput += nl+GetTabs()+'<td '
			addattributes(aPara)
			cOutput += '>'

		Func cellend
			cOutput += nl+GetTabs()+"</td>" + nl
			TabPOP()

		Func headerstart aPara
			cOutput += nl+GetTabs()+'<th '
			addattributes(aPara)
			cOutput += '>'
			TabPush()

		Func headerend
			TabPOP()
			cOutput += nl+GetTabs()+"</th>" + nl

		Func braceend 
			print() 

