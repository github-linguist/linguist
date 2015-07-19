<?xml version="1.0" encoding="ISO-8859-1"?>
<ncl id="topProperty" xmlns="http://www.ncl.org.br/NCL3.0/EDTVProfile">
	<head>
		
		<regionBase>
			<region height="50%" id="imageReg" top="50%"/>
		</regionBase>
		
		<descriptorBase>
			<descriptor id="imageDescriptor" region="imageReg"/>
		</descriptorBase>
		
	</head>
	<body>
		<port component="image" id="entry"/>
		<media descriptor="imageDescriptor" id="image" src="../resources/images/background.jpg"/> 
	</body>
</ncl>
