<?xml version="1.0" encoding="ISO-8859-1"?>
<ncl id="sender" xmlns="http://www.ncl.org.br/NCL3.0/EDTVProfile">
	<head>
		<regionBase>
			<region id="rTV" width="100%" height="100%" zIndex="1"/>		
		</regionBase>
		
		
		<descriptorBase>
			<descriptor id="dTV" region="rTV" />
		</descriptorBase>
	</head>
	
	<body>
		
		<port id="pLua" component="lua" />
		
		<media id="lua" descriptor="dTV" src="receiver.lua" />
		
	</body>
</ncl>