<?xml version="1.0" encoding="ISO-8859-1"?>

<!--
2008 PUC-RIO/LABORATORIO TELEMIDIA,
Some Rights Reserved.

This program is free software; you can redistribute it and/or modify it under 
the terms of the GNU General Public License version 2 as published by
the Free Software Foundation.

This program is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
PARTICULAR PURPOSE.  See the GNU General Public License version 2 for more 
details.

You should have received a copy of the GNU General Public License version 2
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
-->

<ncl id="teste" xmlns="http://www.ncl.org.br/NCL3.0/EDTVProfile"
     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
     xsi:schemaLocation="http://www.ncl.org.br/NCL3.0/EDTVProfile
     http://www.ncl.org.br/NCL3.0/profiles/NCL30EDTV.xsd">

<head>

	<regionBase>
		<region id="luaRegion" width="100%" height="100%"/>
	</regionBase>

	<descriptorBase>
		<descriptor id="luaDesc" region="luaRegion" focusIndex="luaIdx"/>
	</descriptorBase>  
</head>

<body>
	<port id="init" component="lua"/>
	<media type="application/x-ginga-settings" id="programSettings">
		<property name="currentKeyMaster" value="luaIdx"/>
	</media>

	<media id="lua" descriptor="luaDesc" src="game.lua"/>
</body>

</ncl>
