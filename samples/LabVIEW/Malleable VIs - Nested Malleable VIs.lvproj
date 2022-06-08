<?xml version='1.0' encoding='UTF-8'?>
<Project Type="Project" LVVersion="19008000">
	<Property Name="NI.LV.All.SourceOnly" Type="Bool">true</Property>
	<Property Name="NI.LV.ExampleFinder" Type="Str">&lt;?xml version="1.0" encoding="UTF-8"?&gt;
&lt;ExampleProgram&gt;&lt;Title&gt;	&lt;Text Locale="US"&gt;Malleable VIs - Nested Malleable VIs.lvproj&lt;/Text&gt;&lt;/Title&gt;&lt;Keywords&gt;	&lt;Item&gt;polymorphic&lt;/Item&gt;	&lt;Item&gt;malleable&lt;/Item&gt;	&lt;Item&gt;VI&lt;/Item&gt;	&lt;Item&gt;Array&lt;/Item&gt;	&lt;Item&gt;sort&lt;/Item&gt;	&lt;Item&gt;search&lt;/Item&gt;&lt;/Keywords&gt;&lt;Navigation&gt;	&lt;Item&gt;7038&lt;/Item&gt;&lt;/Navigation&gt;&lt;FileType&gt;LV Project&lt;/FileType&gt;&lt;Metadata&gt;&lt;Item Name="RTSupport"&gt;LV Project&lt;/Item&gt;&lt;/Metadata&gt;&lt;ProgrammingLanguages&gt;&lt;Item&gt;LabVIEW&lt;/Item&gt;&lt;/ProgrammingLanguages&gt;&lt;RequiredSoftware&gt;&lt;NiSoftware MinVersion="18.0"&gt;LabVIEW&lt;/NiSoftware&gt; &lt;/RequiredSoftware&gt;&lt;/ExampleProgram&gt;</Property>
	<Property Name="NI.Project.Description" Type="Str">This example shows how you can use nested malleable VIs to create particularly flexible APIs. The particular API demonstrated is for searching and sorting arrays, but the principles apply to many other APIs.</Property>
	<Item Name="My Computer" Type="My Computer">
		<Property Name="server.app.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.control.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.tcp.enabled" Type="Bool">false</Property>
		<Property Name="server.tcp.port" Type="Int">0</Property>
		<Property Name="server.tcp.serviceName" Type="Str">My Computer/</Property>
		<Property Name="server.tcp.serviceName.default" Type="Str">My Computer/</Property>
		<Property Name="server.vi.callsEnabled" Type="Bool">true</Property>
		<Property Name="server.vi.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="specify.custom.address" Type="Bool">false</Property>
		<Item Name="Lesson 1 code" Type="Folder">
			<Item Name="Incorrect - Shuffle Bytes.vim" Type="VI" URL="../Incorrect - Shuffle Bytes.vim"/>
			<Item Name="Inefficient - Shuffle Bytes.vim" Type="VI" URL="../Inefficient - Shuffle Bytes.vim"/>
			<Item Name="Recommended - Shuffle Bytes.vim" Type="VI" URL="../Recommended - Shuffle Bytes.vim"/>
		</Item>
		<Item Name="Lesson 2a code" Type="Folder">
			<Item Name="Equal Via Regular Expression Functor.lvclass" Type="LVClass" URL="../Equal Via Regular Expression Functor/Equal Via Regular Expression Functor.lvclass"/>
			<Item Name="String Equals Ignore Case.vi" Type="VI" URL="../String Equals Ignore Case.vi"/>
		</Item>
		<Item Name="Lesson 2b code" Type="Folder">
			<Item Name="String With Equals.lvclass" Type="LVClass" URL="../String With Equals/String With Equals.lvclass"/>
			<Item Name="String Without Equals.lvclass" Type="LVClass" URL="../String Without Equals/String Without Equals.lvclass"/>
		</Item>
		<Item Name="Lesson 3 code" Type="Folder">
			<Item Name="Equal With Epsilon Functor.lvclass" Type="LVClass" URL="../Equal With Epsilon Functor/Equal With Epsilon Functor.lvclass"/>
		</Item>
		<Item Name="Lesson 4 code" Type="Folder">
			<Item Name="String Less Than Ignore Case.vi" Type="VI" URL="../String Less Than Ignore Case.vi"/>
			<Item Name="String Less Than Ignore Case_Wrong Conpane.vi" Type="VI" URL="../String Less Than Ignore Case_Wrong Conpane.vi"/>
		</Item>
		<Item Name="Lesson 5 code" Type="Folder">
			<Property Name="NI.SortType" Type="Int">0</Property>
			<Item Name="Numeric Strings Less Functor.lvclass" Type="LVClass" URL="../Numeric Strings Less Functor/Numeric Strings Less Functor.lvclass"/>
		</Item>
		<Item Name="Nested Malleable VIs - Lesson 1.vi" Type="VI" URL="../Nested Malleable VIs - Lesson 1.vi"/>
		<Item Name="Nested Malleable VIs - Lesson 2a.vi" Type="VI" URL="../Nested Malleable VIs - Lesson 2a.vi"/>
		<Item Name="Nested Malleable VIs - Lesson 2b.vi" Type="VI" URL="../Nested Malleable VIs - Lesson 2b.vi"/>
		<Item Name="Nested Malleable VIs - Lesson 3.vi" Type="VI" URL="../Nested Malleable VIs - Lesson 3.vi"/>
		<Item Name="Nested Malleable VIs - Lesson 4.vi" Type="VI" URL="../Nested Malleable VIs - Lesson 4.vi"/>
		<Item Name="Nested Malleable VIs - Lesson 5.vi" Type="VI" URL="../Nested Malleable VIs - Lesson 5.vi"/>
		<Item Name="Dependencies" Type="Dependencies">
			<Item Name="vi.lib" Type="Folder">
				<Item Name="Equal Comparable.lvclass" Type="LVClass" URL="/&lt;vilib&gt;/Comparison/Equal/Equal Comparable/Equal Comparable.lvclass"/>
				<Item Name="Equal Functor.lvclass" Type="LVClass" URL="/&lt;vilib&gt;/Comparison/Equal/Equal Functor/Equal Functor.lvclass"/>
				<Item Name="Equals.vim" Type="VI" URL="/&lt;vilib&gt;/Comparison/Equals.vim"/>
				<Item Name="Insert Into Sorted Array.vim" Type="VI" URL="/&lt;vilib&gt;/Array/Insert Into Sorted Array.vim"/>
				<Item Name="Less Comparable.lvclass" Type="LVClass" URL="/&lt;vilib&gt;/Comparison/Less/Less Comparable/Less Comparable.lvclass"/>
				<Item Name="Less Functor.lvclass" Type="LVClass" URL="/&lt;vilib&gt;/Comparison/Less/Less Functor/Less Functor.lvclass"/>
				<Item Name="Less.vim" Type="VI" URL="/&lt;vilib&gt;/Comparison/Less.vim"/>
				<Item Name="Search Sorted 1D Array.vim" Type="VI" URL="/&lt;vilib&gt;/Array/Search Sorted 1D Array.vim"/>
				<Item Name="Search Unsorted 1D Array Core.vim" Type="VI" URL="/&lt;vilib&gt;/Array/Helpers/Search Unsorted 1D Array Core.vim"/>
				<Item Name="Search Unsorted 1D Array.vim" Type="VI" URL="/&lt;vilib&gt;/Array/Search Unsorted 1D Array.vim"/>
				<Item Name="Shuffle 1D Array.vim" Type="VI" URL="/&lt;vilib&gt;/Array/Shuffle 1D Array.vim"/>
				<Item Name="Sort 1D Array Core.vim" Type="VI" URL="/&lt;vilib&gt;/Array/Helpers/Sort 1D Array Core.vim"/>
				<Item Name="Sort 1D Array.vim" Type="VI" URL="/&lt;vilib&gt;/Array/Sort 1D Array.vim"/>
				<Item Name="Two-Way Comparison Versus Array Element.vim" Type="VI" URL="/&lt;vilib&gt;/Array/Helpers/Two-Way Comparison Versus Array Element.vim"/>
			</Item>
		</Item>
		<Item Name="Build Specifications" Type="Build"/>
	</Item>
</Project>
