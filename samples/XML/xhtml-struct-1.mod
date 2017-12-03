<!-- ...................................................................... -->
<!-- XHTML Structure Module  .............................................. -->
<!-- file: xhtml-struct-1.mod

     This is XHTML, a reformulation of HTML as a modular XML application.
     Copyright 1998-2000 W3C (MIT, INRIA, Keio), All Rights Reserved.
     Revision: $Id: xhtml-struct-1.mod,v 1.1.1.1 2006/01/09 19:23:30 rcrews Exp $ SMI

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

       PUBLIC "-//W3C//ELEMENTS XHTML Document Structure 1.0//EN"
       SYSTEM "http://www.w3.org/TR/xhtml-modulatization/DTD/xhtml-struct-1.mod"

     Revisions:
     (none)
     ....................................................................... -->

<!-- Document Structure

        title, head, body, html

     The Structure Module defines the major structural elements and 
     their attributes.

     Note that the content model of the head element type is redeclared 
     when the Base Module is included in the DTD.

     The parameter entity containing the XML namespace URI value used 
     for XHTML is '%XHTML.xmlns;', defined in the Qualified Names module.
-->

<!-- title: Document Title ............................. -->

<!-- The title element is not considered part of the flow of text.
     It should be displayed, for example as the page header or
     window title. Exactly one title is required per document.
-->

<!ENTITY % title.element  "INCLUDE" >
<![%title.element;[
<!ENTITY % title.content  "( #PCDATA )" >
<!ENTITY % title.qname  "title" >
<!ELEMENT %title.qname;  %title.content; >
<!-- end of title.element -->]]>

<!ENTITY % title.attlist  "INCLUDE" >
<![%title.attlist;[
<!ATTLIST %title.qname;
      %XHTML.xmlns.attrib; 
      %I18n.attrib;
>
<!-- end of title.attlist -->]]>

<!-- head: Document Head ............................... -->

<!ENTITY % head.element  "INCLUDE" >
<![%head.element;[
<!ENTITY % head.content
    "( %HeadOpts.mix;, %title.qname;, %HeadOpts.mix; )"
>
<!ENTITY % head.qname  "head" >
<!ELEMENT %head.qname;  %head.content; >
<!-- end of head.element -->]]>

<!ENTITY % head.attlist  "INCLUDE" >
<![%head.attlist;[
<!-- reserved for future use with document profiles
-->
<!ENTITY % profile.attrib
     "profile      %URI.datatype;           '%XHTML.profile;'"
>

<!ATTLIST %head.qname;
      %XHTML.xmlns.attrib; 
      %I18n.attrib;
      %profile.attrib;
>
<!-- end of head.attlist -->]]>

<!-- body: Document Body ............................... -->

<!ENTITY % body.element  "INCLUDE" >
<![%body.element;[
<!ENTITY % body.content
     "( %Block.mix; )+"
>
<!ENTITY % body.qname  "body" >
<!ELEMENT %body.qname;  %body.content; >
<!-- end of body.element -->]]>

<!ENTITY % body.attlist  "INCLUDE" >
<![%body.attlist;[
<!ATTLIST %body.qname;
      %Common.attrib;
>
<!-- end of body.attlist -->]]>

<!-- html: XHTML Document Element ...................... -->

<!ENTITY % html.element  "INCLUDE" >
<![%html.element;[
<!ENTITY % html.content  "( %head.qname;, %body.qname; )" >
<!ENTITY % html.qname  "html" >
<!ELEMENT %html.qname;  %html.content; >
<!-- end of html.element -->]]>

<!ENTITY % html.attlist  "INCLUDE" >
<![%html.attlist;[
<!-- version attribute value defined in driver
-->
<!ENTITY % XHTML.version.attrib
     "version      %FPI.datatype;           #FIXED '%XHTML.version;'"
>

<!-- see the Qualified Names module for information 
     on how to extend XHTML using XML namespaces 
-->
<!ATTLIST %html.qname;
      %XHTML.xmlns.attrib; 
      %XHTML.version.attrib;
      %I18n.attrib;
>
<!-- end of html.attlist -->]]>

<!-- end of xhtml-struct-1.mod -->
