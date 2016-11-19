<?xml version="1.0" encoding="UTF-8"?>
<schema xmlns="http://purl.oclc.org/dsdl/schematron"
   queryBinding="xslt2"
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- XSLT 2.0 Schematron by Wendell Piez (Mulberry Technologies, Inc.),
       August 2011 -->
  
  <!-- This Schematron is released into the public domain.     
       Please credit your sources. -->
  
  
  <!-- Requires Schematron that allows embedded XSLT 2.0 with support for
       the namespace:: axis. Tested with Saxon 9.3.0.5. -->
  
  <ns prefix="m" uri="http://www.mulberrytech.com/xslt/util"/>
  
  <xsl:variable name="ns-set" xmlns:m="http://www.mulberrytech.com/xslt/util">
    <!-- include elements to declare expected namespace prefixes
         with their bindings, like so:

         (prefix 'mml')
         <m:ns prefix="mml"   uri="http://www.w3.org/1998/Math/MathML"/>
         (default namespace, no prefix)
         <m:ns prefix=""      uri="http://www.w3.org/1998/Math/MathML"/>
         -->
         
    <m:ns prefix="mml"   uri="http://www.w3.org/1998/Math/MathML"/>
    <m:ns prefix="xlink" uri="http://www.w3.org/1999/xlink"/>>
    <m:ns prefix="oasis" uri="http://docs.oasis-open.org/ns/oasis-exchange/table"/>

    <!-- 'xml' prefix is always in scope with this binding -->
    <m:ns prefix="xml"   uri="http://www.w3.org/XML/1998/namespace"/>
  </xsl:variable>
  
  <pattern>
    <!-- checking the document element against the spec given in ns-set -->
    <rule context="/*">
      <let name="new-prefixes" value="in-scope-prefixes(.)[not(. = $ns-set/m:ns/@prefix)]"/>
      <let name="new-namespaces" value="(for $p in (in-scope-prefixes(.)) return namespace-uri-for-prefix($p,.))
        [not(. = $ns-set/m:ns/@uri)]"/>
      <report test="exists($new-prefixes)">
        Unrecognized namespace prefix<xsl:value-of select="('es')[count($new-prefixes) gt 1]"/>: 
        <value-of select="string-join(
          (for $p in $new-prefixes return m:label($p)),
          ', ')"/>
      </report>
      <report test="exists($new-namespaces)">
        Unrecognized namespace URI<xsl:value-of select="('s')[count($new-namespaces) gt 1]"/>: 
        <value-of select="string-join(($new-namespaces),', ')"/>
      </report>
      
      <let name="misassigned-prefixes" value="in-scope-prefixes(.)[not(.=$new-prefixes)]
        [for $p in (.) return 
          namespace-uri-for-prefix($p,current()) ne $ns-set/m:ns[@prefix=$p]/@uri]"/>
      <report test="exists($misassigned-prefixes)">
        Prefix<xsl:value-of select="('es')[count($misassigned-prefixes) gt 1]"/> 
        assigned incorrectly: <value-of select="string-join(
          (for $p in $misassigned-prefixes return
          concat(m:label($p), ' (should be ''',$ns-set/m:ns[@prefix=$p]/@uri,''')')),
          '; ')"/>
      </report>
    </rule>
    
    <!-- Elsewhere, all namespaces given must correspond with those
      on the parent -->
    <rule context="*">
      <assert test="every $n in (namespace::*) satisfies
        exists(../namespace::*[deep-equal(.,$n)])">
        Namespace may not be declared here.
      </assert>
      <assert test="every $n in (../namespace::*) satisfies
        exists(namespace::*[deep-equal(.,$n)])">
        Namespace may not be undeclared here.
      </assert>
    </rule>
  </pattern>
  
  <xsl:function name="m:label" as="xs:string">
    <xsl:param name="label" as="xs:string"/>
    <xsl:sequence select="if (string($label)) then concat('''',$label,'''') else '[unprefixed]'"/>
  </xsl:function>
  
</schema>