<?xml version="1.0" encoding="UTF-8"?>

<!-- ============================================================= -->
<!-- 
  This work is in the public domain and may be reproduced, published or 
  otherwise used without the permission of the National Library of Medicine (NLM).
  
  We request only that the NLM is cited as the source of the work.
  
  Although all reasonable efforts have been taken to ensure the accuracy and 
  reliability of the software and data, the NLM and the U.S. Government  do 
  not and cannot warrant the performance or results that may be obtained  by
  using this software or data. The NLM and the U.S. Government disclaim all 
  warranties, express or implied, including warranties of performance, 
  merchantability or fitness for any particular purpose.
-->
<!-- ============================================================= -->

<schema xmlns="http://purl.oclc.org/dsdl/schematron"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:m="http://mulberrytech.com/xslt/oasis-html/util"
  queryBinding="xslt2">
  
  <title>OASIS/CALS table validation</title>
  
  <!-- Mulberry Technologies (wap)
       
       Designed for JATS, but probably also useful in other
       systems using OASIS/CALS/Docbook tables
       
       Assumes a table DTD (or schema) valid to the OASIS/CALS/Docbook model
  -->
  
  <ns prefix="o" uri="http://docs.oasis-open.org/ns/oasis-exchange/table"/>
  <ns prefix="m" uri="http://mulberrytech.com/xslt/oasis-html/util"/>
  <!--<ns prefix="xsl" uri="http://www.w3.org/1999/XSL/Transform"/>-->
  
  <!-- the included stylesheet includes key and function declarations required -->
  <xsl:include href="oasis-exchange-support.xsl"/>
  
  <let name="default-border-style" value="solid"/>
  
  <pattern>
    <rule context="o:tgroup">
      <let name="okay-cols" value="@cols[. castable as xs:integer][. > 0]"/>
      <assert test="exists(@cols)">tgroup/@cols is not given</assert>
      <assert test="empty(@cols) or exists($okay-cols)">@cols should be a natural number
      (integer greater than zero).</assert>
      <assert test="empty($okay-cols) or empty(o:colspec) or (count(o:colspec) = @cols)">The tgroup has 
        <value-of select="count(o:colspec)"/> colspec<value-of select="if (count(o:colspec) = 1) then '' else 's'"/>, 
        but its @cols is given as '<value-of select="@cols"/>'.</assert>
      <report test="not(*/o:row/m:actual-cols(.) != */o:row/m:actual-cols(.))
        and ($okay-cols != m:actual-cols((*/o:row)[1]))">tgroup/@cols is given as
        <value-of select="$okay-cols"/>, but all rows have <value-of
          select="m:actual-cols((*/o:row)[1])"/> entr<value-of 
          select="if (m:actual-cols((*/o:row)[1]) eq 1) then 'y' else 'ies'"/>.
      </report>
      <report test="@align='char'" role="warning">Without assigning @char or @charoff to everything,
        assigning @align='char' to tgroup only aligns contents to right of center.</report>
    </rule>
    
    <rule context="o:colspec">
      <let name="okay-colwidth"
        value="@colwidth[exists(m:colwidth-unit(current()))]"/>
      <assert test="empty(@colwidth) or exists($okay-colwidth)">Malformed @colwidth.</assert>
      <assert test="empty($okay-colwidth) or 
        (count(../o:colspec[m:colwidth-unit(.)=m:colwidth-unit(current())]) &gt;
         count(../o:colspec[not(m:colwidth-unit(.)=m:colwidth-unit(current()))]))">@colwidth unit
        (<value-of select="m:colwidth-unit(.)"/>) is not consistent with the
        units on other colspecs.</assert>
      
      <assert test="empty($okay-colwidth) or matches($okay-colwidth,'^\s*\*\s*$')
        or (xs:double(replace($okay-colwidth,'[\s\p{L}\*]','')[. castable as xs:double]) &gt; 0)">@colwidth must be positive</assert>
      <report test="empty(m:colwidth-unit(.))
        and exists(../o:colspec/m:colwidth-unit(.))">The same unit of measure should be used on every 
        colspec/@colwidth.</report>
      
      <assert test="empty(@colnum) or (@colnum = count(.|preceding-sibling::o:colspec))">@colnum 
        '<value-of select="@colnum"/>' does not correspond to
        the column's actual number (<value-of select="count(.|preceding-sibling::o:colspec)"/>)</assert>
      <report test="@colname = (../o:colspec except .)/@colname">The same @colname is assigned to more than
         one colspec.</report>
      <assert test="not(@align='char') or exists(@char)" role="warning">@align='char', but no @char is given.</assert>
      <report test="normalize-space(@char) and not((@align,../@align)[1]='char')">@char is given, but alignment is not 'char'.</report>
      <assert test="empty(@charoff) or ((@align,../@align)[1]='char')" role="warning">@charoff is given, but alignment is not 'char'.</assert>
    </rule>
    
    <rule context="o:row">
      <let name="tgroup" value="ancestor::o:tgroup[1]"/>
      <let name="rowno" value="m:rowno(.)"/>
      <let name="given-entries" value="count(distinct-values(key('entry-by-row',$rowno,$tgroup)/m:across(.)))"/>
      <report test="$given-entries &lt; $tgroup/@cols
        and exists($tgroup/@cols[. castable as xs:integer])" role="warning">
        The row doesn't have enough entries (<value-of select="$tgroup/@cols"/> 
        <value-of select="if ($tgroup/@cols=1) then ' is' else ' are'"/> expected;
        <value-of select="$given-entries"/> <value-of select="if ($given-entries=1) then ' is' else ' are'"/> given).
      </report>
    </rule>
    <rule context="o:entry">
      <let name="tgroup" value="ancestor::o:tgroup[1]"/>
      <assert test="empty(@nameend) or exists(key('colspec-by-name',@nameend,$tgroup))">No colspec is 
        named <value-of select="@nameend"/>.</assert>
      <assert test="empty(@nameend|@namest) or 
        (key('colspec-by-name',@nameend,$tgroup) >> key('colspec-by-name',@namest,$tgroup))">Entry's end
        column (<value-of select="@nameend"/>) must follow its start column 
        (<value-of select="@namest"/>).</assert>
      <assert test="empty(@namest) or exists(key('colspec-by-name',@namest,$tgroup))">No colspec is 
        named <value-of select="@namest"/>.</assert>
      <assert test="empty(@colname) or exists(key('colspec-by-name',@colname,$tgroup))">No colspec is 
        named <value-of select="@colname"/>.</assert>
      <assert test="empty(@nameend) or exists(@colname|@namest)">Entry is assigned an end
        column (<value-of select="@nameend"/>) but not a start column.</assert>
      <assert test="not(@colname != @namest)">Entry is assigned to column <value-of select="@colname"/>,
        so it can't start at column <value-of select="@namest"/>.
      </assert>

      <assert test="m:across(.)[1] &gt; (preceding-sibling::o:entry[1]/m:across(.)[last()],0)[1]">
        Entry must be assigned to a free column (after its preceding entries). 
      </assert>
      
      <report test="m:down(.) &gt; m:rowno(../../o:row[last()])">This entry doesn't fit into
        its <value-of select="local-name(../..)"/>.</report>
      
      <report test="(exists(@morerows) and
        (key('entry-by-row',m:down(.),$tgroup)/m:across(.)[last()] &gt; $tgroup/@cols))
        or empty($tgroup/@cols)" role="warning">
        A row in which this entry appears has too many entries.
      </report>
      <!-- the next rule will never fire for entries spanning columns: they always
           fit by virtue of being assigned a @nameend -->
      <report test="(m:across(.)[last()] &gt; $tgroup/@cols) or empty($tgroup/@cols)">
        Entry does not fit in row. (<value-of select="$tgroup/@cols"/> are allowed; entry
        is in column <value-of select="m:across(.)[last()]"/>.)
        <!-- Entry does not fit in row. (# columns are allowed; row ends in column #.)       -->
      </report>
      
      <assert test="empty(@char) or m:align(.)='char'" role="warning">@char is given, but alignment is not 'char'.</assert>
      <assert test="empty(@charoff) or m:align(.)='char'" role="warning">@charoff is given, but alignment is not 'char'.</assert>
      <assert test="empty(@charoff) or ((@charoff castable as xs:integer) and
        (@charoff &gt;= 0) and (@charoff &lt;= 100))">@charoff must be a whole number between 0 and 100.</assert>
      <assert test="not(m:align(.)='char') or exists(@char|m:colspec-for-entry(.)/@char)" role="warning">
        Entry is designated for character alignment, but no character (@char) is given on it or its colspec.
      </assert>
      <assert test="empty(@char) or not(@char != m:colspec-for-entry(.)/@char)">
        Entry is assigned an alignment character (<value-of select="@char"/>)
        different from its column's (<value-of select="m:colspec-for-entry(.)/@char"/>).</assert>
      <report test="exists(*) and (m:align(.)='char')" role="warning">With @align='char', markup of 
        entry contents (<value-of select="string-join(distinct-values(*/name()),', ')"/>) will be ignored.</report>
    </rule>
  </pattern>
</schema>