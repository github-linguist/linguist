<xsl:if test="true() or false()">
  True and false are returned by built-in XPath functions.
</xsl:if>
<xsl:if test="@myAttribute='true'">
  Node attributes set to "true" or "false" are just strings. Use string comparison to convert them to booleans.
</xsl:if>
<xsl:if test="@myAttribute or not($nodeSet)">
  Test an attribute for its presence (empty or not), or whether a node set is empty.
</xsl:if>
