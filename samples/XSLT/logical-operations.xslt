<xsl:template name="logic">
  <xsl:param name="a" select="true()"/>
  <xsl:param name="b" select="false()"/>
  <fo:block>a and b = <xsl:value-of select="$a and $b"/></fo:block>
  <fo:block>a or b = <xsl:value-of select="$a or $b"/></fo:block>
  <fo:block>not a = <xsl:value-of select="not($a)"/></fo:block>
 </xsl:template>
