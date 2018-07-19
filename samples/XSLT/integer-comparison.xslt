<xsl:template name="compare">
  <xsl:param name="a" select="1"/>
  <xsl:param name="b" select="2"/>
  <fo:block>
  <xsl:choose>
    <xsl:when test="$a &lt; $b">a &lt; b</xsl:when>
    <xsl:when test="$a &gt; $b">a &gt; b</xsl:when>
    <xsl:when test="$a = $b">a = b</xsl:when>
  </xsl:choose>
  </fo:block>
