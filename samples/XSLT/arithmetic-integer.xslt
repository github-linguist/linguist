<xsl:template name="arithmetic">
  <xsl:param name="a">5</xsl:param>
  <xsl:param name="b">2</xsl:param>
  <fo:block>a + b = <xsl:value-of select="$a + $b"/></fo:block>
  <fo:block>a - b = <xsl:value-of select="$a - $b"/></fo:block>
  <fo:block>a * b = <xsl:value-of select="$a * $b"/></fo:block>
  <fo:block>a / b = <xsl:value-of select="round($a div $b)"/></fo:block>
  <fo:block>a mod b = <xsl:value-of select="$a mod $b"/></fo:block>
 </xsl:template>
