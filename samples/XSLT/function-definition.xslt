<xsl:template name="product">
  <xsl:param name="a" select="2"/>
  <xsl:param name="b" select="3"/>
  <fo:block>product = <xsl:value-of select="$a * $b"/></fo:block>
</xsl:template>

<xsl:call-template name="product">
  <xsl:with-param name="a">4</xsl:with-param>
  <xsl:with-param name="b">5</xsl:with-param>
</xsl:call-template>
