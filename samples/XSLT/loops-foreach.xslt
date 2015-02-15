<fo:block font-weight="bold">Adults:</fo:block>
<xsl:for-each select="person[@age &gt;= 21]">
  <fo:block><xsl:value-of select="position()"/>. <xsl:value-of select="@name"/></fo:block>
</xsl:for-each>
