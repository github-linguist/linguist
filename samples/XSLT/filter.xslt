<xsl:for-each select="nodes[@value mod 2 = 0]">
  <xsl:value-of select="@value" />
</xsl:for-each>
