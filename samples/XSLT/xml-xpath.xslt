<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" />
  <xsl:template match="/">

    <!-- 1. first item element -->
    <xsl:text>
The first item element is</xsl:text>
    <xsl:value-of select="//item[1]" />

    <!-- 2. Print each price element -->
    <xsl:text>
The prices are: </xsl:text>
    <xsl:for-each select="//price">
      <xsl:text>
      </xsl:text>
      <xsl:copy-of select="." />
    </xsl:for-each>

    <!-- 3. Collect all the name elements -->
    <xsl:text>
The names are: </xsl:text>
    <xsl:copy-of select="//name" />
  </xsl:template>
</xsl:stylesheet>
