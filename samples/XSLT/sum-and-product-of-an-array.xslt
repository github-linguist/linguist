<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" />

  <xsl:template name="sum-prod">
    <xsl:param name="values" />
    <xsl:param name="sum"  select="0" />
    <xsl:param name="prod" select="1" />
    <xsl:choose>
      <xsl:when test="not($values)">
        <xsl:text>
Sum: </xsl:text>
        <xsl:value-of select="$sum" />
        <xsl:text>
Product: </xsl:text>
        <xsl:value-of select="$prod" />
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="sum-prod">
          <xsl:with-param name="values" select="$values[position() > 1]" />
          <xsl:with-param name="sum"  select="$sum  + $values[1]" />
          <xsl:with-param name="prod" select="$prod * $values[1]" />
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="/">
     <xsl:text>
Sum (built-in): </xsl:text>
    <xsl:value-of select="sum(//price)" />

    <xsl:call-template name="sum-prod">
      <xsl:with-param name="values" select="//price" />
    </xsl:call-template>
  </xsl:template>
</xsl:stylesheet>
