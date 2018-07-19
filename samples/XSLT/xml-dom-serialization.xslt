<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="yes" />
  <xsl:template match="/">   <!-- replace the root of the incoming document with our own model -->
    <xsl:element name="root">
      <xsl:element name="element">
        <xsl:text>Some text here</xsl:text>
      </xsl:element>
    </xsl:element>
  </xsl:template>
</xsl:stylesheet>
