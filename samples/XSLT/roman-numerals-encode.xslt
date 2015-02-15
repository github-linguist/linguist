<xsl:stylesheet  version="1.0"    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/data/number">
        <xsl:call-template name="for">
               <xsl:with-param name="stop">13</xsl:with-param>
        	<xsl:with-param name="value"><xsl:value-of select="@value"></xsl:value-of></xsl:with-param>
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="for">
      <xsl:param name="start">1</xsl:param>
      <xsl:param name="stop">1</xsl:param>
      <xsl:param name="step">1</xsl:param>
      <xsl:param name="value">1</xsl:param>
      <xsl:text/>
      <xsl:choose>
      <xsl:when test="($value &gt; /data/roman
/numeral[@pos=$start]/@value or $value = /data/roman
/numeral[@pos=$start]/@value) ">
          <xsl:value-of select="/data/roman
/numeral[@pos=$start]/@letter"/>
          <xsl:call-template name="for">
          <xsl:with-param name="stop">
            <xsl:value-of select="$stop"/>
          </xsl:with-param>
          <xsl:with-param name="start">
            <xsl:value-of select="$start"/>
          </xsl:with-param>
          <xsl:with-param name="value">
          	<xsl:value-of select="$value - /data/roman/numeral[@pos=$start]/@value"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$start &lt; $stop">
        <xsl:call-template name="for">
          <xsl:with-param name="stop">
            <xsl:value-of select="$stop"/>
          </xsl:with-param>
          <xsl:with-param name="start">
            <xsl:value-of select="$start + $step"/>
          </xsl:with-param>
          <xsl:with-param name="value">
          	<xsl:value-of select="$value"/>
          </xsl:with-param>
        </xsl:call-template>
        </xsl:if>
      </xsl:otherwise>
      </xsl:choose>
    </xsl:template>
</xsl:stylesheet>
