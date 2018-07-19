<xsl:template name="hanoi">
<xsl:param name="n"/>
<xsl:param name="from">left</xsl:param>
<xsl:param name="to">middle</xsl:param>
<xsl:param name="via">right</xsl:param>
  <xsl:if test="$n &gt; 0">
    <xsl:call-template name="hanoi">
      <xsl:with-param name="n"    select="$n - 1"/>
      <xsl:with-param name="from" select="$from"/>
      <xsl:with-param name="to"   select="$via"/>
      <xsl:with-param name="via"  select="$to"/>
    </xsl:call-template>
    <fo:block>
      <xsl:text>Move disk from </xsl:text>
      <xsl:value-of select="$from"/>
      <xsl:text> to </xsl:text>
      <xsl:value-of select="$to"/>
    </fo:block>
    <xsl:call-template name="hanoi">
      <xsl:with-param name="n"    select="$n - 1"/>
      <xsl:with-param name="from" select="$via"/>
      <xsl:with-param name="to"   select="$to"/>
      <xsl:with-param name="via"  select="$from"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>
