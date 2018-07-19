<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- XSLT Mandelbrot - written by Joel Yliluoma 2007, http://iki.fi/bisqwit/ -->

<xsl:output method="html" indent="no"
  doctype-public="-//W3C//DTD HTML 4.01//EN"
  doctype-system="http://www.w3.org/TR/REC-html40/strict.dtd"
 />

<xsl:template match="/fractal">
 <html>
  <head>
   <title>XSLT fractal</title>
   <style type="text/css">
body { color:#55F; background:#000 }
pre { font-family:monospace; font-size:7px }
pre span { background:<xsl:value-of select="background" /> }
   </style>
  </head>
  <body>
   <div style="position:absolute;top:20px;left:20em">
    Copyright © 1992,2007 Joel Yliluoma
    (<a href="http://iki.fi/bisqwit/">http://iki.fi/bisqwit/</a>)
   </div>
   <h1 style="margin:0px">XSLT fractal</h1>
   <pre><xsl:call-template name="bisqwit-mandelbrot" /></pre>
  </body>
 </html>
</xsl:template>

<xsl:template name="bisqwit-mandelbrot"
  ><xsl:call-template name="bisqwit-mandelbrot-line">
   <xsl:with-param name="y" select="y/min"/>
  </xsl:call-template
></xsl:template>

<xsl:template name="bisqwit-mandelbrot-line"
 ><xsl:param name="y"
 /><xsl:call-template name="bisqwit-mandelbrot-column">
  <xsl:with-param name="x" select="x/min"/>
  <xsl:with-param name="y" select="$y"/>
 </xsl:call-template
 ><xsl:if test="$y < y/max"
  ><br
  /><xsl:call-template name="bisqwit-mandelbrot-line">
   <xsl:with-param name="y" select="$y + y/step"/>
  </xsl:call-template
 ></xsl:if
></xsl:template>

<xsl:template name="bisqwit-mandelbrot-column"
 ><xsl:param name="x"
 /><xsl:param name="y"
 /><xsl:call-template name="bisqwit-mandelbrot-slot">
  <xsl:with-param name="x" select="$x" />
  <xsl:with-param name="y" select="$y" />
  <xsl:with-param name="zr" select="$x" />
  <xsl:with-param name="zi" select="$y" />
 </xsl:call-template
 ><xsl:if test="$x < x/max"
  ><xsl:call-template name="bisqwit-mandelbrot-column">
   <xsl:with-param name="x" select="$x + x/step"/>
   <xsl:with-param name="y" select="$y" />
  </xsl:call-template
 ></xsl:if
></xsl:template>

<xsl:template name="bisqwit-mandelbrot-slot"
><xsl:param name="x"
 /><xsl:param name="y"
 /><xsl:param name="zr"
 /><xsl:param name="zi"
 /><xsl:param name="iter" select="0"
 /><xsl:variable name="zrsqr" select="($zr * $zr)"
 /><xsl:variable name="zisqr" select="($zi * $zi)"
 /><xsl:choose>
  <xsl:when test="(4*scale*scale >= $zrsqr + $zisqr) and (maxiter > $iter+1)"
   ><xsl:call-template name="bisqwit-mandelbrot-slot">
    <xsl:with-param name="x" select="$x" />
    <xsl:with-param name="y" select="$y" />
    <xsl:with-param name="zi" select="(2 * $zr * $zi) div scale + $y" />
    <xsl:with-param name="zr" select="($zrsqr - $zisqr) div scale + $x" />
    <xsl:with-param name="iter" select="$iter + 1" />
   </xsl:call-template
  ></xsl:when>
  <xsl:otherwise
   ><xsl:variable name="magnitude" select="magnitude[@value=$iter]"
    /><span style="color:{$magnitude/color}"
   ><xsl:value-of select="$magnitude/symbol"
  /></span></xsl:otherwise>
 </xsl:choose
></xsl:template>

</xsl:stylesheet>
