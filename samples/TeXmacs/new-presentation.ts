<TeXmacs|1.0.6.14>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|presentation|1.0|presentation|1.0>

    <\src-purpose>
      European-style numbering.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <use-package|varsession|alt-colors>

  <\active*>
    <\src-comment>
      Customizations
    </src-comment>
  </active*>

  <assign|font-family|ss>

  <assign|name|<macro|x|<with|font-family|rm|font-shape|small-caps|<arg|x>>>>

  <assign|bg-color|<pattern|paper-ridged-light.png|*3/5|*3/5|#f4eee8>>

  <assign|monochrome-bg-color|#f4eee8>

  <assign|ornament-border|2ln>

  <assign|ornament-color|<pattern|pine.png|*3/5|*3/5|#e0b050>>

  <assign|ornament-sunny-color|#f0e0c0>

  <assign|ornament-shadow-color|#c07055>

  <assign|ornament-hpadding|1spc>

  <assign|ornament-vpadding|1spc>

  <assign|img|<macro|body|<with|ornament-color|white|<ornament|<arg|body>>>>>

  <assign|tit|<macro|body|<with|ornament-color|<pattern|paper-manila-medium.png|*3/5|*3/5|#e0e0e0>|<ornament|<with|font-series|bold|color|dark
  brown|<tabular*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|2|2|cell-hpart|1>|<cwith|1|1|1|-1|cell-valign|c>|<cwith|1|-1|1|-1|cell-lsep|0px>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|1|-1|1|-1|cell-bsep|0px>|<cwith|1|-1|1|-1|cell-tsep|0px>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<twith|table-lsep|0px>|<twith|table-rsep|0px>|<twith|table-bsep|0px>|<twith|table-tsep|0px>|<table|<row|<cell|<postscript|$HOME/.TeXmacs/misc/pixmaps/right-head.png|/5|/5||||>>|<cell|<arg|body>>|<cell|<postscript|$HOME/.TeXmacs/misc/pixmaps/left-head.png|/5|/5||||>>>>>>>>>>>

  <assign|tit|<macro|body|<with|ornament-color|<pattern|paper-manila-medium.png|*3/5|*3/5|#e0e0e0>|<ornament|<with|font-series|bold|color|dark
  brown|<postscript|$HOME/.TeXmacs/misc/pixmaps/right-head.png|/5|/5||||>><htab|5mm><move|<with|font-series|bold|math-font-series|bold|<large|<arg|body>>>|0fn|0.333fn><htab|5mm><postscript|$HOME/.TeXmacs/misc/pixmaps/left-head.png|/5|/5||||>>>>>

  <assign|tit|<macro|body|<with|ornament-color|<pattern|paper-manila-medium.png|*3/5|*3/5|#e0e0e0>|<ornament|<with|font-series|bold|color|dark
  brown|<postscript|$HOME/.TeXmacs/misc/pixmaps/right-head.png|/4.5|/4.5||||>><htab|5mm><move|<with|font-series|bold|math-font-series|bold|<large|<with|color|dark
  brown|<arg|body>>>>|0fn|0.333fn><htab|5mm><postscript|$HOME/.TeXmacs/misc/pixmaps/left-head.png|/4.5|/4.5||||>>>>>

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|0.5fn><with|par-first|-2.5fn|<yes-indent>><resize|<arg|x>|r-2.5fn||r+0.0fn|>>>>

  <assign|render-bibitem|<macro|text|<aligned-item|<transform-bibitem|<arg|text>>>>>

  <assign|strong-color|#504000>

  <assign|greyed-math-color|#c08080>

  <assign|greyed|<macro|x|<with|color|#c08080|math-color|<value|greyed-math-color>|<arg|x>>>>

  <assign|granite|<macro|x|<with|ornament-color|<pattern|granite-dark.png|*3/5|*3/5|#101010>|color|white|strong-color|#f0ffb0|math-color|#ffd4c0|ornament-sunny-color|light
  grey|ornament-shadow-color|dark grey|<ornament|<arg|x>>>>>

  <assign|pine|<macro|x|<with|ornament-color|<pattern|pine.png|*3/5|*3/5|#e0b050>|strong-color|#0c3000|math-color|#500000|<ornament|<arg|x>>>>>

  <assign|input-color|<pattern|paper-ridged-medium.png|*3/5|*3/5|#e8dcd0>>

  <assign|fold-bar-color|<pattern|wood-light.png|*3/5|*3/5|#e0b050>>

  <assign|fold-title-color|<pattern|pine.png|*3/5|*3/5|#e0b050>>

  <assign|folded-body|<macro|body|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>