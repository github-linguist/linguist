(: -------------------------------------------------------------------------------------

    xproc.xqm - core xqm contains entry points, primary eval-step function and
    control functions.

 ---------------------------------------------------------------------------------------- :)
xquery version "3.0"  encoding "UTF-8";

module namespace xproc = "http://xproc.net/xproc";

 (: declare namespaces :)
 declare namespace p="http://www.w3.org/ns/xproc";
 declare namespace c="http://www.w3.org/ns/xproc-step";
 declare namespace err="http://www.w3.org/ns/xproc-error";

 (: module imports :)
(:  import module namespace util = "http://xproc.net/xproc/util" at "util1.xqm"; :)
 import module namespace const = "http://xproc.net/xproc/const" at "const.xqm";
 import module namespace parse = "http://xproc.net/xproc/parse" at "parse.xqm";
 import module namespace u = "http://xproc.net/xproc/util" at "util.xqm";

 (: declare options :)
 declare boundary-space preserve;
 declare option saxon:output "indent=yes";

 (: declare functions :)
 declare variable $xproc:run-step       := xproc:run#6;
 declare variable $xproc:parse-and-eval := ();
 declare variable $xproc:declare-step   := ();
 declare variable $xproc:choose         := ();
 declare variable $xproc:try            := ();
 declare variable $xproc:catch          := ();
 declare variable $xproc:group          := ();
 declare variable $xproc:for-each       := ();
 declare variable $xproc:viewport       := ();
 declare variable $xproc:library        := ();
 declare variable $xproc:pipeline       := ();
 declare variable $xproc:variable       := ();


 (: list all declared namespaces :)
 (: -------------------------------------------------------------------------- :)
 declare function xproc:enum-namespaces($pipeline){
 (: -------------------------------------------------------------------------- :)
    <namespace name="{$pipeline/@name}">{u:enum-ns(<dummy>{$pipeline}</dummy>)}</namespace>
 };

 (: entry point :)
 (: -------------------------------------------------------------------------- :)
 declare function xproc:run($pipeline,$stdin,$dflag,$tflag,$bindings,$options){
 (: -------------------------------------------------------------------------- :)

 (: STEP I: preprocess :)
 let $validate   := ()
 let $namespaces := xproc:enum-namespaces($pipeline)
 let $parse      := parse:explicit-bindings( parse:AST(parse:explicit-name(parse:explicit-type($pipeline))))
 let $ast        := element p:declare-step {$parse/@*,
       parse:pipeline-step-sort( $parse/*, () )
     }

 (: STEP II: eval AST :)
 let $eval_result := ()

 (: STEP III: serialize and return results :)
 let $serialized_result := $pipeline

 return 
   $serialized_result
 };

