: replace-text ( button text -- )
    [ drop children>> pop drop ] [ >label add-gadget drop ] 2bi;
: present-locations ( loc1 loc2 -- string )
    [
      first2 [ number>string ] bi@ "," glue
    ] bi@ ";" glue ;
: example ( -- ) "click me"
[
  dup hand-rel ! location relative to the button
  hand-loc get ! location relative to the window
  present-locations replace-text
]
<border-button> gadget. ;
