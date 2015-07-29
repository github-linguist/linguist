THE_URL:file://localhost/Users/hubery/Public/ucar/Document/Functions/Contributed/zonalAve.shtml
THE_TITLE:zonalAve
   NCL Home > Documentation > Functions > General applied math

zonalAve

   Computes a zonal average of the input array.

Prototype

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

        function zonalAve (
                x  : numeric
        )

        return_val  :  typeof(x)

Arguments

   x

   An array of any size and type.

Return value

   The results are returned in an array of the same type and one dimension smaller than x. Metadata are preserved.

Description

   This function computes a zonal average of the input array x. If the input array has a "long_name" or "short_name"
   attribute, it will be updated.


   Â©2015 UCAR | Privacy Policy | Terms of Use | Contact the Webmaster | Sponsored by NSF
