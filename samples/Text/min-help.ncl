THE_URL:file://localhost/Users/hubery/Public/ucar/Document/Functions/Built-in/min.shtml
THE_TITLE:min
   NCL Home > Documentation > Functions > General applied math

min

   Computes the minimum value of a multi-dimensional array.

Prototype

        function min (
                value  : numeric
        )

        return_val [1] :  numeric

Arguments

   value

   An array of one or more numeric values of any dimension.

Return value

   Returns a scalar of the same type as value.

Description

   This function returns the minimum value for an array of any dimensionality. Missing values are ignored; a missing value
   is returned only if all values are missing.

See Also

   max, minind, maxind, dim_min, dim_max, dim_min_n, dim_max_n

Examples

   Example 1

  f = (/2.1, 3.2, 4.3, 5.4, 6.5, 7.6, 8.7, 9.8/)
  min_f = min(f)
  print(min_f)        ; Should be 2.1


   Â©2015 UCAR | Privacy Policy | Terms of Use | Contact the Webmaster | Sponsored by NSF
