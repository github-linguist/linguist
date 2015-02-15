report zday_of_week
data: lv_start type i value 2007,
      lv_n type i value 114,
      lv_date type sy-datum,
      lv_weekday type string,
      lv_day type c,
      lv_year type n length 4.

write 'December 25 is a Sunday in: '.
do lv_n times.
   lv_year = lv_start + sy-index.
   concatenate lv_year '12' '25' into lv_date.
   call function 'DATE_COMPUTE_DAY'
    exporting date = lv_date
    importing day  = lv_day.

   select single langt from t246 into lv_weekday
     where sprsl = sy-langu and
     wotnr = lv_day.

   if lv_weekday eq 'Sunday'.
     write / lv_year.
   endif.
enddo.
