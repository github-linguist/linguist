report zdate.
data: lv_month type string,
      lv_weekday type string,
      lv_date type string,
      lv_day type c.

call function 'DATE_COMPUTE_DAY'
  exporting date = sy-datum
  importing day  = lv_day.
select single ltx from t247 into lv_month
  where spras = sy-langu and
  mnr = sy-datum+4(2).

select single langt from t246 into lv_weekday
  where sprsl = sy-langu and
  wotnr = lv_day.

concatenate lv_weekday ', ' lv_month ' ' sy-datum+6(2) ', ' sy-datum(4) into lv_date respecting blanks.
write lv_date.
concatenate sy-datum(4) '-' sy-datum+4(2) '-' sy-datum+6(2) into lv_date.
write / lv_date.
