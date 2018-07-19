report zdot_product
data: lv_n type i,
      lv_sum type i,
      lt_a type standard table of i,
      lt_b type standard table of i.

append: '1' to lt_a, '3' to lt_a, '-5' to lt_a.
append: '4' to lt_b, '-2' to lt_b, '-1' to lt_b.
describe table lt_a lines lv_n.

perform dot_product using lt_a lt_b lv_n changing lv_sum.

write lv_sum left-justified.

form dot_product using it_a like lt_a
                       it_b like lt_b
                       iv_n type i
                 changing
                       ev_sum type i.
  field-symbols: <wa_a> type i, <wa_b> type i.

  do iv_n times.
    read table: it_a assigning <wa_a> index sy-index, it_b assigning <wa_b> index sy-index.
    lv_sum = lv_sum + ( <wa_a> * <wa_b> ).
  enddo.
endform.
