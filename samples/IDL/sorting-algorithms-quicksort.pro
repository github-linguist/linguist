function qs, arr
  if (count = n_elements(arr)) lt 2 then return,arr
  pivot = total(arr) / count ; use the average for want of a better choice
  return,[qs(arr[where(arr le pivot)]),qs(arr[where(arr gt pivot)])]
 end
