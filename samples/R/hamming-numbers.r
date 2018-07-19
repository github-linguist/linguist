hamming=function(hamms,limit) {
  tmp=hamms
  for(h in c(2,3,5)) {
    tmp=c(tmp,h*hamms)
  }
  tmp=unique(tmp[tmp<=limit])
  if(length(tmp)>length(hamms)) {
    hamms=hamming(tmp,limit)
  }
  hamms
}
sort(hamming(1,limit=2^31)[-1])
