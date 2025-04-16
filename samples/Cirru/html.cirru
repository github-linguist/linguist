
doctype

html
  head
    title $ = a
    link (:rel a) $ :href b
    link (:rel icon) $ :href a
    meta $ :charset utf-8
  body
    #about
      .line
        a (:href a) $ = b
        span $ = " "
        a (:href a) $ = b
      .line $ span $ = a
    #list
      .year $ = 2014
      .month $ = May
      .post $ a.link (:href) $ =
      .month $ = Apr
      .post $ a.link (:href a) $ = b