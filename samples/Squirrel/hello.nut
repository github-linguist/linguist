class Hello
{
  static hello = 
  {
    en = "hello"
    es = "hola"
    fr = "bonjour"
    zh = "nǐ hǎo"
  }
  lang = null
  
  constructor(lang)
  {
    this.lang = lang
  }
  
  function sayit()
  {
    ::print(hello[lang])
  }
}

hi <- Hello("fr")
hi.sayit() // prints "bonjour"
