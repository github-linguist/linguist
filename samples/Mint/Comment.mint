record Comment {
  createdAt : Time,
  updatedAt : Time,
  author : Author,
  body : String,
  id : Number
}

module Comment {
  fun empty : Comment {
    {
      author = Author.empty(),
      createdAt = Time.now(),
      updatedAt = Time.now(),
      body = "",
      id = 0
    }
  }

  fun decode (object : Object) : Result(Object.Error, Comment) {
    decode object as Comment
  }

  fun fromResponse (object : Object) : Result(Object.Error, Comment) {
    with Object.Decode {
      field("comment", decode, object)
    }
  }
}
