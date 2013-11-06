ruleset sample {
  meta {
    name "Hello World"
    description <<
Hello world
>>
    author "Phil Windley"
  }

  // just one rule
  rule hello {
    select when web pageview
    notify("Hello world!", "Just a note to say hello");
  }
}
