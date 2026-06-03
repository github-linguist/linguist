suite "Header" {
  test "it has a logo" {
    with Test.Html {
      <Header/>
      |> start()
      |> assertElementExists("[class*=brand] svg")
    }
  }

  test "it has a brand name" {
    with Test.Html {
      <Header/>
      |> start()
      |> assertTextOf("span", "Conduit")
    }
  }

  test "it renders the sign in link" {
    with Test.Html {
      <Header/>
      |> start()
      |> assertTextOf("div[class*=links] a:first-child", "Sign in")
    }
  }

  test "it renders the sign up link" {
    with Test.Html {
      <Header/>
      |> start()
      |> assertTextOf("div[class*=links] a:last-child", "Sign up")
    }
  }
}
