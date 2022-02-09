enum UserStatus {
  LoggedIn(User)
  LoggedOut
}

enum Page {
  Settings
  NotFound
  Article
  Initial
  Profile
  Editor
  SignIn
  SignUp
  Home
}

store Application {
  state user : UserStatus = UserStatus::LoggedOut
  state page : Page = Page::Initial

  fun initializeWithPage (page : Page) : Promise(Never, Void) {
    sequence {
      setPage(page)
      initialize()
    }
  }

  fun initialize : Promise(Never, Void) {
    sequence {
      Http.abortAll()

      try {
        data =
          Storage.Local.get("user")

        object =
          Json.parse(data)
          |> Maybe.toResult("")

        currentUser =
          decode object as User

        next { user = UserStatus::LoggedIn(currentUser) }
      } catch Storage.Error => error {
        next { user = UserStatus::LoggedOut }
      } catch Object.Error => error {
        next { user = UserStatus::LoggedOut }
      } catch String => error {
        next { user = UserStatus::LoggedOut }
      }
    }
  }

  fun setUser (user : User) : Promise(Never, Void) {
    next { user = UserStatus::LoggedIn(user) }
  }

  fun logout : Promise(Never, Void) {
    sequence {
      Storage.Local.remove("user")

      resetStores()

      next { user = UserStatus::LoggedOut }

      Window.navigate("/")
    } catch Storage.Error => error {
      Promise.never()
    }
  }

  fun login (user : User) : Promise(Never, Void) {
    sequence {
      Storage.Local.set("user", Json.stringify(encode user))

      resetStores()

      next { user = UserStatus::LoggedIn(user) }

      Window.navigate("/")
    } catch Storage.Error => error {
      Promise.never()
    }
  }

  fun resetStores : Promise(Never, Void) {
    parallel {
      Stores.Articles.reset()
      Stores.Comments.reset()
      Stores.Article.reset()
    }
  }

  fun setPage (page : Page) : Promise(Never, Void) {
    next { page = page }
  }
}
