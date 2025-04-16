  λ ( xs
    : List
      { cores             : Natural
      , host              : Text
      , key               : Text
      , mandatoryFeatures : List Text
      , platforms         :
          List
          < AArch64_Linux  : {}
          | ARMv5tel_Linux : {}
          | ARMv7l_Linux   : {}
          | I686_Cygwin    : {}
          | I686_Linux     : {}
          | MIPS64el_Linux : {}
          | PowerPC_Linux  : {}
          | X86_64_Cygwin  : {}
          | X86_64_Darwin  : {}
          | X86_64_FreeBSD : {}
          | X86_64_Linux   : {}
          | X86_64_Solaris : {}
          >
      , speedFactor       : Natural
      , supportedFeatures : List Text
      , user              : Optional Text
      }
    )
→ List/fold
  { cores             : Natural
  , host              : Text
  , key               : Text
  , mandatoryFeatures : List Text
  , platforms         :
      List
      < AArch64_Linux  : {}
      | ARMv5tel_Linux : {}
      | ARMv7l_Linux   : {}
      | I686_Cygwin    : {}
      | I686_Linux     : {}
      | MIPS64el_Linux : {}
      | PowerPC_Linux  : {}
      | X86_64_Cygwin  : {}
      | X86_64_Darwin  : {}
      | X86_64_FreeBSD : {}
      | X86_64_Linux   : {}
      | X86_64_Solaris : {}
      >
  , speedFactor       : Natural
  , supportedFeatures : List Text
  , user              : Optional Text
  }
  xs
  Text
  (   λ ( x
        : { cores             : Natural
          , host              : Text
          , key               : Text
          , mandatoryFeatures : List Text
          , platforms         :
              List
              < AArch64_Linux  : {}
              | ARMv5tel_Linux : {}
              | ARMv7l_Linux   : {}
              | I686_Cygwin    : {}
              | I686_Linux     : {}
              | MIPS64el_Linux : {}
              | PowerPC_Linux  : {}
              | X86_64_Cygwin  : {}
              | X86_64_Darwin  : {}
              | X86_64_FreeBSD : {}
              | X86_64_Linux   : {}
              | X86_64_Solaris : {}
              >
          , speedFactor       : Natural
          , supportedFeatures : List Text
          , user              : Optional Text
          }
        )
    → λ(y : Text)
    →     (     Optional/fold
                Text
                x.user
                Text
                (λ(user : Text) → user ++ "@" ++ x.host ++ "")
                x.host
            ++  " "
            ++  ( merge
                  { Empty    = λ(_ : {}) → ""
                  , NonEmpty = λ(result : Text) → result
                  }
                  ( List/fold
                    < AArch64_Linux  : {}
                    | ARMv5tel_Linux : {}
                    | ARMv7l_Linux   : {}
                    | I686_Cygwin    : {}
                    | I686_Linux     : {}
                    | MIPS64el_Linux : {}
                    | PowerPC_Linux  : {}
                    | X86_64_Cygwin  : {}
                    | X86_64_Darwin  : {}
                    | X86_64_FreeBSD : {}
                    | X86_64_Linux   : {}
                    | X86_64_Solaris : {}
                    >
                    x.platforms
                    < Empty : {} | NonEmpty : Text >
                    (   λ ( element
                          : < AArch64_Linux  : {}
                            | ARMv5tel_Linux : {}
                            | ARMv7l_Linux   : {}
                            | I686_Cygwin    : {}
                            | I686_Linux     : {}
                            | MIPS64el_Linux : {}
                            | PowerPC_Linux  : {}
                            | X86_64_Cygwin  : {}
                            | X86_64_Darwin  : {}
                            | X86_64_FreeBSD : {}
                            | X86_64_Linux   : {}
                            | X86_64_Solaris : {}
                            >
                          )
                      → λ(status : < Empty : {} | NonEmpty : Text >)
                      → merge
                        { Empty    =
                              λ(_ : {})
                            → < Empty : {} | NonEmpty : Text >.NonEmpty (
                                  merge
                                  { AArch64_Linux  = λ(_ : {}) → "aarch64-linux"
                                  , ARMv5tel_Linux =
                                      λ(_ : {}) → "armv5tel-linux"
                                  , ARMv7l_Linux   = λ(_ : {}) → "armv7l-linux"
                                  , I686_Cygwin    = λ(_ : {}) → "i686-cygwin"
                                  , I686_Linux     = λ(_ : {}) → "i686-linux"
                                  , MIPS64el_Linux =
                                      λ(_ : {}) → "mips64el-linux"
                                  , PowerPC_Linux  = λ(_ : {}) → "powerpc-linux"
                                  , X86_64_Cygwin  = λ(_ : {}) → "x86_64-cygwin"
                                  , X86_64_Darwin  = λ(_ : {}) → "x86_64-darwin"
                                  , X86_64_FreeBSD =
                                      λ(_ : {}) → "x86_64-freebsd"
                                  , X86_64_Linux   = λ(_ : {}) → "x86_64-linux"
                                  , X86_64_Solaris =
                                      λ(_ : {}) → "x86_64-solaris"
                                  }
                                  element
                              )
                        , NonEmpty =
                              λ(result : Text)
                            → < Empty : {} | NonEmpty : Text >.NonEmpty (
                                      ( merge
                                        { AArch64_Linux  =
                                            λ(_ : {}) → "aarch64-linux"
                                        , ARMv5tel_Linux =
                                            λ(_ : {}) → "armv5tel-linux"
                                        , ARMv7l_Linux   =
                                            λ(_ : {}) → "armv7l-linux"
                                        , I686_Cygwin    =
                                            λ(_ : {}) → "i686-cygwin"
                                        , I686_Linux     =
                                            λ(_ : {}) → "i686-linux"
                                        , MIPS64el_Linux =
                                            λ(_ : {}) → "mips64el-linux"
                                        , PowerPC_Linux  =
                                            λ(_ : {}) → "powerpc-linux"
                                        , X86_64_Cygwin  =
                                            λ(_ : {}) → "x86_64-cygwin"
                                        , X86_64_Darwin  =
                                            λ(_ : {}) → "x86_64-darwin"
                                        , X86_64_FreeBSD =
                                            λ(_ : {}) → "x86_64-freebsd"
                                        , X86_64_Linux   =
                                            λ(_ : {}) → "x86_64-linux"
                                        , X86_64_Solaris =
                                            λ(_ : {}) → "x86_64-solaris"
                                        }
                                        element
                                      )
                                  ++  ","
                                  ++  result
                              )
                        }
                        status
                        : < Empty : {} | NonEmpty : Text >
                    )
                   (< Empty : {} | NonEmpty : Text >.Empty {=})
                  )
                  : Text
                )
            ++  " "
            ++  x.key
            ++  " "
            ++  Integer/show (Natural/toInteger x.cores)
            ++  " "
            ++  Integer/show (Natural/toInteger x.speedFactor)
            ++  " "
            ++  ( merge
                  { Empty    = λ(_ : {}) → ""
                  , NonEmpty = λ(result : Text) → result
                  }
                  ( List/fold
                    Text
                    x.supportedFeatures
                    < Empty : {} | NonEmpty : Text >
                    (   λ(element : Text)
                      → λ(status : < Empty : {} | NonEmpty : Text >)
                      → merge
                        { Empty    =
                            λ(_ : {}) →
                            (< Empty : {} | NonEmpty : Text >.NonEmpty element)
                        , NonEmpty =
                              λ(result : Text)
                            → < Empty : {} | NonEmpty : Text >.NonEmpty (
                                  element ++ "," ++ result
                              )
                        }
                        status
                        : < Empty : {} | NonEmpty : Text >
                    )
                   (< Empty : {} | NonEmpty : Text >.Empty {=})
                  )
                  : Text
                )
            ++  " "
            ++  ( merge
                  { Empty    = λ(_ : {}) → ""
                  , NonEmpty = λ(result : Text) → result
                  }
                  ( List/fold
                    Text
                    x.mandatoryFeatures
                    < Empty : {} | NonEmpty : Text >
                    (   λ(element : Text)
                      → λ(status : < Empty : {} | NonEmpty : Text >)
                      → merge
                        { Empty    =
                            λ(_ : {}) →
                            < Empty : {} | NonEmpty : Text >.NonEmpty element
                        , NonEmpty =
                              λ(result : Text)
                            → < Empty : {} | NonEmpty : Text >.NonEmpty (
                                  element ++ "," ++ result
                              )
                        }
                        status
                        : < Empty : {} | NonEmpty : Text >
                    )
                   (< Empty : {} | NonEmpty : Text >.Empty {=})
                  )
                  : Text
                )
            ++  "\n"
          )
      ++  y
  )
  ""
