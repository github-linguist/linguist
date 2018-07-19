i code = True -- I am a comment.

{- I am also
   a comment. {-comments can be nested-}
   let u x = x x (this code not compiled)
   Are you? -}

-- |This is a Haddock documentation comment for the following code
i code = True
-- ^This is a Haddock documentation comment for the preceding code

{-|
  This is a Haddock documentation block comment
-}
i code = True
