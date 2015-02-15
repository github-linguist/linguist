{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

fact n = product [1..n]

main = print $([|fact 10|])
