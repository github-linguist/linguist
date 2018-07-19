import StdEnv, StdIO

Start :: *World -> *World
Start world = startIO NDI Void (snd o openDialog undef hello) [] world
where
    hello = Dialog "" (TextControl "Goodbye, World!" [])
                                     [WindowClose (noLS closeProcess)]
