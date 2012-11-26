
import List (intercalate,intersperse)
import Website.Skeleton
import Website.ColorScheme

addFolder folder lst =
  let add (x,y) = (x, folder ++ y ++ ".elm") in
  let f (n,xs) = (n, map add xs) in
  map f lst

elements = addFolder "Elements/"
  [ ("Primitives",
        [ ("Text"  , "HelloWorld")
        , ("Images", "Image")
        , ("Fitted Images", "FittedImage")
        , ("Videos", "Video")
        , ("Markdown", "Markdown")
        ])
  , ("Formatting",
        [ ("Size"    , "Size")
        , ("Opacity" , "Opacity")
        , ("Text"    , "Text")
        , ("Typeface", "Typeface")
        ])
  , ("Layout",
        [ ("Simple Flow", "FlowDown1a")
        , ("Flow Down"  , "FlowDown2")
        , ("Layers"     , "Layers")
        , ("Positioning", "Position")
        , ("Spacers"    , "Spacer")
        ])
  , ("Collage", [ ("Lines"     , "Lines")
                , ("Shapes"    , "Shapes")
                , ("Sprites"   , "Sprite")
                , ("Elements"  , "ToForm")
                , ("Colors"    , "Color")
                , ("Textures"  , "Texture")
                , ("Transforms", "Transforms")
                ])
  ]


functional = addFolder "Functional/"
  [ ("Recursion",
        [ ("Factorial"  , "Factorial")
        , ("List Length", "Length")
        , ("Zip"        , "Zip")
        , ("Quick Sort" , "QuickSort")
        ])
  , ("Functions",
        [ ("Anonymous Functions", "Anonymous")
        , ("Application"        , "Application")
        , ("Composition"        , "Composition")
        , ("Infix Operators"    , "Infix")
        ])
  , ("Higher-Order",
        [ ("Map"    , "Map")
        , ("Fold"   , "Sum")
        , ("Filter" , "Filter")
        , ("ZipWith", "ZipWith")
        ])
  , ("Data Types",
        [ ("Maybe", "Maybe")
        , ("Boolean Expressions", "BooleanExpressions")
        , ("Tree", "Tree")
        ])
  ]

reactive = addFolder "Reactive/"
  [ ("Mouse",  [ ("Position", "Position")
               , ("Presses"    , "IsDown")
               , ("Clicks"    , "CountClicks")
               , ("Position+Image", "ResizeYogi")
               , ("Position+Collage"    , "Transforms")
               -- , ("Hover"     , "IsAbove")
               ])
  ,("Keyboard",[ ("Keys Down"  , "KeysDown")
               , ("Key Presses", "CharPressed")
               ])
  , ("Window", [ ("Size", "ResizePaint")
               , ("Centering", "Centering")
               ])
  , ("Time",   [ ("Before and After", "Between")
               , ("Every"           , "Every")
               , ("Clock"           , "Clock")
               ])
  , ("Input",  [ ("Text Fields", "TextField")
               , ("Passwords"  , "Password")
               , ("Check Boxes", "CheckBox")
               , ("String Drop Down", "StringDropDown")
               , ("Drop Down", "DropDown")
               ])
  , ("Random", [ ("Randomize", "Randomize") ])
  , ("HTTP",   [ ("Zip Codes", "ZipCodes") ])
  , ("Filters",[ ("Sample", "SampleOn")
               , ("Keep If", "KeepIf")
               , ("Drop Repeats", "DropRepeats")
               ])
  ]

example (name, loc) = Text.link ("/edit/examples/" ++ loc) (toText name)
toLinks (title, links) =
  flow right [ width 130 (text $ toText "   " ++ italic (toText title))
             , text (intercalate (bold . Text.color accent4 $ toText "  &middot;  ") $ map example links)
             ]

insertSpace lst = case lst of { x:xs -> x : spacer 1 5 : xs ; [] -> [] }

subsection w (name,info) =
  flow down . insertSpace . intersperse (spacer 1 1) . map (width w) $
    (text . bold $ toText name) : map toLinks info

words = [markdown|

### Basic Examples

Each example listed below focuses on a single function or concept.
These examples demonstrate all of the basic building blocks of Elm.

|]

content w =
  words : map (subsection w) [ ("Display",elements), ("React",reactive), ("Compute",functional) ]

exampleSets w = flow down . map (width w) . intersperse (plainText " ") $ content w

main = lift (skeleton exampleSets) Window.width
