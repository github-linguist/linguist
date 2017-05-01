-- | This is a library which colourises Haskell code.
--   It currently has six output formats:
--
-- * ANSI terminal codes
--
-- * LaTeX macros
--
-- * HTML 3.2 with font tags
--
-- * HTML 4.01 with external CSS.
--
-- * XHTML 1.0 with internal CSS.
--
-- * mIRC chat client colour codes.
--
module Language.Haskell.HsColour (Output(..), ColourPrefs(..),
                                  hscolour) where

import Language.Haskell.HsColour.Colourise  (ColourPrefs(..))
import qualified Language.Haskell.HsColour.TTY        as TTY
import qualified Language.Haskell.HsColour.HTML       as HTML
import qualified Language.Haskell.HsColour.CSS        as CSS
import qualified Language.Haskell.HsColour.ACSS       as ACSS 
import qualified Language.Haskell.HsColour.InlineCSS  as ICSS
import qualified Language.Haskell.HsColour.LaTeX      as LaTeX
import qualified Language.Haskell.HsColour.MIRC       as MIRC
import Data.List(mapAccumL, isPrefixOf)
import Data.Maybe
import Language.Haskell.HsColour.Output
--import Debug.Trace

-- | Colourise Haskell source code with the given output format.
hscolour :: Output      -- ^ Output format.
         -> ColourPrefs -- ^ Colour preferences (for formats that support them).
         -> Bool        -- ^ Whether to include anchors.
         -> Bool        -- ^ Whether output document is partial or complete.
         -> String	-- ^ Title for output.
         -> Bool        -- ^ Whether input document is literate haskell or not
         -> String      -- ^ Haskell source code.
         -> String      -- ^ Coloured Haskell source code.
hscolour output pref anchor partial title False =
        (if partial then id else top'n'tail output title) .
        hscolour' output pref anchor
hscolour output pref anchor partial title True  =
        (if partial then id else top'n'tail output title) .
        concatMap chunk . joinL . classify . inlines
  where
    chunk (Code c) = hscolour' output pref anchor c
    chunk (Lit c)  = c

-- | The actual colourising worker, despatched on the chosen output format.
hscolour' :: Output      -- ^ Output format.
          -> ColourPrefs -- ^ Colour preferences (for formats that support them)
          -> Bool        -- ^ Whether to include anchors.
          -> String      -- ^ Haskell source code.
          -> String      -- ^ Coloured Haskell source code.
hscolour' TTY       pref _      = TTY.hscolour     pref
hscolour' (TTYg tt) pref _      = TTY.hscolourG tt pref
hscolour' MIRC      pref _      = MIRC.hscolour    pref
hscolour' LaTeX     pref _      = LaTeX.hscolour   pref
hscolour' HTML      pref anchor = HTML.hscolour    pref anchor
hscolour' CSS       _    anchor = CSS.hscolour          anchor
hscolour' ICSS      pref anchor = ICSS.hscolour    pref anchor
hscolour' ACSS      _    anchor = ACSS.hscolour         anchor

-- | Choose the right headers\/footers, depending on the output format.
top'n'tail :: Output           -- ^ Output format
           -> String           -- ^ Title for output
           -> (String->String) -- ^ Output transformer
top'n'tail TTY   _     = id
top'n'tail (TTYg _) _  = id
top'n'tail MIRC  _     = id
top'n'tail LaTeX title = LaTeX.top'n'tail title
top'n'tail HTML  title = HTML.top'n'tail title
top'n'tail CSS   title = CSS.top'n'tail  title
top'n'tail ICSS  title = ICSS.top'n'tail title
top'n'tail ACSS  title = CSS.top'n'tail  title

-- | Separating literate files into code\/comment chunks.
data Lit = Code {unL :: String} | Lit {unL :: String} deriving (Show)

-- Re-implementation of 'lines', for better efficiency (but decreased laziness).
-- Also, importantly, accepts non-standard DOS and Mac line ending characters.
-- And retains the trailing '\n' character in each resultant string.
inlines :: String -> [String]
inlines s = lines' s id
  where
  lines' []             acc = [acc []]
  lines' ('\^M':'\n':s) acc = acc ['\n'] : lines' s id	-- DOS
--lines' ('\^M':s)      acc = acc ['\n'] : lines' s id	-- MacOS
  lines' ('\n':s)       acc = acc ['\n'] : lines' s id	-- Unix
  lines' (c:s)          acc = lines' s (acc . (c:))


-- | The code for classify is largely stolen from Language.Preprocessor.Unlit.
classify ::  [String] -> [Lit]
classify []             = []
classify (x:xs) | "\\begin{code}"`isPrefixOf`x
                        = Lit x: allProg xs
   where allProg []     = []  -- Should give an error message,
                              -- but I have no good position information.
         allProg (x:xs) | "\\end{code}"`isPrefixOf`x
                        = Lit x: classify xs
         allProg (x:xs) = Code x: allProg xs
classify (('>':x):xs)   = Code ('>':x) : classify xs
classify (x:xs)         = Lit x: classify xs

-- | Join up chunks of code\/comment that are next to each other.
joinL :: [Lit] -> [Lit]
joinL []                  = []
joinL (Code c:Code c2:xs) = joinL (Code (c++c2):xs)
joinL (Lit c :Lit c2 :xs) = joinL (Lit  (c++c2):xs)
joinL (any:xs)            = any: joinL xs

