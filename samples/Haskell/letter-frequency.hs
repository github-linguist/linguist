import Data.List (group,sort)
import Control.Arrow ((&&&))
main = interact (show . map (head &&& length) . group . sort)
