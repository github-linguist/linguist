import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.ByteString (pack)
import Data.Char (ord)

main = do
  let message = "The quick brown fox jumped over the lazy dog's back"
      digest  = (md5sum . pack . map (fromIntegral . ord)) message
  putStrLn digest
