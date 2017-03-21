import Data.Digest.Pure.MD5
import Data.ByteString.Lazy

main :: IO ()
main = print $ md5 "abc3231929"