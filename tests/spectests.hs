{-# Language OverloadedStrings #-}

import Database.KyotoCabinet
import Data.ByteString.Char8 ()

import Prelude hiding (iterate)

test1 :: IO ()
test1 = do
  -- Create the DB
  db <- openPersistent Hash "/tmp/casket.kch" defaultLoggingOptions [] (Writer [Create] [])

  set db "foo" "hop"
  set db "bar" "step"
  set db "baz" "jump"

  get db "foo" >>= putStrLn . show
  
  -- This will be done with cursors
  let visitor = \k v -> putStr (show k) >> putStr ":" >> putStrLn (show v) >> return (Left NoOperation)
  iterate db visitor False

  close db

test2 :: IO ()
test2 = do
  db <- openPersistent Hash "/tmp/casket.kch" defaultLoggingOptions [] (Reader [])

  let vfull  = \k v -> putStr (show k) >> putStr ":" >> putStrLn (show v) >> return (Left NoOperation)
      vempty = \k   -> putStr (show k) >> putStrLn " is missing" >> return Nothing

  accept db "foo" vfull vempty False
  accept db "dummy" vfull vempty False
  
  iterate db vfull False

  close db

main :: IO ()
main = putStrLn "----------------------" >> test1 >> putStrLn "\n" >>
       putStrLn "----------------------" >> test2 >> putStrLn "\n" >>
       return ()
