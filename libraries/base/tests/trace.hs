import Debug.Trace

main :: IO ()
main = do
  putStrLn $ trace "one" "two"
  putStrLn $ traceShow (3 :: Int) "four"
  putStrLn $ traceId "five"
  print $ traceShowId (6 :: Int)
  putStrLn $ traceWith (take 3) "seven"
  putStrLn $ traceShowWith length "eight"
  traceIO "nine"
  traceM "ten"
